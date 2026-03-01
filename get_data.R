library(here)
library(refbankr)
library(redivis)
#get current dataset version from redivis
#If local version file == dataset version (don’t do anything)
#else (blank local version file, download & save (maybe?) – time how long this takes?, write local version file)
#Update workflow

compute_hedge_pos_summary <- function(messages_df, trials_df) {
  library(dplyr)
  library(stringr)

  # ---------- HEDGES ----------
  hedge_phrases <- c(
    "i think", "i guess", "sort of", "kind of", "a little", "not sure",
    "looks like", "i feel like", "i suppose"
  )
  hedge_words <- c(
    "maybe", "perhaps", "probably", "possibly", "seems",
    "somewhat", "might", "could", "likely", "appears"
  )

  # ---------- NEGATION ----------
  neg_pattern <- "\\b(not|never|no|none|nothing|nowhere|neither|nor|cannot)\\b|n't\\b"

  # ---------- QUESTIONS ----------
  wh_pattern_start <- "^\\s*(what|where|who|when|why|how|which|whose)\\b"

  # ---------- PREPOSITIONS ----------
  prep_words <- c(
    "about","above","across","after","against","along","among","around","at",
    "before","behind","below","beneath","beside","between","beyond","but","by",
    "concerning","despite","down","during","except","for","from","in","inside","into",
    "like","near","of","off","on","onto","out","outside","over","past","regarding",
    "since","through","throughout","till","to","toward","towards","under","underneath",
    "until","up","upon","with","within","without"
  )
  prep_pattern <- paste0("\\b(", paste(prep_words, collapse="|"), ")\\b")

  msgs <- messages_df |>
    filter(role == "describer") |>
    mutate(text_l = str_to_lower(text)) |>
    mutate(
      word_count = str_count(text_l, "\\b\\w+\\b"),

      hedge_count =
        rowSums(sapply(hedge_phrases, \(p) str_count(text_l, fixed(p)))) +
        rowSums(sapply(hedge_words, \(w) str_count(text_l, regex(paste0("\\b", w, "\\b"))))),

      negation_count = str_count(text_l, regex(neg_pattern)),

      is_question = ifelse(
        str_detect(text_l, fixed("?")) | str_detect(text_l, regex(wh_pattern_start)),
        1L, 0L
      ),

      prep_count = str_count(text_l, regex(prep_pattern))
    ) |>
    filter(word_count > 0) |>
    select(trial_id, hedge_count, negation_count, is_question, prep_count, word_count)

  pos <- msgs |>
    inner_join(trials_df |> select(trial_id, game_id, rep_num, stage_num), by = "trial_id") |>
    group_by(game_id, rep_num, stage_num) |>
    summarise(
      hedge_count    = sum(hedge_count, na.rm = TRUE),
      negation_count = sum(negation_count, na.rm = TRUE),
      question_count = sum(is_question, na.rm = TRUE),
      prep_count     = sum(prep_count, na.rm = TRUE),
      word_count     = sum(word_count, na.rm = TRUE),

      hedge_rate     = hedge_count / word_count,
      negation_rate  = negation_count / word_count,
      question_rate  = question_count / word_count,
      prep_rate      = prep_count / word_count,
      .groups = "drop"
    ) |>
    select(game_id, rep_num, stage_num, hedge_rate, negation_rate, question_rate, prep_rate)

  pos
}

compute_udpipe_pos_rates <- function(messages_df, trials_df, model_path) {
  # messages_df must have: trial_id, role, text
  # trials_df must have: trial_id, game_id, rep_num, stage_num

  msgs <- messages_df |>
    filter(role == "describer") |>
    mutate(text = ifelse(is.na(text), "", text)) |>
    select(trial_id, text)

  if (nrow(msgs) == 0) {
    return(tibble(game_id=character(), rep_num=integer(), stage_num=integer()))
  }

  ud_model <- udpipe::udpipe_load_model(model_path)

  # Annotate with doc_id = trial_id so we can join back
  ann <- udpipe::udpipe_annotate(
    ud_model,
    x = msgs$text,
    doc_id = as.character(msgs$trial_id)
  )
  ann <- as.data.frame(ann)

  # Keep only real tokens (drop punctuation/symbols)
  ann2 <- ann |>
    filter(!upos %in% c("PUNCT", "SYM")) |>
    mutate(doc_id = as.character(doc_id))

  # Total tokens per message
  totals <- ann2 |>
    count(doc_id, name = "total_tokens")

  # Count target POS per message (udpipe UPOS tagset)
  wanted <- c("NOUN","VERB","ADJ","ADV",
            "PRON","ADP","DET","AUX",
            "CCONJ","SCONJ")

  pos_counts <- ann2 |>
    filter(upos %in% wanted) |>
    count(doc_id, upos, name = "n") |>
    tidyr::pivot_wider(
      names_from = upos,
      values_from = n,
      values_fill = 0
    )

  # Ensure all wanted columns exist even if absent
  for (tag in wanted) {
    if (!tag %in% names(pos_counts)) pos_counts[[tag]] <- 0L
  }

  per_msg <- totals |>
    left_join(pos_counts, by = "doc_id") |>
    mutate(across(all_of(wanted), ~ tidyr::replace_na(.x, 0L))) |>
    mutate(
      noun_rate = NOUN / total_tokens,
      verb_rate = VERB / total_tokens,
      adj_rate  = ADJ  / total_tokens,
      adv_rate  = ADV  / total_tokens,

      pron_rate = PRON / total_tokens,
      adp_rate  = ADP  / total_tokens,
      det_rate  = DET  / total_tokens,
      aux_rate  = AUX  / total_tokens,
      cconj_rate = CCONJ / total_tokens,
      sconj_rate = SCONJ / total_tokens,

      content_tokens = NOUN + VERB + ADJ + ADV,
      function_tokens = PRON + ADP + DET + AUX + CCONJ + SCONJ,

      content_rate = content_tokens / total_tokens,
      function_rate = function_tokens / total_tokens,

      content_function_ratio = ifelse(
        function_tokens > 0,
        content_tokens / function_tokens,
        NA_real_
      )
    ) |>
    select(
      doc_id, total_tokens,
      noun_rate, verb_rate, adj_rate, adv_rate,
      pron_rate, adp_rate, det_rate, aux_rate,
      cconj_rate, sconj_rate,
      content_rate, function_rate, content_function_ratio
    ) |>
    rename(trial_id = doc_id) |>
    mutate(trial_id = as.numeric(trial_id))

  # Aggregate to (game_id, rep_num, stage_num)
  out <- per_msg |>
    inner_join(trials_df |> select(trial_id, game_id, rep_num, stage_num), by = "trial_id") |>
    group_by(game_id, rep_num, stage_num) |>
    summarise(
      noun_rate = weighted.mean(noun_rate, w = total_tokens, na.rm = TRUE),
      verb_rate = weighted.mean(verb_rate, w = total_tokens, na.rm = TRUE),
      adj_rate  = weighted.mean(adj_rate,  w = total_tokens, na.rm = TRUE),
      adv_rate  = weighted.mean(adv_rate,  w = total_tokens, na.rm = TRUE),

      pron_rate = weighted.mean(pron_rate, w = total_tokens, na.rm = TRUE),
      adp_rate  = weighted.mean(adp_rate,  w = total_tokens, na.rm = TRUE),
      det_rate  = weighted.mean(det_rate,  w = total_tokens, na.rm = TRUE),
      aux_rate  = weighted.mean(aux_rate,  w = total_tokens, na.rm = TRUE),
      cconj_rate = weighted.mean(cconj_rate, w = total_tokens, na.rm = TRUE),
      sconj_rate = weighted.mean(sconj_rate, w = total_tokens, na.rm = TRUE),

      content_rate = weighted.mean(content_rate, w = total_tokens, na.rm = TRUE),
      function_rate = weighted.mean(function_rate, w = total_tokens, na.rm = TRUE),
      content_function_ratio = weighted.mean(content_function_ratio, w = total_tokens, na.rm = TRUE),

      .groups = "drop"
    )

  out
}

check_cache <- function(file_loc){
  if (!file.exists(here(file_loc))){
    dir.create(file.path(here(), file_loc))
  }

  get_cached_version <- function(){
    if (file.exists(here(file_loc, "version.txt"))){
      version <- readLines(here(file_loc, "version.txt"))
    }
    else {
      file.create(here(file_loc, "version.txt"))
      version <- " "
    }
    if (length(version)==0){
      version <- " "
    }
    version
  }

  cached_version <- get_cached_version()
  #message(cached_version)
  current_version <- refbankr::get_current_version()
  #message(current_version)

  pos_path <- here(file_loc, "per_game_pos_summary.csv")
  summary_path <- here(file_loc, "per_game_summary.csv")

  needs_update <- (cached_version != current_version) || !file.exists(summary_path)
  needs_pos <- needs_update || !file.exists(pos_path)

  if (!needs_update && !needs_pos) {
    message("Cache up to date.")
    return(invisible(NULL))
  }

  message("Updating cache.")
  writeLines("", here(file_loc, "version.txt"))

  con <- redivis$user("mcfrank")$dataset("refbank:2zy7")

  # Always ensure per_game_summary exists if needed
  if (needs_update) {
    con$table("per_game_summary:bsw0")$to_tibble() |>
      write_csv(summary_path)
  }

  if (needs_pos) {
    tables <- con$list_tables()
    table_names <- sapply(tables, \(t) t$name)

    messages_name <- table_names[grepl("^messages", table_names)][1]
    trials_name   <- table_names[grepl("^trials", table_names)][1]

    if (is.na(messages_name) || is.na(trials_name)) {
      message("WARNING: Could not find messages/trials tables; skipping PoS cache.") # nolint
    } else {
      messages_df <- con$table(messages_name)$to_tibble(variables = c("trial_id", "role", "text"))
      trials_df   <- con$table(trials_name)$to_tibble(variables = c("trial_id", "game_id", "rep_num", "stage_num"))

      # 1) existing lexical PoS-ish metrics (hedge/neg/question/prep/etc)
      pos_df <- compute_hedge_pos_summary(messages_df, trials_df)

      # 2) udpipe model (download once into cached_data/)
      ud_dir <- here(file_loc, "udpipe_model")
      if (!dir.exists(ud_dir)) dir.create(ud_dir, recursive = TRUE)

      model_file <- file.path(ud_dir, "english-ewt-ud-2.5-191206.udpipe")
      if (!file.exists(model_file)) {
        m <- udpipe::udpipe_download_model(language = "english-ewt")
        file.copy(m$file_model, model_file, overwrite = TRUE)
      }

      # 3) compute udpipe POS rates and join onto pos_df
      pos_ud <- compute_udpipe_pos_rates(messages_df, trials_df, model_file)

      pos_all <- pos_df |>
        left_join(pos_ud, by = c("game_id", "rep_num", "stage_num"))

      pos_all |> write_csv(pos_path)

      message("PoS cache updated (including udpipe POS rates).")
    }
  }

  writeLines(current_version, here(file_loc, "version.txt"))
  message("Cache updated.")
}