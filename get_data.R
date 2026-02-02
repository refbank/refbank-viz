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

  hedge_phrases <- c(
    "i think", "i guess", "sort of", "kind of", "a little", "not sure",
    "looks like", "i feel like", "i suppose"
  )
  hedge_words <- c(
    "maybe", "perhaps", "probably", "possibly", "seems",
    "somewhat", "might", "could", "likely", "appears"
  )

  msgs <- messages_df |>
    filter(role == "describer") |>
    mutate(text_l = str_to_lower(text)) |>
    mutate(
      word_count = str_count(text_l, "\\b\\w+\\b"),
      hedge_count =
        # count phrases
        rowSums(sapply(hedge_phrases, \(p) str_count(text_l, fixed(p)))) +
        # count single words with word boundaries
        rowSums(sapply(hedge_words, \(w) str_count(text_l, regex(paste0("\\b", w, "\\b")))))
    ) |>
    filter(word_count > 0) |>
    select(trial_id, hedge_count, word_count)

  pos <- msgs |>
    inner_join(trials_df |> select(trial_id, game_id, rep_num, stage_num), by = "trial_id") |>
    group_by(game_id, rep_num, stage_num) |>
    summarise(
      hedge_count = sum(hedge_count, na.rm = TRUE),
      word_count  = sum(word_count,  na.rm = TRUE),
      hedge_rate  = hedge_count / word_count,
      .groups = "drop"
    ) |>
    select(game_id, rep_num, stage_num, hedge_rate)

  pos
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
    message("WARNING: Could not find messages/trials tables; skipping PoS cache.")
  } else {
    messages_df <- con$table(messages_name)$to_tibble(variables = c("trial_id", "role", "text"))
    trials_df   <- con$table(trials_name)$to_tibble(variables = c("trial_id", "game_id", "rep_num", "stage_num"))

    pos_df <- compute_hedge_pos_summary(messages_df, trials_df)
    pos_df |> write_csv(pos_path)

    message("PoS cache updated.")
  }
}

writeLines(current_version, here(file_loc, "version.txt"))
message("Cache updated.")
}