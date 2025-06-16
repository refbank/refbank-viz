# SBERT Processing
library(tidyverse)
library(here)
library(stringi)

source(here("data_helpers.R"))



### helper funcs
get_sim_matrix <- function(df, F_mat, method = "cosine") {
  feats <- F_mat[df$feature_ind, ]
  if (method == "cor") {
    return(cor(t(feats), method = "pearson"))
  } else if (method == "euclidean") {
    return(as.matrix(dist(feats, method = "euclidean")))
  } else if (method == "cosine") {
    return(as.matrix(lsa::cosine(t(feats))))
  } else {
    stop(paste0("unknown method", method))
  }
}

# note this does de-duplicated version
flatten_sim_matrix <- function(cormat, ids) {
  ut <- upper.tri(cormat)
  data.frame(
    dim1 = ids[row(cormat)[ut]],
    dim2 = ids[col(cormat)[ut]],
    sim  = as.numeric(cormat[ut])
  ) |>
    mutate(
      dim1 = as.character(dim1),
      dim2 = as.character(dim2)
    )
}

make_within_df <- function(M_mat, F_mat, method) {
  M_mat %>%
    do(flatten_sim_matrix(
      get_sim_matrix(., F_mat, method = method),
      .$repNum
    )) %>%
    mutate(
      rep1 = as.numeric(dim1),
      rep2 = as.numeric(dim2)
    )
}

make_across_df <- function(M_mat, F_mat, method) {
  M_mat %>%
    do(flatten_sim_matrix(
      get_sim_matrix(., F_mat, method = method),
      as.character(.$combinedId)
    ))
}

### funcs
do_diverge <- function(concat) {
  check_size <- concat |> group_by(
    target, stage_num, rep_num, condition_id,
    paper_id, group_size, structure, language, option_size
  ) |> tally() |> 
    filter(n>1) |> select(-n) |> ungroup()
  
  useable <- concat |> inner_join(check_size)
  F_mat <- useable |>
    select(starts_with("dim")) |>
    as.matrix()
  M_mat <- useable |>
    select(-starts_with("dim")) |>
    mutate(feature_ind = row_number())

  game_divergence <- M_mat |>
    group_by(
      target, stage_num, rep_num, condition_id,
      paper_id, group_size, structure, language, option_size
    ) |>
    mutate(combinedId = game_id) |>
    make_across_df(F_mat, "cosine") |>
    rename(game_id_1 = dim1) |>
    rename(game_id_2 = dim2) |>
    filter(game_id_1 != game_id_2) |>
    mutate(sim = ifelse(is.nan(sim), NA, sim)) |>
    ungroup()

  return(game_divergence)
}

do_converge <- function(concat) {
  check_size <- concat |>  group_by(
    game_id, target, stage_num, condition_id,
    paper_id, group_size, structure, language, option_size
  ) |> tally() |> 
    filter(n>1) |> select(-n) |> ungroup()
  
  useable <- concat |> inner_join(check_size)
  F_mat <- useable |>
    select(starts_with("dim")) |>
    as.matrix() # Features
  M_mat <- useable |>
    select(-starts_with("dim")) |>
    mutate(feature_ind = row_number())

  tangram_change <- M_mat |>
    group_by(
      game_id, target, stage_num, condition_id,
      paper_id, group_size, structure, language, option_size
    ) |>
    mutate(combinedId = str_c(rep_num, describer, sep = ";")) |>
    make_across_df(F_mat, "cosine") |>
    separate(dim1, into = c("rep_num_1", "describer_1"), convert = TRUE, sep = ";") |>
    separate(dim2, into = c("rep_num_2", "describer_2"), convert = T, sep = ";") |>
    mutate(sim = ifelse(is.nan(sim), NA, sim)) |>
    mutate(
      later = ifelse(rep_num_1 > rep_num_2, rep_num_1, rep_num_2),
      earlier = ifelse(rep_num_1 > rep_num_2, rep_num_2, rep_num_1),
      same_describer = ifelse(describer_1 == describer_2, 1, 0)
    )

  return(tangram_change)
}

do_diff_tangrams <- function(concat) {
  check_size <- concat |>  group_by(
    game_id, stage_num, rep_num, condition_id,
    paper_id, group_size, structure, language, option_size
  ) |> tally() |> 
    filter(n > 1) |> select(-n) |> ungroup()
  
  useable <- concat |> inner_join(check_size)
  F_mat <- useable |>
    select(starts_with("dim")) |>
    as.matrix() # Features
  M_mat <- useable |>
    select(-starts_with("dim")) |>
    mutate(feature_ind = row_number())

  tangram_distinctive <- M_mat |>
    group_by(
      game_id, stage_num, rep_num, condition_id,
      paper_id, group_size, structure, language, option_size
    ) |>
    mutate(combinedId = target) |>
    make_across_df(F_mat, "cosine") |>
    rename(target_1 = dim1, target_2 = dim2) |>
    mutate(sim = ifelse(is.nan(sim), NA, sim)) |>
    filter(target_1 != target_2) |>
    ungroup()

  return(tangram_distinctive)
}

do_idiosyncrasy <- function(concat) {
  check_size <- concat |>  group_by(
    game_id, target, stage_num, rep_num, condition_id,
    paper_id, group_size, structure, language, option_size
  ) |> tally() |> 
    filter(n > 1) |> select(-n) |> ungroup()
  
  useable <- concat |> inner_join(check_size)
  F_mat <- useable |>
    select(starts_with("dim")) |>
    as.matrix() # Features
  M_mat <- useable |>
    select(-starts_with("dim")) |>
    mutate(feature_ind = row_number())

  idiosyncrasy <- M_mat |>
    group_by(
      game_id, target, stage_num, rep_num, condition_id,
      paper_id, group_size, structure, language, option_size
    ) |>
    mutate(combinedId = class) |>
    make_across_df(F_mat, "cosine") |>
    mutate(sim = ifelse(is.nan(sim), NA, sim)) |>
    select(-dim1, -dim2) |> 
    ungroup()

  return(idiosyncrasy)
}

# run fxns

DATA_LOC <- here("harmonized_data")


do_dataset_sims <- function(dataset) {
  
  message(dataset)
  trials <- get_trials_full(DATA_LOC, dataset) |>
    mutate(option_size = option_set |>
             str_split(";") |>
             lengths()) |>
    filter(option_size != 1)
  embeds <- get_tbl(DATA_LOC, dataset, "embeddings") |>
    left_join(trials) |>
    select(
      game_id, target, stage_num, rep_num, condition_id,
      describer, paper_id, group_size, structure, language, option_size, starts_with("dim")
    )

  # dir.create(file.path(here("sim_cache"), dataset), showWarnings = FALSE)
  # 
  # message("start converge")
  # converge <- do_converge(embeds)
  # message("end converge")
  # to_next <- converge |>
  #   filter(earlier + 1 == later) |>
  #   write_csv(here("sim_cache", dataset, "to_next.csv"))
  # to_last <- converge |>
  #   group_by(paper_id, game_id, stage_num) |>
  #   mutate(max_trial = max(later)) |>
  #   filter(later == max_trial) |>
  #   write_csv(here("sim_cache", dataset, "to_last.csv"))
  # to_first <- converge |>
  #   group_by(paper_id, game_id, stage_num) |>
  #   mutate(min_trial = min(later)) |>
  #   filter(earlier == min_trial) |>
  #   write_csv(here("sim_cache", dataset, "to_first.csv"))
  # 
  # message("start diverge")
  # diverge <- do_diverge(embeds) |>
  #   write_csv(here("sim_cache", dataset, "diverge.csv"))
  # 
  # message("start tangram diff")
  # tangram_distinct <- do_diff_tangrams(embeds) |>
  #   write_csv(here("sim_cache", dataset, "target_diff.csv"))
  
  message("start idiosyncrasy")
  prior <- embeds |> 
    filter(rep_num == 1) |> 
    group_by(target) |> 
    summarise(across(starts_with("dim"), \(d) mean(d, na.rm = TRUE))) |> 
    mutate(class = "prior")
  idiosyncrasy <- embeds |> 
    mutate(class = "embed") |> 
    bind_rows(
      embeds |> 
        select(-starts_with("dim")) |> 
        left_join(prior, by = join_by(target))
    ) |> 
    do_idiosyncrasy() |>
    write_csv(here("sim_cache", dataset, "idiosyncrasy.csv"))
}


all_dirs <- list.dirs(DATA_LOC, full.names = FALSE) |>
  stri_remove_empty() 

walk(all_dirs, do_dataset_sims)

