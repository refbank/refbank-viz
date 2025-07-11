library(here)
library(refbankr)
#get current dataset version from redivis
#If local version file == dataset version (don’t do anything)
#else (blank local version file, download & save (maybe?) – time how long this takes?, write local version file)
#Update workflow

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
if(cached_version==current_version){
  message("Cache up to date.")
}
if(cached_version!=current_version){
  message("Updating cache.")
  writeLines("", here(file_loc, "version.txt")) 
  get_datasets() |> write_csv(here(file_loc, "datasets.csv"))
  get_trials(include_condition_data = T)  |>
    write_csv(here(file_loc, "trials.csv"))
  get_messages(include_trial_data=T, include_condition_data=T) |>
    write_csv(here(file_loc, "messages.csv"))
  get_choices(include_trial_data = T, include_condition_data = T) |>
    write_csv(here(file_loc, "choices.csv"))
  #TODO add more when we need more
  get_cosine_similarities(sim_type="to_next") |> write_csv(here(file_loc, "to_next.csv"))
  get_cosine_similarities(sim_type="diverge") |> write_csv(here(file_loc, "diverge.csv"))
  get_cosine_similarities(sim_type="diff") |> write_csv(here(file_loc, "diff.csv"))
  get_cosine_similarities(sim_type="idiosyncrasy") |> write_csv(here(file_loc, "idiosyncrasy.csv"))
  writeLines(current_version, here(file_loc, "version.txt")) 
  message("Cache updated.")
}
}