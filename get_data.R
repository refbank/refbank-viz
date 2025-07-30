library(here)
library(refbankr)
library(redivis)
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
  con <- redivis$user("mcfrank")$dataset("refbank:2zy7")
  con$table("per_game_summary:bsw0")$to_tibble() |> write_csv(here(file_loc, "per_game_summary.csv"))
  writeLines(current_version, here(file_loc, "version.txt")) 
  message("Cache updated.")
}
}