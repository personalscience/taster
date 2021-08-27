# add Tastermonial database to notes

taster_notes_file <- file.path(config::get("tastermonial")$datadir, "table-data.csv")

taster_raw <- readr::read_csv(taster_notes_file, col_types = cols(
  endEatingDate = col_character(),
  productFdcID = col_double(),
  barcode = col_double(),
  notes = col_character(),
  scannedTimeStamp = col_character(),
  afterTags = col_character(),
  isArchive = col_logical(),
  user = col_character(),
  netCarb = col_double(),
  carb = col_double(),
  servings = col_double(),
  fiber = col_double(),
  image = col_character(),
  beforeTags = col_character(),
  startEatingDate = col_character(),
  type = col_character(),
  name = col_character(),
  `_id` = col_character(),
  `__metadata` = col_character()
))

id_from_initial <- function(name_initials) {
  name_lookup_table <- user_df_from_libreview %>% transmute(name = paste(first_name,last_name), user_id )
  names <- name_lookup_table %>% dplyr::filter(str_detect(name,name_initials)) %>% pull(user_id)
  first_hit <- as.numeric(first(names))
  return(if(is.na(first_hit)) 0 else first_hit)

}

name_from_taster <- function(json_object) {
  parsed <- jsonlite::parse_json(json_object)[["name"]]
  if (is.null(parsed)) NA
  else return(stringr::str_trim(parsed, side = "both"))
}

id_from_taster <- function(json_object) {
  parsed <- jsonlite::parse_json(json_object)[["name"]]
  if (is.null(parsed)) 0
  else return(id_from_initial(stringr::str_sub(stringr::str_trim(parsed, side = "both"),1,3)))

  # map_dbl(taster_names %>% map_chr(function(x){ if(!is.null(x)) str_sub(x, 1,3)
  #   else NA}), id_from_initial)

}

taster_notes_df <- taster_raw %>% transmute(Start = with_tz(lubridate::parse_date_time(startEatingDate, orders = "dmY HM p z"),
                                                            tzone = Sys.timezone()),
                         End = as_datetime(NA),
                         Activity = "Food",
                         Comment = name,
                         Z = as.numeric(NA),
                         user_id = map_dbl(taster_raw$user, id_from_taster))
taster_notes_df

# map_dbl(taster_names %>% map_chr(function(x){ if(!is.null(x)) str_sub(x, 1,3)
#   else NA}), id_from_initial)
#
# same as map_chr(taster_raw$user, id_from_taster)
# unlist(sapply(taster_raw$user,
#               function(x) {id_from_taster(x)},
#               USE.NAMES = FALSE)))



