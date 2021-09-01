# taster data
# functions to read data from Tastermonial databases


#' Read a raw Tastermonial CGM log (as downloaded from retool)
#' @param filepath path to a CSV download
taster_raw <- function( filepath = file.path(config::get("tastermonial")$datadir, "table-data.csv")) {

  readr::read_csv(filepath, col_types = cols(
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
}

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


psi_fill_taster_notes_from_scratch <- function(taster_notes_df) {

  conn_args <- config::get("dataconnection")
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  DBI::dbWriteTable(con, "notes_records", taster_notes_df, append = TRUE)
  message("wrote Taster Notes")
  tbl(con, "notes_records") %>% distinct(user_id)


}
