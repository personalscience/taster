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

#' @description Tastermonial users are kept in a JSON object with $email $name and $uid fields.
#' Unfortunately, the names are often not consistent with what we find in Libreview (e.g. Bude is Buda).
#' so this function attempts to match people based on the first name in the libreview list that matches initials.
#'
id_from_initial <- function(name_initials) {
  name_lookup_table <- user_df_from_libreview %>% transmute(name = paste(first_name,last_name), user_id )
  names <- name_lookup_table %>% dplyr::filter(str_detect(name,name_initials)) %>% pull(user_id)
  first_hit <- as.numeric(first(names))
  return(if(is.na(first_hit)) 0 else first_hit)

}


taster_usernames <- function(taster_raw_data) {
  map(taster_raw_data, function(x) {unlist(jsonlite::fromJSON(x)[["name"]])}) %>% unlist() %>% str_trim() %>% unique()
}
#taster_usernames(taster_raw()$user)

taster_emails <- function(taster_raw_data) {
  map(taster_raw_data, function(x) {unlist(jsonlite::fromJSON(x)[["email"]])}) %>% unlist() %>% str_trim() %>% unique()
}
#taster_emails(taster_raw()$user)

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

### How I made the conversion file
# t <- taster_raw_all %>% transmute(name,productFdcID=as.character(productFdcID),type,barcode,notes)
#
# pids <- t %>% drop_na(productFdcID) %>% distinct(pid =productFdcID) %>% pull(pid)
# up <- t %>% filter(productFdcID %in% pids) %>% distinct(name,productFdcID)
# up %>% group_by(pid = productFdcID) %>% summarize(n=n(), names = paste0(name)) %>% mutate(pid = paste0("\'",pid)) %>%
#   clipr::write_clip(object_type = c("table"))
#
# name_convert_file <- read_csv(file=file.path(config::get("tastermonial")$datadir, "Tastermonial Name Mapping.csv"), col_types = "cdcc") %>%
#   transmute(pid = str_replace_all(pid, "\'",""),names,simpleName)

#' a CSV file with columns `pid`, `names`, and `simpleName` to convert from each format
taster_names_convert_table <- read_csv(file=file.path(config::get("tastermonial")$datadir,
                                                      "Tastermonial Name Mapping.csv"), col_types = "cdcc") %>%
  transmute(pid = str_replace_all(pid, "\'",""),names,simpleName)

#' Classify a Tastermonial food into limited categories
#' @param foodname a string representation of a name
#' @return character string representing simplifed name
taster_classify_food <- function(foodname) {

  if(!is.null(foodname)){
  s <- taster_names_convert_table %>% filter(names %in% foodname) #%>% pull(simpleName)
  if(nrow(s)>0) return(s %>% pull(simpleName))
  else return(foodname)}
  else return(NA)

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
