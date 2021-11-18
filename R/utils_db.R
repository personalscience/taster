# All functions that require database access
# Eventually this should be turned into its own package


#' @title make a database connection
#' @description hardcoded to look for Tastermonial db
db_connection <- function() {
  conn_args <- get_golem_config("dataconnection")

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  return(con)
}

#' @title Get a table from the database and return it as a well-formed dataframe.
#' @description As much as possible, route calls to the database table to here. This function will
#' abstract away problems with differences between databases.
#' @param con valid database connection
#' @param table_name string name for a table in the database (usual `glucose_records` or `notes_records`)
#' @description hardcoded to look for Tastermonial db
db_get_table <- function(con, table_name = "glucose_records") {

  if (table_name == "notes_records") {
    df <- tbl(con, "notes_records")  %>%
      collect() %>%
      filter(!is.na(Start)) %>%
      mutate(Start = lubridate::as_datetime(Start),
             End = lubridate::as_datetime(End))
  } else if (table_name == "glucose_records") {
  df <- tbl(con,"glucose_records") %>%
    collect() %>%
    filter(!is.na(time)) %>%
    mutate(time = lubridate::as_datetime(time))
  } else df <- tbl(con, table_name) %>% collect()

  return(df)
}


#' @title Write a table to the database
#' @param con valid database connection
#' @param table_name char string for table name
#' @param table_df valid dataframe to write to the database
#' @return character string with a message that can be displayed to the user
db_write_table <- function(con = db_connection(), table_name = "raw_glucose", table_df) {

  msg <- "Nothing to write"
  if (DBI::dbExistsTable(con, table_name)) {
    # check that you're not adding another copy of the same table
    sn <- first(unique(table_df$serial_number))
    if(nrow(tbl(con, table_name) %>% filter(.data[["serial_number"]] == sn) %>% collect()) > 0){
      message(sprintf("Already have that serial number %s", sn))
      msg <- sprintf("Already have that serial number %s", sn)
      return(msg)
    } else {
      message(sprintf("writing %d rows to table %s",nrow(table_df), table_name))
      msg <- sprintf("wrote %d records to %s",nrow(table_df), table_name)
      DBI::dbAppendTable(con, table_name, table_df)
    }

    } else {


  DBI::dbWriteTable(con, table_name, table_df)
      msg <- "Wrote to table for the first time"
    }

  return(msg)

}


#' @title List all products consumed by `user_id`
#' @description Return all products consumed by this user.
#' @param user_id vector of user IDs or NULL to show all users
#' @return character vector of product names sorted alphabetically
#' @importFrom dplyr tbl collect filter distinct group_by transmute pull add_count arrange
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
db_food_list<- function(user_id = 1234  ) {

  con <- db_connection()

  ID <- user_id

  if (is.null(ID)) {
    prods <- db_get_table(con, "notes_records") %>% filter(.data[["Activity"]] == "Food") %>%
      filter(.data[["Start"]] > "2021-06-01") %>%
      group_by(.data[["Comment"]]) %>% add_count() %>% filter(n > 2) %>% distinct(.data[["Comment"]]) %>%
      transmute(productName = .data[["Comment"]], `user_id` = ID) %>%
      arrange(.data[['productName']])
  } else
    prods <-
    db_get_table(con, "notes_records") %>% filter(.data[["Activity"]] == "Food") %>%
    filter(.data[["Start"]] > "2021-06-01") %>%
    filter(`user_id` %in% ID) %>% distinct(.data[["Comment"]]) %>%
    pull(.data[["Comment"]])

  if(length(prods) > 0)
    return(sort(prods))
  else return(NULL)

  DBI::dbDisconnect(con)
  return(prods)
}

# psi User Management Functions

#' @title Returns privileges assigned to this user
#' @param user_id user ID
#' @return privilege level, NULL if no privileges available
#' @export
db_user_privileges <- function(user_id = -1){
  con <- db_connection()

  ID = user_id
  result <- NULL
  if(DBI::dbExistsTable(con, "accounts_user")){
    priv <- tbl(con, "accounts_user") %>%
      filter(user_id == ID) %>%
      pull(.data[["privilege"]]) %>%
      first()
    result <- if(!is.na(priv)) priv else NULL

  } else {
    message(sprintf("Database %s doesn't have an accounts_user table"))

  }

  DBI::dbDisconnect(con)
  return(result)
}

#' @title All user records in the database
#' @param conn_args database connection
#' @return dataframe of all user records
#' @importFrom dplyr tbl collect
#' @export
db_user_df <- function(conn_args = config::get("dataconnection")){
  con <- db_connection()

  users_df <- db_get_table(con, "user_list")

  DBI::dbDisconnect(con)
  return(users_df)

}

#' @title Character string for user_id
#' @param con database connection
#' @param firebase_obj firebase object
#' @param user_id numerical `user_id`
#' @return character string name for user
#' @importFrom dplyr tbl collect
#' @export
db_name_for_user_id <- function(con, firebase_obj, user_id) {
  ID = user_id

    return(db_user_df() %>% dplyr::filter(user_id == ID)  %>%
             select(first_name,last_name) %>%
             as.character() %>%
             stringr::str_flatten(collapse = " "))
}



#' @title Set up Firebase support
#' @description This is just a stub for now, in order to consolidate all program-wide calls to Firebase.
#' @param con database connection
#' @import firebase
#' @return firebase object
firebase_setup <- function(con) {
  f <- firebase::FirebaseUI$
  new("local")$ # instantiate
  set_providers( # define providers
    email = TRUE,
    google = TRUE
  )$
  set_tos_url("https://ensembio.com/privacy")$
  set_privacy_policy_url("https://ensembio.com/privacy")$
  launch() # launch

  return(f)
}
