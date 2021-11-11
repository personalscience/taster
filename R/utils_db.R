
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

#' @title Write a table to the database
#' @param con valid database connection
#' @param table_name char string for table name
#' @param table_df valid dataframe to write to the database
db_write_table <- function(con = db_connection(), table_name = "raw_glucose", table_df) {
  if (DBI::dbExistsTable(con, table_name)) {
    # check that you're not adding another copy of the same table
    sn <- first(unique(table_df$serial_number))
    if(nrow(tbl(con, "raw_glucose") %>% filter(.data[["serial_number"]] == sn) %>% collect()) > 0){
      message(sprintf("Already have that serial number %s", sn))
      return(NULL)
    } else {
      message(sprintf("writing %d rows to table %s",nrow(table_df), table_name))
      DBI::dbAppendTable(con, table_name, table_df)
    }

    } else {


  DBI::dbWriteTable(con, table_name, table_df)
    }

}


#' @title List all products consumed by `user_id`
#' @description Return all products consumed by this user.
#' @param user_id vector of user IDs or NULL to show all users
#' @return character vector of product names sorted alphabetically
#' @importFrom dplyr tbl collect filter distinct group_by transmute pull add_count arrange
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
food_list_db <- function(user_id = 1234  ) {

  con <- db_connection()

  ID <- user_id

  if (is.null(ID)) {
    prods <- tbl(con, "notes_records") %>% filter(.data[["Activity"]] == "Food") %>%
      filter(.data[["Start"]] > "2021-06-01") %>%
      group_by(.data[["Comment"]]) %>% add_count() %>% filter(n > 2) %>% distinct(.data[["Comment"]]) %>%
      transmute(productName = .data[["Comment"]], `user_id` = ID) %>%
      collect() %>% arrange(.data[['productName']])
  } else
    prods <-
    tbl(con, "notes_records") %>% filter(.data[["Activity"]] == "Food") %>%
    filter(.data[["Start"]] > "2021-06-01") %>% filter(`user_id` %in% ID) %>% distinct(.data[["Comment"]]) %>%
    collect() %>% pull(.data[["Comment"]])

  if(length(prods) > 0)
    return(sort(prods))
  else return(NULL)

  DBI::dbDisconnect(con)
  return(prods)
}

# psi User Management Functions

#' @title All user records in the database
#' @param conn_args database connection
#' @return dataframe of all user records
#' @importFrom dplyr tbl collect
#' @export
user_df_from_db <- function(conn_args = config::get("dataconnection")){
  con <- db_connection()

  users_df <- tbl(con, "user_list" ) %>% collect()

  DBI::dbDisconnect(con)
  return(users_df)

}

#' @title Character string for user_id
#' @param con database connection
#' @param firebase_obj firebase object
#' @param user_id numerical `user_id`
#' @return dataframe of all user records
#' @importFrom dplyr tbl collect
#' @export
name_for_user_id <- function(con, firebase_obj, user_id) {
  ID = user_id

  if (ID == 0) return("Unknown Name")
  else   if(!is.null(firebase_obj)){
    user <- firebase_obj$get_signed_in()
    message(sprintf("user = %s", user$response$email))
    return(user_df_from_db() %>% dplyr::filter(user_id == ID)  %>%
             select(first_name,last_name) %>%
             as.character() %>%
             stringr::str_flatten(collapse = " "))
  } else {
    return(user_id)
}


}

