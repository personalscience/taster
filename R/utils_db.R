
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
