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
#' @export
db_get_table <- function(con, table_name = "glucose_records") {

  if (!DBI::dbExistsTable(con, table_name)) {
    return(NULL)
  }
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
#' @description Return all products consumed by this user. If `user_id==NULL` then show
#'  from all users, all products consumed more than once.
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
      pull(.data[["Comment"]])
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
    message(sprintf("Database %s doesn't have an accounts_user table", class(con)))

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

#' @title Find User From Firebase ID
#' @description A signed-in user, by definition, has a Firebase ID.  Because the ID was
#' registered as a unique `user_id` when the account was created, we can look it up in
#' the user table and return its user (and username)
#' @param firebase_id valid firebase ID for a signed in user
#' @return numeric user_id; NA if no user exists or if the table is not in the database
#' @export
db_user_id_from_firebase <- function(firebase_id) {
  f_id <- firebase_id
  con <- db_connection()
  if (!DBI::dbExistsTable(con, "accounts_firebase")) {
    cat(file=stderr(), sprintf("table `accounts_firebase` does not exist in database %s", class(con)))
    DBI::dbDisconnect(con)
    return(NA)
  }
    f_match <- tbl(con, "accounts_firebase") %>% filter(f_id == firebase_id) %>% collect()
    if(nrow(f_match)>0)
      user_id <- first(f_match[["user_id"]])
    else user_id <- NA

    DBI::dbDisconnect(con)
    return(user_id)
}

#' @title Users Visible at Privilege
#' @param user_id user ID
#' @return vector of user lists in the form (`user_id`, `username`)
#' @export
db_users_visible <- function(user_id = -1 ) {

  ID = if(is.numeric(user_id)) user_id else 0
  privilege <- "user"

  visible_user_ids <- c(1234,ID)  # you can always see your own ID

  con <- db_connection()

  db_get_table(con, "accounts_user")

  privilege <- if (DBI::dbExistsTable(con, "accounts_user")){
    priv_rows <- tbl(con, "accounts_user") %>% filter(user_id == ID)  %>% collect()
    if(nrow(priv_rows) > 0) first(priv_rows[["privilege"]]) else "nobody"
  } else "user"

  if (privilege == "admin")
    visible_user_ids <- db_user_df() %>% pull(user_id)


  DBI::dbDisconnect(con)
  return(visible_user_ids)
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



