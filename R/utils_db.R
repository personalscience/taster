# All functions that require database access
# Eventually this should be turned into its own package


#' @title make a database connection
#' @description hardcoded to look for Tastermonial db
db_connection <- function() {
  conn_args <- get_golem_config("dataconnection")

  con <- tryCatch(
    error = function(cnd) {
      stop(sprintf("Database host %s is invalid",
                   {if(!is.null(conn_args$host)) conn_args$host else "unknown"}
                   )
           )
      },
    DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  ))

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
  if (DBI::dbExistsTable(con, table_name) & table_name == "raw_glucose") {
    # check that you're not adding another copy of the same table
    sn <- first(unique(table_df$serial_number))
    if(nrow(tbl(con, table_name) %>% filter(.data[["serial_number"]] == sn) %>% collect()) > 0){
      message(sprintf("Already have that serial number %s", sn))
      msg <- sprintf("Already have that serial number %s", sn)
      return(msg)
    } else {  # writes to raw_glucose table because the serial number is new
      message(sprintf("writing %d rows to table %s",nrow(table_df), table_name))
      msg <- sprintf("wrote %d records to %s",nrow(table_df), table_name)
      DBI::dbAppendTable(con, table_name, table_df)
    }

  } else
    if (DBI::dbExistsTable(con, table_name) & table_name == "accounts_firebase") {
      if(nrow(table_df)>1) { return("Error in accounts_firebase: rows > 1")}
      # find the max id already in this table
      max_id <- tbl(con, table_name) %>% pull(id) %>% max()
      # don't write if the firebase_id already exists
      new_table_df_fid <- table_df %>% pull("firebase_id") %>% first()
      existing_df_fids <- tbl(con, table_name) %>% pull("firebase_id")
      if (new_table_df_fid %in% existing_df_fids){
        return("Firebase id already exists")
      }
      new_records <- bind_cols(id=seq(to = nrow(table_df) + max_id, from = max_id + 1), table_df)
      result <- DBI::dbAppendTable(con, table_name, new_records)
      msg <- sprintf("Wrote to table with result = %s",result)
    }  else # something other than raw_glucose or accounts_firebase
      if (DBI::dbExistsTable(con, table_name))  {
        # find the max id already in this table
        max_id <- tbl(con, table_name) %>% pull(id) %>% max()
        new_records <- bind_cols(id=seq(to = nrow(table_df) + max_id, from = max_id + 1), table_df)
        result <- DBI::dbAppendTable(con, table_name, new_records)
        msg <- sprintf("Wrote to table with result = %s",result)
      } else {
        DBI::dbWriteTable(con, table_name, table_df)
        msg <- "Wrote to table for the first time"
      }

  return(msg)

}

#' @title Write a notes table to the database
#' @param con valid database connection
#' @param table_name char string for table name
#' @param table_df valid dataframe to write to the database
#' @param user_id User ID
#' @return character string with a message that can be displayed to the user
db_write_notes_table <- function(con = db_connection(),
                                 table_name = "raw_notes",
                                 table_df,
                                 user_id) {
  msg <- "NA"
  if (DBI::dbExistsTable(con, table_name)) {
    new_records <- table_df
    #result <- DBI::dbAppendTable(con, table_name, new_records)

    result <- db_replace_records(con, user_id, table_name, new_records)
    msg <- if(!is.null(result)) result else "NULL from writeTable"
  } else { # table doesn't exist, so create one from scratch
    new_table <- table_df
    result <- DBI::dbWriteTable(con, table_name, new_table)
    msg <- if(!is.null(result)) result else "NULL from writeTable"
  }
  return(sprintf("Wrote to table with result = %s",msg))
}


#' @title Replace User Information in Database
#' @description Find the user matching this `user_id` and replace
#' with the other values in `user`
#' @param con valid database connection
#' @param user list representing user object
#' @return logical does it succeed or fail?
db_replace_user <- function(con, user) {

  first_name = if(is.null(user$first_name)) "" else user$first_name
  last_name = if(is.null(user$last_name)) "" else user$last_name
  age = user$age
  user_id = user$user_id

  s <- sprintf("UPDATE user_list SET first_name = '%s', last_name = '%s' WHERE user_id = %d",
               first_name, last_name, user_id)

  result <- DBI::dbExecute(con, s)

  if (result > 0) return(TRUE) else return(FALSE)


}

#' @title Insert a new `user_id` into appropriate tables
#' @param con valid database connection
#' @param user list containing `$user_id` and `$firebase_id`
#' @return integer total of rows affected in database write operations. You have a problem if this is 0.
db_insert_user <- function(con, user){
  if(is.null(user)) {warning("tried to insert NULL user"); return(0)}

  ID <- user[["user_id"]]
  if (is.null(ID)) {warning("tried to insert NULL user"); return(0)}
  user_fb <- user[["firebase_id"]]
  if (is.null(user_fb)) {warning("tried to insert NULL firebase_id"); return(0)}

  if (nrow(tbl(con, "user_list") %>% filter(user_id == ID) %>% collect()) ==  0) {
  s1 <- sprintf("INSERT INTO user_list(user_id)
               VALUES(%d)",
               ID)
  result1 <- DBI::dbExecute(con, s1)
  } else result1 = 0

  # don't insert new user_id if user_id already exists in accounts_firebase

  if (nrow(tbl(con, "accounts_firebase") %>% filter(user_id == ID) %>% collect()) ==  0) {

  s2 <- sprintf("INSERT INTO accounts_firebase(user_id, firebase_id)
               VALUES(%d,'%s')",
                ID,user_fb)
  result2 <- DBI::dbExecute(con, s2)
  } else result2 = 0


  return(result1+result2)  # you have a problem if return = 0
}

#' @title Replace All Records in `table_name` that match `user_id`
#' @param con valid database connection
#' @param user_id user ID
#' @param table_name character string name of a database table
#' @param table_df dataframe of records to substitute
#' @return integer number of rows affected
db_replace_records <- function( con, user_id, table_name, table_df) {

  if(is.null(user_id)) {return(0)}


  sql_drop <- glue::glue_sql('
                           DELETE
                           FROM {`table_name`}
                           WHERE user_id IN ({user_id*})

                             ',
                             .con = con)

# todo add some type of COMMIT to this SQL, so the old records aren't deleted until we're sure the new ones are in place.
  # BEGIN
  # DELETE FROM # as above
  # INSERT INTO
  # COMMIT
  query <- DBI::dbSendStatement(con, sql_drop)
  results <- DBI::dbGetRowsAffected(query)
  DBI::dbClearResult(query)

  if (results>0) {message(sprintf("User IDs replaced in %s rows",results))}
  DBI::dbWriteTable(con, table_name, table_df, append = TRUE)

  return(results)
}


#' @title List all valid experiments
#' @description Return a vector of all experiments currently available on the platform.
#' Unlike [taster::db_food_list()], it uses the values from the database table `experiments`.
#' so it will not find experiments (usually food names) for some people. On the other hand,
#' this calls the database each time, so you'll see performance problems if you do it too often.
#' @param con valid database connection
#' @return character vector of all experiment names
#' @export
db_experiments_list <- function(con) {
  result = NULL
  if(DBI::dbExistsTable(con, "experiments"))
  { result <- tbl(con, "experiments") %>% pull("experiment_name")}
  return(result)
}


#' @title List all products consumed by `user_id`
#' @description Return all products consumed by this user. If `user_id==NULL` then show
#'  from all users, all products consumed more than once.
#' @param con valid database connection
#' @param user_id vector of user IDs or NULL to show all users
#' @return character vector of product names sorted alphabetically
#' @importFrom dplyr tbl collect filter distinct group_by transmute pull add_count arrange
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
db_food_list<- function(con, user_id = 1234  ) {

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

  return(prods)
}

# psi User Management Functions

#' @title Returns privileges assigned to this user
#' @param con valid database connection
#' @param user_id user ID
#' @return privilege level, NULL if no privileges available
#' @export
db_user_privileges <- function(con, user_id = -1){

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

  return(result)
}

#' @title All user ids in the database
#' @param conn_args database connection
#' @return vector of all known `user_id`
#' @importFrom dplyr tbl collect
#' @export
db_user_all_ids <- function(con){


  user_list <- db_get_table(con, "user_list")
  if(is.null(user_list)) { message("no users in table user_list"); return(NULL)}
  accounts_list <- db_get_table(con, "accounts_user")
  if(is.null(accounts_list)) { message("no users in table accounts_list") ; return(NULL)}

  u_id <- user_list[["user_id"]]
  a_id <- accounts_list[["user_id"]]

  all_ids <- union(u_id,a_id)

  return(all_ids)

}

#' @title Find User From Firebase ID
#' @description A signed-in user, by definition, has a Firebase ID.  Because the ID was
#' registered as a unique `user_id` when the account was created, we can look it up in
#' the user table and return its user (and username)
#' @param con valid database connection
#' @param firebase_id valid firebase ID for a signed in user
#' @return numeric user_id; NA if no user exists or if the table is not in the database
#' @export
db_user_id_from_firebase <- function(con, firebase_id) {
  f_id <- firebase_id

  if (!DBI::dbExistsTable(con, "accounts_firebase")) {
    cat(file=stderr(), sprintf("table `accounts_firebase` does not exist in database %s", class(con)))
    DBI::dbDisconnect(con)
    return(NA)
  }
    f_match <- tbl(con, "accounts_firebase") %>% filter(f_id == firebase_id) %>% collect()
    if(nrow(f_match)>0)
      user_id <- first(f_match[["user_id"]])
    else user_id <- NA

    return(user_id)
}

#' @title Users Visible at Privilege
#' @param con valid database connection
#' @param user_id user ID
#' @return vector of user lists in the form (`user_id`, `username`)
#' @export
db_users_visible <- function(con, user_id = -1 ) {

  ID = if(is.numeric(user_id)) user_id else 0
  privilege <- "user"

  visible_user_ids <- union(0,c(1234,ID))  # you can always see your own ID


  db_get_table(con, "accounts_user")

  privilege <- if (DBI::dbExistsTable(con, "accounts_user")){
    priv_rows <- tbl(con, "accounts_user") %>% filter(user_id == ID)  %>% collect()
    if(nrow(priv_rows) > 0) first(priv_rows[["privilege"]]) else "nobody"
  } else "user"

  if (privilege == "admin")
    visible_user_ids <- db_user_all_ids(con) %>% union(0)



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

  users_list <- db_get_table(con, "user_list")
  if(is.null(users_list)) { warning("no users in table user_list"); return(NULL)}
  accounts_list <- db_get_table(con, "accounts_user")
  if(is.null(accounts_list)) { warning("no users in table accounts_list"); return(NULL)}

    return(inner_join(users_list,accounts_list) %>% dplyr::filter(user_id == ID)  %>%
             select(first_name,last_name) %>%
             as.character() %>%
             stringr::str_flatten(collapse = " "))
}



