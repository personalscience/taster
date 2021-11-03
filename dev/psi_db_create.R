# create_db
# Use this script only once: to set up the initial database and scheme
# If your Postgres database is already set up and running, you should be able to simply 'source' this script
# and it will automatically create the database 'qsdev' and a table 'glucose_records'
# Note: nothing bad should happen if you source this on an existing database (i.e. nothing will happen)

# WARNING: Be sure you know the value of R_CONFIG_ACTIVE before running this script.
# It will blindly create whatever database is in config::get("dataconnection")

library(tidyverse)

GLUCOSE_LAZY_FRAME <- dbplyr::tbl_lazy(tibble(time=lubridate::now(),
                                         scan = 0.0,
                                         hist = 0.0,
                                         strip = 0.0,
                                         value = 0.0,
                                         food = "",
                                         user_id = 0.0),
                                       con = dbplyr::simulate_postgres())
GLUCOSE_DATA_FRAME <-
  tibble(time=lubridate::now(), scan = 0.0, hist = 0.0, strip = 0.0, value = 0.0, food = "", user_id = 0.0)
NOTES_DATA_FRAME <-
  tibble(Start=lubridate::now(), End =lubridate::now(), Activity = "Event", Comment = NA, Z = NA, user_id = 0)
USER_DATA_FRAME <-
  tibble(first_name = "first", last_name = "last", birthdate = as.Date("1900-01-01"), libreview_status = as.character(NA), user_id = 0)

# set the active configuration globally via Renviron.site or Rprofile.site
# Sys.setenv(R_CONFIG_ACTIVE = "tastercloud")
#Sys.setenv(R_CONFIG_ACTIVE = "local")  # save to local postgres
# Sys.setenv(R_CONFIG_ACTIVE = "localtest") # a database useful for testing
# Sys.setenv(R_CONFIG_ACTIVE = "cloud") # save to cloud
# Sys.setenv(R_CONFIG_ACTIVE = "default") # save to sqlite
# Sys.setenv(R_CONFIG_ACTIVE = "cloud")


conn_args <- config::get("dataconnection")
message("Preparing to create ", conn_args$host)

#' List all objects in the current PSI database
#' @import DBI
psi_list_objects <-
  function(conn_args = config::get("dataconnection")) {
    con <- DBI::dbConnect(
      drv = conn_args$driver,
      user = conn_args$user,
      host = conn_args$host,
      port = conn_args$port,
      dbname = conn_args$dbname,
      password = conn_args$password
    )

  dbName <- conn_args$dbname
  dbHost <- conn_args$host

  objects <- DBI::dbListObjects(con)
  tables <- DBI::dbListTables(con)

  DBI::dbDisconnect(con)
  return(list(dbName=dbName, dbHost=dbHost, objects=objects, tables=tables))

}


#' Make new Postgres database if one doesn't already exist.
#' @param conn_args a valid database connection (assumes Postgres)
#' @import DBI
psi_make_database_if_necessary <- function(conn_args = config::get("dataconnection")) {
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
   # dbname = conn_args$dbname,
    password = conn_args$password
  )


  newdb_sqlstring <-
    paste0(
      "CREATE DATABASE ",
      conn_args$dbname,
      "
            WITH
            OWNER = postgres
            ENCODING = 'UTF8'
            CONNECTION LIMIT = -1;"
    )

  ## Add a new database "qsdb" if none exists on this server
  if (conn_args$dbname %in%
      DBI::dbGetQuery(con, "SELECT datname FROM pg_database WHERE datistemplate = false;")$datname)
    { message("database already exists")
    return(NULL)
  } else
    DBI::dbSendQuery(con, newdb_sqlstring)

  # Now that qsdb is available, use that as the database for everything
  DBI::dbDisconnect(con)

}

#' @title Make new database tables if necessary
#' @param conn_args connection
#' @param table_name character string name for the table.
#' @param table a valid glucose data frame. Never use the default value unless you are testing.
#' @import DBI
#' @return NULL if table already exists. Otherwise creates the table and returns TRUE invisibly.
psi_make_table_if_necessary <- function(conn_args = config::get("dataconnection"),
                                        table_name = "glucose_records",
                                        table = GLUCOSE_DATA_FRAME) {
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  if (DBI::dbExistsTable(con, table_name))
  {message(paste0("Table '",table_name,"' already exists"))
  } else {
   # DBI::dbCreateTable(con, table_name, table)
    message(paste("Writing to table", table_name))
    DBI::dbWriteTable(con, name = table_name, value = table, overwrite=TRUE)
  }

  DBI::dbDisconnect(con)

  return(NULL)

}

#' Read contents of `table` from the database and return it as a dataframe
#' @title Show table as a dataframe
#' @param conn_args valid data connection
#' @param table_name character string of the table name (default: `glucose_records`)
#' @import DBI
#' @return dataframe
psi_table_df <-
  function(conn_args = config::get("dataconnection"),
           table_name = "glucose_records") {
    con <- DBI::dbConnect(
      drv = conn_args$driver,
      user = conn_args$user,
      host = conn_args$host,
      port = conn_args$port,
      dbname = conn_args$dbname,
      password = conn_args$password
    )

    table_df <- tbl(con, table_name) %>% collect()

    DBI::dbDisconnect(con)

    return(table_df)

  }


#' @title Make a new database tables with `index`
#' @param conn_args connection
#' @param table_name character string name for the table.
#' @param table a valid glucose data frame. Never use the default value unless you are testing.
#' @param index (list) table column to be used for index
#' @import DBI
#' @return NULL if table already exists. Otherwise creates the table and returns TRUE invisibly.
psi_make_table_with_index <- function(conn_args = config::get("dataconnection"),
                                      table_name = "experiments",
                                      table = NULL,
                                      index = NULL
                                      ){
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  if (DBI::dbExistsTable(con, table_name))
  {message(paste0("Table '",table_name,"' already exists"))
  } else {

    message(sprintf("Writing new table %s with index %s", table_name, index))
    # Lets you create an index
    dplyr::copy_to(con, experiments, name = table_name, index = index, temporary = FALSE)
  }

  DBI::dbDisconnect(con)
}

# Start Here ----
# Makes a naked database filled with some sample values


psi_make_database_if_necessary()
psi_list_objects()

message("write placeholder info into each table")
psi_make_table_if_necessary(table = glucose_df_from_libreview_csv())
psi_make_table_if_necessary(table_name = "notes_records", table = NOTES_DATA_FRAME)
psi_make_table_if_necessary(table_name = "user_list", table = USER_DATA_FRAME)
psi_make_table_with_index(table_name = "experiments",
                          table = read_csv(file=file.path(config::get("tastermonial")$datadir,
                                                                                      "tastermonial_experiments.csv"), col_types = "dcc",
                                                                       show_col_types = FALSE),
                          index = list("experiment_id")
                          )



