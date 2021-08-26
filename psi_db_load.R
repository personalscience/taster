# Script to load database tables
# Assumes existence of `glucose_records`
# To erase the current glucose_records table from the database and start over:
# psi_fill_database_from_scratch(drop=TRUE)

# To erase all notes_records and start over:
# DBI::dbRemoveTable(con, "notes_records")

# see more examples at https://rpostgres.r-dbi.org/
library(tidyverse)
library(lubridate)



# set the active configuration globally via Renviron.site or Rprofile.site
#Sys.setenv(R_CONFIG_ACTIVE = "local")  # save to local postgres
# Sys.setenv(R_CONFIG_ACTIVE = "cloud") # save to cloud
# Sys.setenv(R_CONFIG_ACTIVE = "default") # save to sqlite
# Sys.setenv(R_CONFIG_ACTIVE = "cloud")


#' @title Write a glucose dataframe to the database
#' @description
#' WARNING: always delete the table before running this on a `user_id` that's already in the database.
#' (it's not finished yet and doesn't take into account previous entries)
#' @param new_table valid formatted glucose dataframe
psi_write_glucose <- function(conn_args = config::get("dataconnection"),
                              user_id = 1235,
                              new_table=glucose_df_from_libreview_csv(user_id = 1235)) {

    con <- DBI::dbConnect(
        drv = conn_args$driver,
        user = conn_args$user,
        host = conn_args$host,
        port = conn_args$port,
        dbname = conn_args$dbname,
        password = conn_args$password
    )

    ID <- user_id

    psi_make_table_if_necessary(conn_args = conn_args, table = new_table)

    maxDate <- psiCGM:::max_date_for_user(conn_args, user_id = ID)
    new_records <-
       new_table %>% dplyr::filter(time > {if(is.na(maxDate)) min(time) else maxDate}) %>%
        dplyr::filter(user_id == ID)



    message("write glucose records")

    # uncomment the following line to do the actual write to db
   DBI::dbWriteTable(con, name = "glucose_records", value = new_records, row.names = FALSE, append = TRUE)

    # uncomment the following line
    # DBI::dbWriteTable(con, name = "notes_records", value = notes_records, row.names = FALSE, overwrite = TRUE)


    DBI::dbDisconnect(con)

}



#' @title Write a Notes CSV to the notes table in the database
#' @description
#' WARNING: this may not work correctly. debug before using
#' psi_write_notes(user_id = 1234, new_table = notes_df_from_glucose_table(user_id = 1234))
#' @param user_id user ID
#' @param new_table valid formatted notes dataframe
#' @param dry_run (default = TRUE). Run without actually writing to the database
psi_write_notes <- function(conn_args = config::get("dataconnection"),
                               user_id = 1235,
                               new_table=notes_df_from_csv(user_id = 1235),
                            dry_run = TRUE) {

    con <- DBI::dbConnect(
        drv = conn_args$driver,
        user = conn_args$user,
        host = conn_args$host,
        port = conn_args$port,
        dbname = conn_args$dbname,
        password = conn_args$password
    )

    ID <- user_id
    message("write notes records")

    if (DBI::dbExistsTable(con, "notes_records"))
        {message("Table 'notes_records' already exists")
    } else {
        message(paste("Writing to notes_records"))
        DBI::dbWriteTable(con, name = "notes_records", value = new_table, overwrite=TRUE)
    }

    maxDate <-
        tbl(con, "notes_records") %>%
        filter(user_id == ID) %>%
        filter(Start == max(Start, na.rm = TRUE)) %>% collect() %>%
        pull(Start)

    maxDate <- if(length(maxDate)>0) maxDate else {tbl(con, "notes_records") %>%
            filter(Start == min(Start)) %>% collect() %>% pull(Start) }

    new_records <-
        new_table %>%  dplyr::filter(user_id == ID) %>%
        dplyr::filter(Start > {if(is.na(maxDate)) min(Start) else maxDate})




    if(dry_run){
        message("not going to actually write this")
    } else {
    # uncomment the following line to do the actual write to db
    DBI::dbWriteTable(con, name = "notes_records", value = new_records, row.names = FALSE, append = TRUE)
    }
    return(new_records)


    DBI::dbDisconnect(con)
}


#' @description
#'  For debugging and dev purposes only. Loads the database tables from scratch.
psi_fill_database_from_scratch <- function(conn_args = config::get("dataconnection"),
                                       drop = TRUE) {

    con <- DBI::dbConnect(
        drv = conn_args$driver,
        user = conn_args$user,
        host = conn_args$host,
        port = conn_args$port,
        dbname = conn_args$dbname,
        password = conn_args$password
    )

    if(drop) {
        message("removing glucose records table")
        DBI::dbRemoveTable(con, "glucose_records")
        message("removing notes records")
        DBI::dbRemoveTable(con, "notes_records")
    }
    martha_glucose <- file.path("/Users/sprague/dev/psi/psiCGM/inst/extdata/Firstname1Lastname1_glucose.csv")
    richard_glucose <- file.path("/Users/sprague/dev/psi/psiCGM/inst/extdata/Firstname2Lastname2_glucose.csv")
    message("write Martha glucose records")
    psi_write_glucose(conn_args = conn_args,
                      user_id = 1235,
                      new_table=glucose_df_from_libreview_csv(file = martha_glucose, user_id = 1235))

    message("Write Martha notes")
    psi_write_notes(user_id = 1235, new_table=notes_df_from_csv(user_id = 1235))

    message("write Richard glucose records")
    psi_write_glucose(conn_args = conn_args,
                      user_id = 1234,
                      new_table=glucose_df_from_libreview_csv(file = richard_glucose, user_id = 1234))

 #  message("Append Richard Notes records (from glucose_records")
    DBI::dbWriteTable(con, name = "notes_records", value = notes_df_from_glucose_table(user_id=1234), row.names = FALSE, append = TRUE)
  # psi_write_notes(user_id = 1234, new_table = notes_df_from_glucose_table(user_id = 1234), dry_run = FALSE)

    message("finished writing")

}


#' @title Read all CSV files again and enter them into the database
#' @return dataframe
psi_fill_glucose_records_from_scratch <- function(conn_args = config::get("dataconnection"),
                                                   drop = TRUE) {

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  if(drop) {
    message("removing glucose records table")
    DBI::dbRemoveTable(con, "glucose_records")
  }

all_glucose_records <- load_libreview_csv_from_directory()
DBI::dbWriteTable(con, name = "glucose_records", value = all_glucose_records, row.names = FALSE, overwrite = TRUE)

return(all_glucose_records)

}


#' Nuke all notes records and start over
psi_fill_notes_records_from_scratch <- function(conn_args = config::get("dataconnection"),
                                                drop = TRUE) {

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )
  if(drop) {

    message("removing notes records")
    DBI::dbRemoveTable(con, "notes_records")
  }

  message("Write Martha notes")

  martha_notes <- bind_rows(notes_df_from_csv(user_id=1235),
                            notes_df_from_glucose_table(user_id=1235))

  DBI::dbWriteTable(con, name = "notes_records",
                    value = martha_notes,
                    row.names = FALSE,
                    overwrite = TRUE)

  message("Write Ayumi notes")
  ayumi_notes <-  notes_df_from_glucose_table(user_id=1002)

  DBI::dbWriteTable(con, name = "notes_records",
                    value = ayumi_notes,
                    row.names = FALSE,
                    append = TRUE)

  message("Write Bude notes")
  bude_notes <-  notes_df_from_glucose_table(user_id=1010)

  DBI::dbWriteTable(con, name = "notes_records",
                    value = bude_notes,
                    row.names = FALSE,
                    append = TRUE)

  message("Write Richard Notes records (from glucose_records")
  DBI::dbWriteTable(con, name = "notes_records",
                    value = notes_df_from_glucose_table(user_id=1234),
                    row.names = FALSE,
                    append = TRUE)
  # psi_write_notes(user_id = 1234, new_table = notes_df_from_glucose_table(user_id = 1234), dry_run = FALSE)


}


# Execute from here----

psi_fill_glucose_records_from_scratch()

psi_fill_notes_records_from_scratch()

# uncomment this section to add an arbitrary new CSV file
# be sure to set both user_ids
# Write Andreos:
# psi_write_glucose(user_id = 1004,
#                   new_table = psiCGM:::glucose_df_from_libreview_csv(rstudioapi::selectFile(), user_id = 1004)
# )
# #write Bude:
# psi_write_glucose(user_id = 1008,
#                   new_table = psiCGM:::glucose_df_from_libreview_csv(rstudioapi::selectFile(), user_id = 1008)
# )
#
