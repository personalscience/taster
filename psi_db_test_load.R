# Create database entries for test purposes
# NOTE: you need the functions in psi_db_create.R for this to work (but careful: don't set the wrong config environment)


active_env <- Sys.getenv("R_CONFIG_ACTIVE")
Sys.setenv(R_CONFIG_ACTIVE = "localtest")

# enter your db writes here

#' fill the test database
psi_fill_test_from_scratch <- function(conn_args = config::get("dataconnection"),
                                      drop = FALSE){

    con <- DBI::dbConnect(
      drv = conn_args$driver,
      user = conn_args$user,
      host = conn_args$host,
      port = conn_args$port,
      dbname = conn_args$dbname,
      password = conn_args$password
    )

    martha_glucose <- glucose_df_from_libreview_csv(
      user_id = 1235,
      system.file("extdata", package = "psiCGM", "Firstname1Lastname1_glucose.csv")
    )
    richard_glucose <- glucose_df_from_libreview_csv(
      user_id = 1234,
      system.file("extdata", package = "psiCGM", "Firstname2Lastname2_glucose.csv")
    ) %>% filter(time>as_date("2021-06-01"))



    martha_notes <-
      notes_df_from_csv(
        user_id = 1235,
        file = system.file("extdata", package = "psiCGM", "Firstname1Lastname1_notes.csv")
      )


    if (drop) {
      message("removing notes records")
      DBI::dbRemoveTable(con, "notes_records")
      message("removing glucose_records")
      DBI::dbRemoveTable(con, "glucose_records")
      psi_make_table_if_necessary()  #defaults to glucose_records
      psi_make_table_if_necessary(table_name = "notes_records", table = martha_notes)
    }

    richard_notes <-
      notes_df_from_glucose_table(user_id=1234)
    # notes_df_from_csv(
    #   user_id = 1234,
    #   file = system.file("extdata", package = "psiCGM", "Firstname2Lastname2_notes.csv")
    # )


    DBI::dbWriteTable(con, "glucose_records", bind_rows(martha_glucose,richard_glucose), overwrite=TRUE)
    DBI::dbWriteTable(con, "notes_records", bind_rows(richard_notes,
                                                      martha_notes,
                                                      notes_df_from_glucose_table(user_id=1235)), overwrite=TRUE)

}

psi_fill_test_from_scratch()

Sys.setenv(R_CONFIG_ACTIVE = active_env )
