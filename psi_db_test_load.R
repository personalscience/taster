# Create database entries for test purposes
# NOTE: this doesn't yet work.

active_env <- Sys.getenv("R_CONFIG_ACTIVE")
Sys.setenv(R_CONFIG_ACTIVE = "localtest")

# enter your db writes here

#' fill the test database
psi_fill_test_from_scratch <- function(conn_args = config::get("dataconnection"),
                                      drop = FALSE,
                                      dry_run = TRUE){

    con <- DBI::dbConnect(
      drv = conn_args$driver,
      user = conn_args$user,
      host = conn_args$host,
      port = conn_args$port,
      dbname = conn_args$dbname,
      password = conn_args$password
    )
    if (drop) {
      message("removing notes records")
      DBI::dbRemoveTable(con, "notes_records")
    }

    write_test_database <-
      martha_glucose <- glucose_df_from_csv(
        user_id = 1235,
        system.file("extdata", package = "psiCGM", "Firstname1Lastname1_glucose.csv")
      )
    richard_glucose <- glucose_df_from_csv(
      user_id = 1234,
      system.file("extdata", package = "psiCGM", "Firstname1Lastname1_glucose.csv")
    )

    richard_notes <-
      notes_df_from_csv(
        user_id = 1234,
        file = system.file("extdata", package = "psiCGM", "Firstname2Lastname2_notes.csv")
      )

  }

Sys.setenv(R_CONFIG_ACTIVE = active_env )
