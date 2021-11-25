# Okay to delete anything here


library(cgmr)
library(tidyverse)
library(lubridate)
devtools::load_all(path="/Users/sprague/dev/psi/tasterdb")


#Sys.setenv(R_CONFIG_ACTIVE = "tastercloud")
Sys.getenv("R_CONFIG_ACTIVE" = "sqldb")
Sys.setenv(R_CONFIG_ACTIVE = "local")
conn_args <- config::get("dataconnection")
con <- DBI::dbConnect(
  drv = conn_args$driver,
  user = conn_args$user,
  host = conn_args$host,

  port = conn_args$port,
  dbname = conn_args$dbname,
  password = conn_args$password
)
#con <- db_connection()

cgm_data <- cgmObject(con)

cgmr::food_times_df_fast(
  glucose_df = cgm_data$glucose_records,
  notes_df = cgm_data$notes_records,
  user_id = 1003,
  foodname = "Clif Bar Chocolate")


