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
con <- db_connection()

GLUCOSE_RECORDS<- tbl(con,"glucose_records")  %>%
  collect() #%>%
  mutate(time = lubridate::as_datetime(time))

GLUCOSE_RECORDS %>% head() %>%
  mutate(timestamp = lubridate::with_tz(time, tzone = "America/Los_Angeles"))

NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

cgmr::food_times_df_fast(
  glucose_df = GLUCOSE_RECORDS,
  notes_df = NOTES_RECORDS,
  user_id = 1003,
  foodname = "Clif Bar Chocolate")


