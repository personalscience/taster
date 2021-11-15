# Okay to delete anything here


library(cgmr)
library(tidyverse)
library(lubridate)
devtools::load_all(path="/Users/sprague/dev/psi/tasterdb")


#Sys.setenv(R_CONFIG_ACTIVE = "tastercloud")
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

GLUCOSE_RECORDS<- tbl(con,"glucose_records") %>% collect()
NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

df <- cgmr::df_for_all_auc(food_list_db(), GLUCOSE_RECORDS,
                           NOTES_RECORDS)

df %>% pull(meal)

df %>% mutate(user_id = meal %>% stringr::str_extract("^([:digit:])+(?=-)") %>% as.numeric())
x = build_all_AUC(s_list = food_list_db(),
              glucose_records = GLUCOSE_RECORDS,
              notes_records = NOTES_RECORDS)
x

