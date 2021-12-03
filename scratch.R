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

me <- list(user_id = 1234, first_name = "Richard", last_name = "Sprague", firebase_id = "769d1YgcNfTy4rQlxTuMqWR0b3t2")
me1 <- list(user_id = 1234, firebase_id = "769d1YgcNfTy4rQlxTuMqWR0b3t2")
u = list(first_name = "a",
            last_name = "z",
            user_id = NULL,
            firebase_id = "a1")

user_find_id(con, me1)
