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

s <- 'SELECT max("user_id") FROM user_list;'
r <- DBI::dbGetQuery(con, s) %>% pull(max)
r
class(r)

tbl(con, "user_list") %>% filter(user_id == max(user_id, na.rm = TRUE)) %>% pull(user_id)
