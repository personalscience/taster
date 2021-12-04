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

scon <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

sample_table <- tibble(id = c(1,2,3),
                       name = c("a","b","c"),
                       user_id = c(7,8,9),
                       firebase_id = c("x","y","z"))

new_user_records <- tibble(id = c(1),
                           name = c("x"),
                           user_id = c(7),
                           firebase_id = c("x"))

db_write_table(scon, table_name = "sample_table", sample_table)
tbl(scon, "sample_table")

table_name = "sample_table"

sql <- glue::glue_sql("
                          SELECT *
                          FROM {`table_name`}
                          WHERE id = ?
                          ",
                      .con = scon)

sql_drop <- glue::glue_sql("
                           DELETE
                           FROM {table_name}
                           WHERE user_id = ?

                             ",
                           .con = scon)


db_replace_records(scon, user_id = c(21), "sample_table", new_user_records)
