# Okay to delete anything here


library(cgmr)
library(tidyverse)
library(lubridate)
devtools::load_all(path="/Users/sprague/dev/psi/tasterdb")


#Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
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

glucose_records <- tbl(con,"glucose_records")
notes_records <- tbl(con,"notes_records")
notes_records

cgmr::food_times_df_fast(glucose_records,notes_records)

# ldb <- tasterdb::load_db("local")
# user_df_from_db() %>% print(n=Inf) %>% select(first_name, last_name, user_id) %>% knitr::kable() %>% clipr::write_clip()
#
# tdb <- tasterdb::load_db("shinyapps")



