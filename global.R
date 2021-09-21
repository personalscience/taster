# global variables
library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(RPostgres)
library(firebase)

library(psiCGM)

conn_args <- config::get("dataconnection")
con <- DBI::dbConnect(
  drv = conn_args$driver,
  user = conn_args$user,
  host = conn_args$host,
  port = conn_args$port,
  dbname = conn_args$dbname,
  password = conn_args$password
)


GLUCOSE_RECORDS<- tbl(con,"glucose_records") %>% collect()
NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

source("mod_goddessUI.R")
source("mod_CSV.R")
taster_foods <- food_list_db(user_id = c(1001:1004,1007:1021))
source("mod_foodTaster_compare.R")


library(showtext)
font_add_google("Montserrat")
showtext_auto()

psi_theme <-   theme(text = element_text(# family = "Montserrat",
  face = "bold", size = 15),
  axis.text.x = element_text(size = 15, angle = 90, hjust = 1),
  legend.title = element_blank())

username_for_id <- function (user_id)
{
  ID = user_id
  if (ID == 0)
    return("Unknown Name")
  else user_df_from_libreview %>% filter(user_id == ID) %>%
  select(first_name, last_name) %>%
    stringr::str_flatten(collapse = " ") %>% str_match("[:alnum:]+ [:alnum:]") %>% as.character()
}

# Firebase ----
# If you are using Firebase, you must
# run once:
# firebase::firebase_config(api_key = config::get("tastermonial")$firebaseapiKey,
#                           project_id = config::get("tastermonial")$firebasePID)

