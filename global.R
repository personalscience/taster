# global variables
library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(RPostgres)

library(psiCGM)

source("mod_goddessUI.R")
source("psi_db_taster_notes.R")
taster_foods <- taster_notes_df %>% distinct(Comment) %>% pull(Comment)


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


