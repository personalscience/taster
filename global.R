# global variables
library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(RPostgres)

library(psiCGM)

source("mod_goddessUI.R")

library(showtext)
font_add_google("Montserrat")
showtext_auto()

psi_theme <-   theme(text = element_text(# family = "Montserrat",
  face = "bold", size = 15),
  axis.text.x = element_text(size = 15, angle = 90, hjust = 1),
  legend.title = element_blank())



