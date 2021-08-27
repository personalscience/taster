#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library(tidyverse)
library(lubridate)

library(DBI)
library(RPostgres)

#Sys.setenv(R_CONFIG_ACTIVE = "local")


#' Define server logic required to display CGM information
#' @import shiny
#' @import magrittr dplyr
server <- function(input, output) {

    # datafilepath <- psiCGM:::csvFilePathServer("datafile")
    output$currentDB <- renderText(sprintf("DB=%s. psiCGM version = %s",
                                           attr(config::get(),"config"),
                                           packageVersion("psiCGM")))


   mod_food2Server("food_compare_plot", active_glucose_record, title = "Tastermonial" )

}






