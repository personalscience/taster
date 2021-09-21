#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




# source("mod_libreviewUI.R")
# source("mod_filter.R")
# source("read_data_utils.R")
# source("mod_user_selectionUI.R")
#source("psi_user_management.R")

#' Define UI for application that reads a CSV file
#' @import shiny
#' @import magrittr dplyr
ui <- fluidPage(  useFirebase(), # import dependencies
                  useFirebaseUI(), # import UI,
                  navbarPage("Tastermonial", collapsible = TRUE, inverse = TRUE,

                 reqSignin(tabPanel("Glucose Analysis",
                          fluidPage(
                              h2("Your CGM Data"),
                              mod_goddessUI("food2_compare_plot")
                          )),
                 tabPanel("All Foods",
                          fluidPage(
                              mod_foodTasterUI("food_compare_plot")
                          )),
                 tabPanel("User View",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      mod_filterUI("psi_filter_plot")

                                  ),

                                  mainPanel(


                                      mod_libreviewUI("modChart")

                                  ))
                          ))
                 ),
                 tabPanel("CSV Load",
                          fluidPage(
                            titlePanel("Upload your own Libreview CSV results"),
                            showLibreviewUI("fromCSV")
                          )),
                 tabPanel("About",
                          fluidPage(
                              #Application title
                              titlePanel("Personal Science Experiments", windowTitle = "Personal Science, Inc."),
                              tags$a(href="https://personalscience.com", "More details"),
                              textOutput("currentDB"),
                          ))
                 )
)


#includeCSS(file.path("www","psi_shiny.css")),
# Application title
# titlePanel("Personal Science Experiments", windowTitle = "Personal Science, Inc."),
# tags$a(href="https://personalscience.com", "More details"),
# textOutput("currentDB"),
#
# # Application title
# h2("Your CGM Data"),
# mod_goddessUI("food2_compare_plot"),
# mod_foodUI("food_compare_plot"),
# # Show a plot of the glucose levels
# sidebarLayout(
#     sidebarPanel(
#         mod_filterUI("psi_filter_plot")
#
#     ),
#
#     mainPanel(
#
#
#         mod_libreviewUI("modChart")
#
#     ))



