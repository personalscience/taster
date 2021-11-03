#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Personal Science Experiments", windowTitle = "Personal Science, Inc."),
    tags$a(href="https://personalscience.com", "More details"),
    textOutput(ns("about_page")),
    textOutput(ns("currentDB")),
    uiOutput(ns("image"))
  )
}

#' about Server Functions
#' @param con database connection
#' @importFrom stats na.omit
#' @noRd
mod_about_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$about_page <- renderText("Be Your Own Scientist")
    output$currentDB <- renderText(sprintf("DB=%s. version = %s, cgmr = %s, db  = %s",
                                           substr(get_golem_config("dataconnection")$host, 1,5),
                                           utils::packageVersion("tastermonial"),
                                           utils::packageVersion("cgmr"),
                                           first(tbl(con,"glucose_records") %>%
                                                   filter(time == max(time, na.rm=TRUE)) %>%
                                                   pull(time) %>%
                                                   lubridate::with_tz(tzone="America/Los_Angeles"))))


    output$image <- renderUI({
      experiments <- if(DBI::dbExistsTable(con, "experiments"))
        {tbl(con, "experiments") %>% collect()}
      else NULL

      tags$img(src=stats::na.omit(experiments$experiment_image_url),
           width = "240px")
    }
    )
  })
}

## To be copied in the UI
# mod_about_ui("about_ui_1")

## To be copied in the server
# mod_about_server("about_ui_1")

demo_about <- function() {
  con <- db_connection()

  ui <- fluidPage(mod_about_ui("x"))
  server <- function(input, output, session) {
    mod_about_server("x",con)

  }
  shinyApp(ui, server)
}
