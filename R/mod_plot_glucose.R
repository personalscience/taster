#' plot_glucose UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_glucose_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      plotOutput(ns("libreview"))
  ))
}

#' @title Plot a glucose chart - server
#' @description
#' Given a (reactive) libreview dataframe, this Shiny module will
#' generate a valid ggplot object and display it in an accompanying UI
#' @param id shiny module id
#' @param glucose_df reactive for a valid glucose dataframe
#' @return ggplot object representing a glucose chart
#' @export
mod_plot_glucose_server <- function(id, glucose_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(glucose_df(),
                 {     cat(file=stderr(),
                           sprintf("User dataframe still has %d rows\n", nrow(glucose_df())))
                   output$libreview <- renderPlot(plot_glucose(glucose_df()))
                 }
    )

    current_glucose <- reactive({cat(file=stderr(), "inside reactive")
      glucose_df()})


    #cat(file=stderr(), current_glucose())
    g <- reactive(plot_glucose(current_glucose()))
    return(g)

  })
}

## To be copied in the UI
# mod_plot_glucose_ui("plot_glucose_ui_1")

## To be copied in the server
# mod_plot_glucose_server("plot_glucose_ui_1")

demo_plot1 <- function() {

  gdf <- cgmr::glucose_df_from_libreview_csv()
  ui <- fluidPage(mod_plot_glucose_ui("x"))
  server <- function(input, output, session) {
    gdf <- reactive(cgmr::glucose_df_from_libreview_csv())
    mod_plot_glucose_server("x", gdf)
  }
  shinyApp(ui, server)

}
