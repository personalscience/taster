#' csv_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_csv_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      fileInput(ns("ask_filename"), label = "Choose CSV File", accept = ".csv"),
      plotOutput(ns("modChart")),
      hr(),
      wellPanel(dataTableOutput(ns("glucoseTable")))
    )

  )
}

#' csv_upload Server Functions
#'
#' @noRd
mod_csv_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filepath<- reactive({
      validate(
        need(input$ask_filename,"Please select a file")
      )
      input$ask_filename})


    glucose_df <- reactive(cgmr::glucose_df_from_libreview_csv(file=filepath()$datapath))

    output$modChart <- renderPlot({
      plot_glucose(glucose_df(),"Glucose Results")
    }
    )

    output$glucoseTable <- renderDataTable(
      glucose_df(),
      options = list(pageLength = 5))

    return(glucose_df)
  })
}

## To be copied in the UI
# mod_csv_upload_ui("csv_upload_ui_1")

## To be copied in the server
# mod_csv_upload_server("csv_upload_ui_1")
