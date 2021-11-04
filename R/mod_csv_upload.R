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
#' @param con database connection
#' @noRd
mod_csv_upload_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filepath<- reactive({
      validate(
        need(input$ask_filename,"Please select a Libreview CSV file")
      )
      input$ask_filename})


    glucose_df_raw <- reactive(cgmr::libreview_csv_df(file=filepath()$datapath))
    glucose_df <- reactive(glucose_df_raw()[["glucose_raw"]] %>%     transmute(`time` = `timestamp`,
                                                                               scan = glucose_scan,
                                                                               hist = glucose_historic,
                                                                               strip = strip_glucose,
                                                                               value = hist,
                                                                               food = notes))
    libreview_name <- reactive(glucose_df_raw()[["name"]])

    output$modChart <- renderPlot({
      plot_glucose(glucose_df(),sprintf("Glucose Results for %s",libreview_name()))
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

#' @description Demo for mod_filter
#' @noRd
#'
demo_csv <- function() {
  ui <- fluidPage(mod_csv_upload_ui("csv_upload_ui_1"))
  #sample_glucose <- cgmr::glucose_df_from_libreview_csv()
  server <- function(input, output, session) {
    mod_csv_upload_server("csv_upload_ui_1")

  }
  shinyApp(ui, server)
}

