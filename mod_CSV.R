# module to read CSV files and plot glucose




libreview_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    fileInput("ask_filename", "Choose CSV File", accept = ".csv"),
    dataTableOutput(NS(id, "glucoseTable"))
  )

}

test_server <- function(id,  librelink_csv) {
  message(librelink_csv)
  moduleServer(id, function(input, output, session) {
    glucose_df <- reactive(glucose_from_csv(librelink_csv))

    output$glucoseTable <- renderDataTable(
      glucose_df(),
      options = list(pageLength = 5))


  })

}

libreCSV_demo <- function() {


  ui <- fluidPage(libreview_ui("x"))
  server <- function(input, output, session) {
    test_server("x", DEFAULT_LIBRELINK_FILE_PATH)
  }
  shinyApp(ui, server)

}

if(interactive()){libreCSV_demo()}
