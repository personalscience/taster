# module to read CSV files and plot glucose




showLibreviewUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    fileInput(ns("ask_filename"), label = "Choose CSV File", accept = ".csv"),
    plotOutput(ns("modChart")),
    hr(),
    wellPanel(dataTableOutput(ns("glucoseTable")))
  )

}

csv_read_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    filepath<- reactive({
      validate(
        need(input$ask_filename,"Please select a file")
      )
      input$ask_filename})


    glucose_df <- reactive(glucose_df_from_libreview_csv(file=filepath()$datapath))

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

libreCSV_demo <- function() {


  ui <- fluidPage(showLibreviewUI("x"))
  server <- function(input, output, session) {
    csv_read_server("x")
  }
  shinyApp(ui, server)

}

if(interactive()){libreCSV_demo()}
