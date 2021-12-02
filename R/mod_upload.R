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
    fluidPage(
      includeMarkdown(app_sys("app/www/docs/upload_instructions.md")),
      hr(),
      fileInput(ns("ask_filename_libreview"), label = "Choose Libreview File", accept = ".csv"),
      uiOutput(ns("ask_to_write_db")),
      hr(),
      fileInput(ns("ask_filename_notes"), label = "Choose Notes File", accept = ".csv"),
      plotOutput(ns("modChart")),
      hr(),
      wellPanel(dataTableOutput(ns("glucoseTable"))),
      wellPanel(dataTableOutput(ns("notesTable")))
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

    # filepath_libreview ----
    filepath_libreview<- reactive({
      validate(
        need(input$ask_filename_libreview,"Please select a Libreview CSV file")
      )
      input$ask_filename_libreview})

    # filepath_notes ----
    filepath_notes<- reactive({
      validate(
        need(input$ask_filename_notes,"Please select a Notes CSV file")
      )
      input$ask_filename_notes})


    # glucose_df : Read Raw ----
    glucose_df_raw <- reactive(cgmr::libreview_csv_df(file=filepath_libreview()$datapath))
    libreview_name <- reactive(glucose_df_raw()[["name"]])

    glucose_df <- reactive({
      g_df <- glucose_df_raw()[["glucose_raw"]] %>%
        transmute(
          `time` = `timestamp`,
          scan = glucose_scan,
          hist = glucose_historic,
          strip = strip_glucose,
          value = hist,
          food = notes,
          user_id = 0
        )
      return(g_df)
      }
    )

    # notes_df ----
    notes_df<- reactive(cgmr::notes_df_from_csv(file = filepath_notes()$datapath))



    # output$modChart ----
    output$modChart <- renderPlot({
      plot_glucose(glucose_df(),sprintf("Glucose Results for %s",libreview_name()))
    }
    )

    # output$ask_to_write_db ----
    output$ask_to_write_db <- renderUI({
      validate(
        need(!is.null(glucose_df()),sprintf("Please upload a CSV file"))
      )
      actionButton(NS(id,"write_db"),
                     label = "Write to Database")


    })


    # input$write_db ----
    observeEvent(input$write_db, {
      if(input$write_db) {
      message(sprintf('writing to %s database now', class(con)))
        # db_write_table(con=con,
        #                table_name = "raw_glucose",
        #                table_df = glucose_df_raw()[["glucose_raw"]])
      }
    })

    # output$glucoseTable ----
    output$glucoseTable <- renderDataTable(
      glucose_df(),
      options = list(pageLength = 5))

    # output$notesTable ----
    output$notesTable <- renderDataTable(
      notes_df(),
      options = list(pageLenth = 5))


    cgm_data <- list(con = reactive(con),
                     glucose_records = glucose_df,
                     notes_records = notes_df)

    return(cgm_data
           ) # This whole module returns a reactive glucose_df

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
    g <- mod_csv_upload_server("csv_upload_ui_1", con = db_connection())
   # message(sprintf("g = %s", str(g$con())))

  }
  shinyApp(ui, server)
}

