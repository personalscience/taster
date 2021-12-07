#' metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metadata_ui <- function(id){
  ns <- NS(id)
  tagList(h1("Edit Data"),
          textOutput(ns("user")),
                     hr(),
          textOutput(ns("message")),
          DT::DTOutput(ns("notes_table"))
          )


}

#' metadata Server Functions
#'
#' @noRd
mod_metadata_server <- function(id, cgm_data, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$user <- renderText({
      print_user(user)
    })

    output$message <- renderText({
      validate(
        need(input$notes_table_cells_selected, "Please select a cell to edit")
      )
      cell <- input$notes_table_cells_selected
      value <- cgm_data$notes_records[cell[1],cell[2]]
      paste0(sprintf("You clicked row=%s, col=%s\n", cell[1],cell[2]),
          sprintf("value = %s", value))

    })

    output$notes_table <- DT::renderDT({
      DT::datatable(cgm_data[["notes_records"]], editable = TRUE, selection = list(mode = 'single',
                                                                                   target = 'cell'))
      })

  })
}

## To be copied in the UI
# mod_metadata_ui("metadata_ui_1")

## To be copied in the server
# mod_metadata_server("metadata_ui_1")
#' @description Demo for mod_filter
#' @noRd
#'
demo_meta <- function() {
  ui <- fluidPage(firebase::useFirebase(),
                  firebase::firebaseUIContainer(),
                  mod_metadata_ui("metadata_ui_1"))
  #sample_glucose <- cgmr::glucose_df_from_libreview_csv()
  server <- function(input, output, session) {
    con <- db_connection()
    cgm_data <- CgmObject(con)
    f <- firebase_setup(con)
    user <- UserObject(con, firebase_obj = f)
    g <- mod_metadata_server("metadata_ui_1", cgm_data, user)
    # message(sprintf("g = %s", str(g$con())))
    onStop(function(){message("gracefully exiting...")
            DBI::dbDisconnect(con)})
  }
  shinyApp(ui, server)
}
