#' metadata UI Function
#'
#' @description A shiny Module to let you view and modify metadata for a user.
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

    current_user <- reactive({
      this_user <- user$f$get_signed_in()
      validate(need(!is.null(this_user), "Not signed in"))
      user$user_id <- db_user_id_from_firebase(user$con,this_user$response$uid)
      user$firebase_id <- this_user$response$uid
      new_user <- UserObject(user$con, user$user_id, firebase_obj = user$f)
      message(sprintf("Current_user()  user_id = %s firebase_id = %s/n", user$user_id, user$firebase_id))
      return(new_user)

    })

    output$user <- renderText({
      this_user <- current_user()
      print_user(this_user)
    })

    selected_notes_records <- reactive({
      user <- current_user()
      ID <- user$user_id
      current_user_notes <- cgm_data[["notes_records"]] %>% filter(user_id == ID)
      return(current_user_notes)
    })

    output$message <- renderText({
      validate(
        need(input$notes_table_cells_selected, "Please select a cell to edit")
      )

      cell <- input$notes_table_cells_selected
      selected_notes <- selected_notes_records()
      value <- selected_notes[cell[1],cell[2]]
      paste0(sprintf("You clicked row=%s, col=%s\n", cell[1],cell[2]),
          sprintf("value = %s", value))

    })

    output$notes_table <- DT::renderDT({

      current_user_notes <- selected_notes_records()
      DT::datatable(current_user_notes,
                    editable = TRUE,
                    selection = list(mode = 'single',
                                     target = 'cell'))
      })

  })
  return(TRUE)
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
