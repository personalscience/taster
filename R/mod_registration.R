#' registration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_registration_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Registration page"),
    p("Answers to all questions are optional"),
    uiOutput(ns("questions")),
    actionButton(ns("reg_save"), label = "Save")

  )
}

#' registration Server Functions
#' @param user userObject
#' @noRd
mod_registration_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$questions <- renderUI({
      tagList(
        textInput(ns("first_name"), label = "First Name"),
        textInput(ns("last_name"), label = "Last Name"),
        numericInput(ns("age_roughly"), value = 25, label = "Your Age (roughly)")
      )
    })

    observeEvent(input$reg_save, {
      message("Thanks for saving!")
      message(sprintf("first_name=%s, last_name=%s, age=%d\n", input$first_name, input$last_name, input$age_roughly))
    })

  })
}

## To be copied in the UI
# mod_registration_ui("registration_ui_1")

## To be copied in the server
# mod_registration_server("registration_ui_1")


#' @description Demo for mod_registration
#' @noRd
#'
demo_reg <- function() {
  ui <- fluidPage(mod_registration_ui("reg_ui1"))


  server <- function(input, output, session) {
    con <- db_connection()


    f <- firebase_setup(con)
    user <- UserObject(con, firebase_obj = f)

    mod_registration_server("reg_ui1", user)

  }
  shinyApp(ui, server)
}
