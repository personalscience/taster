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

    con <- user$con

    current_user <- reactive({
      f_user <- user$f$get_signed_in()
      validate(need(!is.null(f_user), "Not signed in"))
      this_user <- user
      this_user$user_id <- db_user_id_from_firebase(con,f_user$response$uid)

      uid <- this_user$user_id

      uname <- tbl(con, "user_list") %>% filter(user_id == uid) %>% select(first_name, last_name) %>% collect()
      message(sprintf("uname = %s\n", uname$first_name))
      if(nrow(uname) > 0) {
        this_user$first_name <- first(uname$first_name)
        this_user$last_name <- first(uname$last_name)
      } else {
        this_user$first_name <- "<Unknown First Name"
        this_user$last_name <- "<Unknown Last Name"
      }


      this_user$user_id <- user$user_id
      message(sprintf("Current_user():  user_id = %s, first=%s\n", this_user$user_id, this_user$first_name))
      return(this_user)

    })

    output$questions <- renderUI({
      this_user <-
      if (is.null(current_user())) {
        list(first_name="<Unknown First Name",
             last_name = "<Unknown Last Name",
             user_id = 0)
      } else current_user()
      message(sprintf("questions: this_user = %s\n", this_user[["full_name"]]))
      tagList(
        textInput(ns("first_name"), value = this_user$first_name, label = "First Name"),
        textInput(ns("last_name"), value = this_user$last_name, label = "Last Name"),
        numericInput(ns("age_roughly"), value = 25, label = "Your Age (roughly)")
      )
    })

    observeEvent(input$reg_save, {
      message("Thanks for saving!")
      message(sprintf("save to %s database: user_id = %d, first_name=%s, last_name=%s, age=%d\n",
                      attributes(con)$class,
                      if(is.null(current_user()$user_id)) "NULL" else current_user()$user_id,
                      input$first_name,
                      input$last_name,
                      input$age_roughly))

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
    user <- UserObject(con, user_id = 1234, firebase_obj = f)

    mod_registration_server("reg_ui1", user)

  }
  shinyApp(ui, server)
}
