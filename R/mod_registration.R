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
    actionButton(ns("reg_save"), label = "Save"),
    firebase::useFirebase(),
    firebase::firebaseUIContainer(),
    firebase::reqSignin(actionButton(ns("signout"), "Sign out"))

  )
}

#' registration Server Functions
#' @param user userObject
#' @noRd
mod_registration_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    con <- user$con

    # current_user() ----
    # return what we know about a user with valid firebase credentials
    current_user <- reactive({
      f_user <- user$f$get_signed_in()
      validate(need(!is.null(f_user), "Not signed in"))

      #tbl(con, "accounts_user")

      a_user <- user_find_id(con,
                             user = list(
                               firebase_id = f_user$response$uid)
                             )
      message(sprintf("a_user: user_id=%s, f_id = %s ", a_user$user_id, a_user$firebase_id))

      return(a_user)

    })

    output$questions <- renderUI({
      this_user <-
      if (is.null(current_user())) {
        list(first_name="<Unknown First Name",
             last_name = "<Unknown Last Name",
             user_id = 0)
      } else current_user()
      message(sprintf("questions: this_user = %s\n", this_user))
      tagList(
        textInput(ns("first_name"), value = this_user$first_name, label = "First Name"),
        textInput(ns("last_name"), value = this_user$last_name, label = "Last Name"),
        numericInput(ns("age_roughly"), value = 25, label = "Your Age (roughly)")
      )
    })

    observeEvent(input$reg_save, {
      message("Thanks for saving!")
      this_user <- current_user()
      accounts_firebase_record <- tibble(
        user_id = this_user$user_id,
        firebase_id = this_user$firebase_id,
        created = lubridate::now(),
        modified = lubridate::now()
      )
      accounts_user_record <- tibble(
        user_id = this_user$user_id,
        first_name = input$first_name,
        last_name = input$last_name,
        privilege = "user",
        modified = lubridate::now()
      )
      message(sprintf("Save to accounts_firebase table %s database: %s\n",attributes(con)$class, accounts_firebase_record))
      response <- db_write_table(con, "accounts_firebase", table_df = accounts_firebase_record)
      message(sprintf("response from appendTable: %s\n",response))

      message(sprintf("Save to %s database: %s\n",attributes(con)$class, accounts_user_record))
      # message(sprintf("save to %s database: user_id = %d, first_name=%s, last_name=%s, age=%d\n",
      #                 attributes(con)$class,
      #                 if(is.null(current_user()$user_id)) "NULL" else current_user()$user_id,
      #                 input$first_name,
      #                 input$last_name,
      #                 input$age_roughly))

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
    message(sprintf("demo_reg user is %s and id = %s", user$full_name, user$firebase_id))

    mod_registration_server("reg_ui1", user)

  }
  shinyApp(ui, server)
}
