#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Personal Science Experiments", windowTitle = "Personal Science, Inc."),
    tags$a(href="https://personalscience.com", "More details"),
    textOutput(ns("about_page")),
    textOutput(ns("currentDB")),
    hr(),
    markdown("Here's what we know about you:

             1. You are one of our users
             2. You are important.

             We want to make your experience as easy as possible. You can view the site anonymously, but more features
             are available if create an account.

             Choose one of the following log-in methods to log in (or out). If this is your first time, a new
             account will be created for you."),
    firebase::useFirebase(),
    firebase::firebaseUIContainer(),
    firebase::reqSignin(actionButton(ns("signout"), "Sign out")),
    uiOutput(ns("image"))
  )
}

#' about Server Functions
#' @param con database connection
#' @param f firebase instance
#' @importFrom stats na.omit
#' @noRd
mod_about_server <- function(id, con, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    output$about_page <- renderText("Be Your Own Scientist")
    output$currentDB <- renderText(sprintf("DB=%s. version = %s, cgmr = %s, db  = %s",
                                           substr(get_golem_config("dataconnection")$host, 1,5),
                                           utils::packageVersion("tastermonial"),
                                           utils::packageVersion("cgmr"),
                                           first(db_get_table(con, "glucose_records") %>%
                                                   filter(time == max(time, na.rm=TRUE)) %>%
                                                   pull(time) %>%
                                                   lubridate::with_tz(tzone="America/Los_Angeles"))))


    mod_registration_server("reg_page", user)

    output$image <- renderUI({
      user$f$req_sign_in() # require sign in
      current_user <- user$f$get_signed_in()
      message(sprintf("signing in: %s\n", user$full_name))

      tagList(
      h4("Welcome,", current_user$response$email),
      hr(),
      p(sprintf("Your Firebase ID = %s and Taster ID = %s",
                 current_user$response$uid,
                 db_user_id_from_firebase(con,current_user$response$uid))),
      # experiments <- if(DBI::dbExistsTable(con, "experiments"))
      #   {tbl(con, "experiments") %>% collect()}
      # else NULL
      #
      # tags$img(src=stats::na.omit(experiments$experiment_image_url),
      #      width = "240px")

      mod_registration_ui(ns("reg_page"))
      )
    }
    )

    observeEvent(input$signout, {
      message("signing out")
      user$f$sign_out()
      message("signed out")

    })
  })
}

## To be copied in the UI
# mod_about_ui("about_ui_1")

## To be copied in the server
# mod_about_server("about_ui_1")

demo_about <- function() {
  con <- db_connection()

  ui <- fluidPage(mod_about_ui("x"))
  server <- function(input, output, session) {
    f <- firebase_setup()
    user <- UserObject(con, firebase_obj = f)
    mod_about_server("x",con, user)

  }
  shinyApp(ui, server)
}
