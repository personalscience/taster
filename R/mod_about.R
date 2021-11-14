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
mod_about_server <- function(id, con, f){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$about_page <- renderText("Be Your Own Scientist")
    output$currentDB <- renderText(sprintf("DB=%s. version = %s, cgmr = %s, db  = %s",
                                           substr(get_golem_config("dataconnection")$host, 1,5),
                                           utils::packageVersion("tastermonial"),
                                           utils::packageVersion("cgmr"),
                                           first(tbl(con,"glucose_records") %>%
                                                   filter(time == max(time, na.rm=TRUE)) %>%
                                                   pull(time) %>%
                                                   lubridate::with_tz(tzone="America/Los_Angeles"))))


    output$image <- renderUI({
      f$req_sign_in() # require sign in
      user <- f$get_signed_in()
      print(user)

      h4("Welcome,", user$response$email)
      # experiments <- if(DBI::dbExistsTable(con, "experiments"))
      #   {tbl(con, "experiments") %>% collect()}
      # else NULL
      #
      # tags$img(src=stats::na.omit(experiments$experiment_image_url),
      #      width = "240px")
    }
    )

    observeEvent(input$signout, {
      message("signing out")
      f$sign_out()
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
    f <- firebase::FirebaseUI$
      new("session")$
      set_providers(
        email = TRUE,
        google = TRUE
      )$
      launch()
    mod_about_server("x",con, f)

  }
  shinyApp(ui, server)
}
