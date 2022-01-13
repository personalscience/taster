#' experiments UI Function
#'
#' @description A shiny Module to browse the types of experiments enabled on the platform
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_experiments_ui <- function(id){
  ns <- NS(id)
  tagList(
    #h1("Experiments"),
    includeMarkdown(app_sys("app/www/docs/experiments_instructions.md")),
    hr(),
    DT::dataTableOutput(ns("experiments_table"))

  )
}



#' experiments Server Functions
#'
#' @noRd
mod_experiments_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    wrap_img <- function(url_string, item_name) {
      if(!is.null(url_string)) return(sprintf("<img src = %s width = '150'>%s</img>",url_string,item_name))
      else return(NULL)
    }

    con <- user$con

    et <- db_get_table(con, table_name = "experiments")

    output$experiments_table <- {


      DT::renderDataTable({

        validate(
          need(!is.null(et), "Experiments Table Doesn't Exist")
        )
        experiments_table <-  et %>% select(-id,-experiment_id)
        experiments_table$experiment_image_url <- purrr::map2_chr(experiments_table$experiment_image_url,
                                                                  experiments_table$experiment_name,
                                                                  wrap_img)
        experiments_table},
        escape = FALSE,
        options = list(
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")))
    }
  })
}

## To be copied in the UI
# mod_experiments_ui("experiments_ui_1")

## To be copied in the server
# mod_experiments_server("experiments_ui_1")

#' @description Demo for mod_registration
#' @noRd
#'
demo_experiments <- function() {
  ui <- fluidPage(mod_experiments_ui("experiments_ui_1"))


  server <- function(input, output, session) {
    con <- db_connection()


    f <- firebase_setup(con)
    user <- UserObject(con, user_id = 1234, firebase_obj = f)
    message(sprintf("demo_reg user is %s and id = %s", user$full_name, user$firebase_id))

    mod_experiments_server("experiments_ui_1", user)

  }
  shinyApp(ui, server)
}
