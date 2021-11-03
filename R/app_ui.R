#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

      navbarPage("Tastermonial", collapsible = TRUE, inverse = TRUE,

                  theme = bslib::bs_theme(bootswatch = "cerulean"),

                 tabPanel("Compare Experiments",
                          fluidPage(
                            h2("Compare Experiments"),
                            mod_goddess_ui("food2_compare_plot")
                          )),
                 tabPanel("Compare Foods",
                          fluidPage(
                            mod_food_compare_ui("food_compare_plot")
                          )),
                 tabPanel("User View",
                          fluidPage(
                                mod_filter_results_ui("filter_results_ui_1")

                          )),

                 tabPanel("CSV Load",
                          fluidPage(
                            titlePanel("Upload your own Libreview CSV results"),
                            mod_csv_upload_ui("fromCSV")
                          )),
                 tabPanel("About",
                          fluidPage(
                            #Application title
                            mod_about_ui("about_ui_1")
                          )
                 )

      )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tastermonial'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

