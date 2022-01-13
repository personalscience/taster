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


            navbarPage(id = "navbarpage",
                  title = tagList(
                    tags$title("Personal Science"),
                    a(
                      alt = "Personal Science",
                      href = "#",
                      img(src = "www/PSILogo300.png",
                          filetype = "image/png",
                          width = "75%"))
                  ),
                  collapsible = TRUE,
                  inverse = TRUE,

                  theme = bslib::bs_theme(bootswatch = "cerulean"),


                 tabPanel("Login",
                          fluidPage(
                            #Application title
                            mod_about_ui("about_ui_1")
                          )),
                 tabPanel("Compare", value = "goddess",
                          fluidPage(
                            h2("Compare Experiments"),
                            mod_goddess_ui("food2_compare_plot")
                          )),
                 tabPanel("Foods", value = "foods",
                          fluidPage(
                            mod_food_compare_ui("food_compare_plot")
                          )),
                 tabPanel("Analysis",
                          fluidPage(
                            mod_analysis_ui("analysis_ui_1")

                          )),


                 navbarMenu("Upload",
                            tabPanel("Upload",
                                     fluidPage(
                                       titlePanel("Upload your own CGM results"),
                                       mod_upload_ui("fromCSV")
                                     )),
                            tabPanel("Metadata",
                                     fluidPage(
                                       mod_metadata_ui("metadata")
                                     )),
                            tabPanel("Filter",
                                     fluidPage(
                                       mod_filter_results_ui("filter_results_ui_1")
                                     )),
                            tabPanel("User View",
                                     fluidPage(
                                       mod_user_view_ui("user_view_ui1")

                                     ))
                            ),

                 tabPanel("Experiments",
                          fluidPage(
                            mod_experiments_ui("experiments_ui_1")

                          ))


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
      app_title = 'Personal Science CGM'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

