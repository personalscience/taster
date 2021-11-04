#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic

  con <- db_connection()

  mod_about_server("about_ui_1", con)


  glucose_df <- mod_csv_upload_server("fromCSV", con)
  mod_food_compare_server("food_compare_plot", con)
  mod_goddess_server("food2_compare_plot", con)
  mod_filter_results_server("filter_results_ui_1", glucose_df(), con)
  mod_analysis_server("analysis_ui_1", glucose_df(), con)
  mod_plot_glucose_server("plot_glucose_ui_1", glucose_df, con)
}
