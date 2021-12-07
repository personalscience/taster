#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic

  # Set up objects that are important throughout
  con <- db_connection()
  cgm_data <- CgmObject(con)
  f <- firebase_setup(con)
  user <- UserObject(con, firebase_obj = f)


  mod_about_server("about_ui_1", con, user)

  c_list <- mod_upload_server("fromCSV", con)
  glucose_df <- c_list$glucose_records
  observe(message(sprintf("glucose_df rows = %d", nrow(glucose_df()))))

  mod_metadata_server("metadata", cgm_data, user)

  mod_food_compare_server("food_compare_plot", cgm_data)
  mod_user_view_server("user_view_ui1", f, csv_user_gdf = glucose_df(), cgm_data)

  mod_goddess_server("food2_compare_plot", f, cgm_data)

  mod_filter_results_server("filter_results_ui_1", glucose_df(), con, firebase_obj = f)
  mod_analysis_server("analysis_ui_1", glucose_df(), cgm_data)
  mod_plot_glucose_server("plot_glucose_ui_1", glucose_df, con)

  onStop(function(){message("gracefully exiting...")
    DBI::dbDisconnect(con)})
}
