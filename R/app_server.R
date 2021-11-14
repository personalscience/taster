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

  GLUCOSE_RECORDS<- tbl(con,"glucose_records") %>% collect()
  NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

  f <- firebase_setup(con)


  mod_about_server("about_ui_1", con, f)


  glucose_df <- mod_csv_upload_server("fromCSV", con)
  mod_food_compare_server("food_compare_plot", con, GLUCOSE_RECORDS, NOTES_RECORDS)
  mod_user_view_server("user_view_ui1", con, f, csv_user_gdf = glucose_df(), GLUCOSE_RECORDS, NOTES_RECORDS)
  mod_goddess_server("food2_compare_plot", con, GLUCOSE_RECORDS, NOTES_RECORDS)
  mod_filter_results_server("filter_results_ui_1", glucose_df(), con)
  mod_analysis_server("analysis_ui_1", glucose_df(), con, GLUCOSE_RECORDS, NOTES_RECORDS)
  mod_plot_glucose_server("plot_glucose_ui_1", glucose_df, con)
}
