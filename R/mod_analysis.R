#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("user_id"),
          label = "User Name",
          choices = user_df_from_db() %>% pull(user_id),
          selected = 1234
        ),
        markdown("White boxes represent 50% of the iAUC results from all users of that product.

                 The line in the middle of each white box is the median iAUC"
        ),
      ),

      mainPanel(

        plotOutput(ns("boxplot"), height="800px"),
        h3("All Meals"),
        tableOutput(ns("food_summary"))
      )
    )

  )
}

#' analysis Server Functions
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dbl
#' @noRd
mod_analysis_server <- function(id, glucose_df, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    GLUCOSE_RECORDS <- tbl(con, "glucose_records") %>% collect()
    NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

    auc_df <- build_all_AUC(s_list = food_list_db(),
                            glucose_records = GLUCOSE_RECORDS,
                            notes_records = NOTES_RECORDS)


    AUC_for_food <- function(foodname) {
      cgmr::auc_for_food(foodname, glucose_records = GLUCOSE_RECORDS,
                         notes_records = NOTES_RECORDS)
    }

    all_foods <- tibble(foodname = food_list_db() ,
                        iAUC = purrr::map_dbl(food_list_db(), function(x) {AUC_for_food(x) %>% pull(iAUC) %>% mean()}),
                        n = purrr::map_dbl(food_list_db(), function(x) {AUC_for_food(x) %>% nrow()}))

    output$boxplot <- renderPlot({auc_df %>% tidyr::pivot_longer(cols=ave:iAUC, names_to = "param") %>%

        filter(param == "iAUC") %>%
        ggplot(aes(x=foodname, y=value)) +
        geom_boxplot()+
        # geom_point( aes(x=foodname,y=value), color = "red") +
        geom_point( data = auc_df %>% filter(user_id == input$user_id), aes(x=foodname, y = iAUC), color = "blue", size = 4) +
        #geom_facet(rows=vars(user_id)) +
        labs(title = "Tastermonial Product Test Results", x = "", y = "iAUC Among All Testers") +
        psi_theme()})

    output$food_summary <- renderTable({all_foods})
  })
}

## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")

## To be copied in the server
# mod_analysis_server("analysis_ui_1")

#' @description Demo for mod_filter
#' @noRd
#'
demo_analysis <- function() {
  ui <- fluidPage(mod_analysis_ui("analysis_ui_1"))
  sample_glucose <- cgmr::glucose_df_from_libreview_csv()
  con <- db_connection()
  server <- function(input, output, session) {
    mod_analysis_server("analysis_ui_1", glucose_df = sample_glucose, con = con)

  }
  shinyApp(ui, server)
}
