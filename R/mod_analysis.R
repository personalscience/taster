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
          choices = db_user_df() %>% pull(user_id),
          selected = 1234
        ),
        markdown("White boxes represent 50% of the iAUC results from all users of that product.

                 The line in the middle of each white box is the median iAUC"
        ),
        actionButton(ns("calc"), label = "Calculate"),
      ),

      mainPanel(

        plotOutput(ns("boxplot"), height="800px"),
        h3("All Meals"),
        dataTableOutput(ns("food_summary"))
      )
    )

  )
}

#' analysis Server Functions
#' @param con database connection
#' @param GLUCOSE_RECORDS valid glucose df
#' @param NOTES_RECORDS valid notes df
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dbl
#' @noRd
mod_analysis_server <- function(id, glucose_df, con, GLUCOSE_RECORDS, NOTES_RECORDS){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

      foods_to_analyze <- db_food_list(c(1000:1009,1013:1502))

    auc_df <- reactive({
      validate(
        need(input$calc,"Press Calculate to see Analytics \n May take up to 30 seconds \n Please be patient!")
      )

      cgmr::df_for_all_auc(food_list = foods_to_analyze,
                            glucose_records = GLUCOSE_RECORDS,
                            notes_records = NOTES_RECORDS)
    }
    )


    AUC_for_food <- function(foodname) {
      cgmr::auc_for_food(foodname, glucose_records = GLUCOSE_RECORDS,
                         notes_records = NOTES_RECORDS, start_limit = 200)
    }

    all_foods <- reactive({
      validate(
        need(input$calc,"Press Calculate to see Analytics")
      )
      tibble(foodname = foods_to_analyze,
                        iAUC = purrr::map_dbl(foods_to_analyze, function(x) {AUC_for_food(x) %>% pull(iAUC) %>% mean()}),
                        n = purrr::map_dbl(foods_to_analyze, function(x) {AUC_for_food(x) %>% nrow()}))
    })

    output$boxplot <- renderPlot({

      validate(
        need(input$calc,"Press Calculate to see Analytics \n May take up to 30 seconds \n Please be patient!")
      )


        auc_df() %>% tidyr::pivot_longer(cols=ave:iAUC, names_to = "param") %>%

        filter(param == "iAUC") %>%
        ggplot(aes(x=foodname, y=value)) +
        geom_boxplot()+
        # geom_point( aes(x=foodname,y=value), color = "red") +
        geom_point( data = auc_df() %>% filter(user_id == input$user_id), aes(x=foodname, y = iAUC), color = "blue", size = 4) +
        #geom_facet(rows=vars(user_id)) +
        labs(title = "Tastermonial Product Test Results", x = "", y = "iAUC Among All Testers") +
        psi_theme()})

    output$food_summary <- renderDataTable({all_foods() %>% arrange(iAUC)})
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
  server <- function(input, output, session) {
    con <- db_connection()

    GLUCOSE_RECORDS<- db_get_table(con, "glucose_records")
    NOTES_RECORDS <- db_get_table(con, "notes_records")
    mod_analysis_server("analysis_ui_1", glucose_df = sample_glucose, con = con, GLUCOSE_RECORDS, NOTES_RECORDS)

  }
  shinyApp(ui, server)
}
