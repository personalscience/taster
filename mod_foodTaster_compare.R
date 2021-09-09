# Shiny module to compare foods

source("psi_db_taster_notes.R")
taster_foods <- taster_notes_df %>% distinct(Comment) %>% pull(Comment)


#' @title UI for food-related plots
#' @description
#' Plot a food object
#' @param id Shiny id
#' @export
mod_foodTasterUI <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("food_name"),
        label = "User Name",
        choices = taster_foods,
        selected = "REAL FOOD BAR"
      ),
      actionButton(ns("submit_food"), label = "Submit Food"),
      checkboxInput(ns("normalize"), label = "Normalize"),
      downloadButton(ns("downloadFood_df"), label = "Download Results")
    ),
    mainPanel(plotOutput(ns("libreview")),
              dataTableOutput(ns("auc_table")))
  )
}

#' @title Plot glucose reaction for all foods that match an input text.
#' @description
#' Given a (reactive) libreview dataframe, this Shiny module will
#' generate a valid ggplot object and display it in an accompanying UI
#' @param id shiny module id
#' @param glucose_df reactive for a valid glucose dataframe
#' @param title a title for the plot
#' @return ggplot object representing a glucose chart
#' @export
mod_foodTasterServer <- function(id,  glucose_df, title = "Name") {

  moduleServer(id, function(input, output, session) {


    all_food_df <- reactive(food_times_df(foodname = input$food_name) %>% filter(!is.na(value)))

    food_df <-  reactive(if (input$normalize) {
      all_food_df() %>% group_by(meal) %>% arrange(t) %>% mutate(value = value-first(value)) %>%
        ungroup() %>%  arrange(meal, t)
    } else  all_food_df())

    #foodname <- input$food_name
    output$libreview <- renderPlot({
      input$submit_food
      plot_food_compare(food_times = food_df(),
                        foodname = isolate(input$food_name))
    })

    output$downloadFood_df <-
      downloadHandler(
        filename = function() {
          sprintf("Food_data-%s-%s.csv", input$food_name, Sys.Date())
        },
        content = function(file) {
          write_csv(food_df(), file)
        })

    output$auc_table <- renderDataTable({
      input$submit_food
      food_df() %>% distinct() %>%
        group_by(meal) %>%
        summarize(auc = DescTools::AUC(t,value-first(value)),
                  min = min(value),
                  max = max(value),
                  rise = last(value) - first(value)) %>%
        #summarize(auc = sum((lag(value)-value)*(t-lag(t)), na.rm = TRUE)) %>%
        arrange(auc)

    })
    })

}

demo_food <- function(){

  glucose_df <- glucose_df_from_db(user_id = 1235)
  ui <- fluidPage(mod_foodTasterUI("x"))
  server <- function(input, output, session) {
    mod_foodTasterServer("x", reactive(glucose_df), reactiveVal("Username"))
  }
  shinyApp(ui, server)

}

if(interactive()) demo_food()
