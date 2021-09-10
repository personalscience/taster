# Shiny module to compare foods



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
        label = "Select Food",
        choices = taster_foods,
        selected = "MOON CHEESE WHITE CHEDDA BLACK PEPPA I OZ OR OZ BAGS"
      ),
      checkboxInput(ns("normalize"), label = "Normalize"),
      actionButton(ns("show_raw"), label = "Show Data and Stats"),
      downloadButton(ns("downloadFood_df"), label = "Download Results"),
      hr(),
      checkboxGroupInput(ns("meal_items"),label = "Meal", choices = NULL)
    ),
    mainPanel(plotOutput(ns("main_plot")),
              h3("Raw Data"),
              dataTableOutput(ns("raw_data_table")),
              hr(),
              h3("Statistics"),
              dataTableOutput(ns("auc_table")))
  )
}

#' @title Plot glucose reaction for all foods that match an input text.
#' @description
#' Given a (reactive) libreview dataframe, this Shiny module will
#' generate a valid ggplot object and display it in an accompanying UI
#' @param id shiny module id
#' @param title a title for the plot
#' @return ggplot object representing a glucose chart
#' @export
mod_foodTasterServer <- function(id, title = "Name") {

  moduleServer(id, function(input, output, session) {


    food_df <- reactive({
      validate(
        need(input$food_name, "No food selected")
      )

      one_food_df <-  food_times_df(
        user_id = NULL,
        timeLength = 150,
        prefixLength = 20,
        foodname = input$food_name
      )

      validate(
        need(!is.null(one_food_df), sprintf("No glucose results for food %s", input$food_name1))
      )

      df <- if(input$normalize) {
        one_food_df %>% normalize_value()}
      else one_food_df

      df
    }
    )
    #foodname <- input$food_name
    output$main_plot <- renderPlot({

      validate(
        need(input$food_name, "Waiting on database..."),
        need(!is.null(food_df()), "Problem with food times")
      )
      observe(
        cat(file = stderr(), sprintf("render plot for food=%s \n",
                                     isolate(input$food_name)))
      )



    g <- food_df() %>% ggplot(aes(x=t,y=value,color=date_ch)) + geom_line(size=2)

    g + psi_theme +
      geom_rect(aes(xmin=0,
                    xmax=120, #max(Date),
                    ymin=-Inf,
                    ymax=Inf),
                color = "lightgrey",
                alpha=0.005) +
      labs(title = "Glucose Response", subtitle = str_to_title(isolate(input$food_name)))

    })

    observeEvent(input$show_raw, {
      updateActionButton(inputId = "show_raw", label = "Hide Raw Data and Stats")
    })

    observeEvent(input$food_name,{
      updateCheckboxGroupInput(inputId = "meal_items",
                               label = "Select Meals",
                               choices = food_df() %>% distinct(meal) %>% pull(meal))
    })


    output$raw_data_table <- renderDataTable({

      validate(
        need(input$show_raw, "Press Show Data and Stats")
      )
      food_df()

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
      validate(
        need(input$show_raw, "Press Show Data and Stats")
      )
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


  ui <- fluidPage(mod_foodTasterUI("x"))
  server <- function(input, output, session) {
    mod_foodTasterServer("x", reactiveVal("Username"))
  }
  shinyApp(ui, server)

}

if(interactive()) demo_food()
