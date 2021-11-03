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
      checkboxInput(ns("smooth"), label = "Smooth"),
      actionButton(ns("show_raw"), label = "Show Data and Stats"),
      downloadButton(ns("downloadFood_df"), label = "Download Results"),
      hr(),
      checkboxGroupInput(ns("meal_items"),label = "Meal", choices = NULL),
      hr(),
      textOutput(ns("ranges"))
    ),
    mainPanel(plotOutput(ns("main_plot")),
              h3("Statistics"),
              wellPanel(dataTableOutput(ns("auc_table"))),
              hr(),
              h3("Raw Data"),
              dataTableOutput(ns("raw_data_table"))
    ))

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
    conn_args <-  config::get("dataconnection")
    con <- DBI::dbConnect(
      drv = conn_args$driver,
      user = conn_args$user,
      host = conn_args$host,
      port = conn_args$port,
      dbname = conn_args$dbname,
      password = conn_args$password)


    # food_df ----
    food_df <- reactive({
      validate(
        need(input$food_name, "No food selected")
      )

      one_food_df <-  food_times_df_fast(
        glucose_df = GLUCOSE_RECORDS,
        notes_df = NOTES_RECORDS,
        user_id = NULL,
        timeLength = 150,
        prefixLength = 30,
        foodname = input$food_name
      )

      validate(
        need(!is.null(one_food_df), sprintf("No glucose results for food %s", input$food_name1))
      )

      df <- one_food_df

      df
    }
    )

    # output$ranges ----
    output$ranges <- renderText(paste0(map_chr(food_df()$user_id %>% unique(),
                                    function(x) {sprintf("%s =(%s)", username_for_id(x), glucose_range_for_id(x) %>% paste0(collapse=","))
                                    }
    )
    ) %>% paste0(collapse = "\n"))


    # meals_all ----
    meals_all <- reactive({
      one_food_df <- food_df()
      validate(
        need(!is.null(one_food_df), sprintf("No glucose results for food %s", input$food_name1))
      )
      one_food_df %>% distinct(meal) %>% pull(meal)}
    )

    # output$main_plot  ----
    output$main_plot <- renderPlot({

      validate(
        need(input$food_name, "Waiting on database..."),
        need(!is.null(food_df()), "Problem with food times")
      )
      observe(
        {cat(file = stderr(), sprintf("render plot for food=%s \n",
                                     isolate(input$food_name)))
        message(sprintf("currently selected choices:%s", input$meal_items))}
      )

      food_df <-  if(input$normalize) {food_df() %>% normalize_value()}
      else food_df()

# foods_to_show ----
    foods_to_show <- food_df %>%
      filter(meal %in% input$meal_items)

    validate(
      need(nrow(foods_to_show)>0, "Please select a meal")
    )

    g <- foods_to_show %>%
      filter(meal %in% input$meal_items) %>%
      ggplot(aes(x=t,y=value,color=date_ch))  +
      if(input$smooth) geom_smooth(method = "loess", aes(fill=date_ch)) else geom_line(size=2)

    g + psi_theme +
      geom_rect(aes(xmin=0,
                    xmax=120, #max(Date),
                    ymin=-Inf,
                    ymax=Inf),
                color = "lightgrey",
                alpha=0.005) +
      labs(title = "Glucose Response", subtitle = str_to_title(isolate(input$food_name)),
           x = "minutes", y = "")

    })

    # observeEvent(input$show_raw, {
    #   updateActionButton(inputId = "show_raw", label = "Hide Raw Data and Stats")
    # })

    observeEvent(input$food_name,{
      validate(
        need(input$food_name, "Waiting on database..."),
        need(!is.null(food_df()), "Problem with food times")
      )
      updateCheckboxGroupInput(inputId = "meal_items",
                               label = "Select Meals",
                               choices = meals_all())
    })

# output$raw_data_table ----
    output$raw_data_table <- renderDataTable({

      validate(
        need(input$show_raw, "Press Show Data and Stats")
      )
      food_df() %>% mutate(timestamp = with_tz(timestamp, tzone = "America/Los_Angeles"))

    })

    output$downloadFood_df <-
      downloadHandler(
        filename = function() {
          sprintf("Food_data-%s-%s.csv", input$food_name, Sys.Date())
        },
        content = function(file) {
          write_csv(food_df(), file)
        })

    # output$auc_table ----
    output$auc_table <- renderDataTable({
      validate(
        need(input$show_raw, "Press Show Data and Stats")
      )

      food_df() %>% distinct() %>%
        filter(t >= -5) %>% # only look at the times after the food was eaten.
        filter(t <= 120) %>% # and only the first 2 hours.
        group_by(meal) %>% arrange(t) %>%
        summarize( iAUC = auc_calc(tibble(time=t,value=value)),
                   auc_total = DescTools::AUC(t,value-first(value)),

          min = min(value),
          max = max(value),
          sd = sd(value),
          rise = last(value) - first(value),
          .groups = 'drop') %>%
        #summarize(auc = sum((lag(value)-value)*(t-lag(t)), na.rm = TRUE)) %>%
        arrange(iAUC)

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
