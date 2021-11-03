#' user_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_view_ui <- function(id){
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
        uiOutput(ns("food_selection")),
        checkboxInput(ns("normalize"), label = "Normalize"),
        checkboxInput(ns("smooth"), label = "Smooth"),
        checkboxInput(ns("baseline"), label = "Show Baseline"),
        numericInput(ns("prefixLength"), label = "Prefix Minutes", value = 0, width = "30%" ),
        numericInput(ns("timewindow"), label = "Time Window (Minutes)", value = 150, width = "30%"),
        actionButton(ns("show_raw"), label = "Show Raw Data"),
        actionButton(ns("submit_foods"), label = "Calculate Stats"),
        downloadButton(ns("downloadFood_df"), label = "Download Results"),
      ),
      mainPanel(

        plotOutput(ns("glucose_plot")),
        h3("Stats Table"),
        dataTableOutput(ns("auc_table")),
        h3("Raw Data"),
        dataTableOutput(ns("raw_data_table")),
        hr(),
        textOutput(ns('show_user'))
      )
    )

  )
}

#' user_view Server Functions
#'
#' @noRd
mod_user_view_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    con <- db_connection()

    GLUCOSE_RECORDS<- tbl(con,"glucose_records") %>% collect()
    NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

    glucose_ranges_for_id <- function(user_id){

      ID = user_id
      GLUCOSE_RECORDS %>% filter(user_id == ID) %>%

        mutate(time = lubridate::with_tz(time, tzone="America/Los_Angeles")) %>%
        filter(lubridate::hour(time) >=1 & lubridate::hour(time) <=4 & !is.na(value)) %>%
        group_by(date=lubridate::date(time)) %>%
        summarize(mean = mean(value, na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>%
        ungroup() %>%
        select(mean,sd) %>%
        summarize(mean=mean(mean),sd=mean(sd))


    }


    ID<- reactive( {cat(file=stderr(), paste("Selected User", isolate(input$user_id)))
      as.numeric(input$user_id)}
    )
    taster_prod_list <- reactive({
      cat(file=stderr(), sprintf("seeking prod list for user %d", ID()))
      food_list_db(user_id = ID())}
    )

    output$show_user <- renderText(

      sprintf("user_id = %d, product = %s, range=%s", ID(),

              input$food_name,
              paste0(glucose_ranges_for_id(ID()), collapse=" : ")
      )
    )

    # glucose_df ----
    # return a glucose dataframe
    glucose_df <- reactive({
      cat(file=stderr(), sprintf("generating new glucose_df for user %s", ID()))
      cat(file=stderr(), sprintf("generating notes based on %s", input$food_name))
      n_df <- NOTES_RECORDS %>% filter(user_id == ID()) %>% filter(Comment == input$food_name)
      food_start <- n_df %>% pull(Start) %>% last() %>% as_datetime()
      cat(file=stderr(), sprintf("start time = %s", food_start))
      g_df <- GLUCOSE_RECORDS %>% filter(user_id == ID()) %>%
        filter(time > food_start) %>%
        filter(time <= food_start + lubridate::minutes(input$timewindow))
      cat(file=stderr(), sprintf("g_df is %d rows",nrow(g_df)))

      return(g_df)

    }

    )

    # output$food_selection ----
    output$food_selection <- renderUI({
      validate(
        need(!is.null(taster_prod_list()),sprintf("No foods available for user_id %s",ID()))
      )

      cat(file=stderr(), paste("finding foods for User", isolate(input$user_id)))
      cat(file=stderr(), sprintf("User %s first food is %s",isolate(input$user_id),first(taster_prod_list()) ))
      selectizeInput(NS(id,"food_name"),
                     label = "Food Item",
                     choices = taster_prod_list(),
                     selected = first(taster_prod_list())
      )
    })

    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",ID()))
    )

    # output$glucose_plot----


      output$glucose_plot <- renderPlot({

        validate(
          need(input$food_name, "Waiting on database...1"),
          need(!is.null(glucose_df()), "Problem with glucose df"),
          need(!is.null(ID()),"No user selected")
        )
        observe(
          cat(file = stderr(), sprintf("render plot for user_id=%d and food=%s \n",
                                       isolate(ID()),
                                       isolate(input$food_name)))
        )

        cat(file=stderr(), sprintf("rendering glucose_df...%d rows", nrow(glucose_df())))
        plot_glucose(glucose_df(), title = sprintf("User %s", ID()))
      })




    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",ID()))
    )




    # output$raw_data_table ----
    output$raw_data_table <- renderDataTable({

      validate(
        need(input$show_raw, "Press Show Raw")
      )
      glucose_df() %>%
        mutate(`timestamp PST` = lubridate::with_tz(time, tzone = "America/Los_Angeles")) %>%
        arrange(time)

    })

    output$downloadFood_df <-
      downloadHandler(
        filename = function() {
          sprintf("Food_data-%s-%s.csv", ID(), Sys.Date())
        },
        content = function(file) {
          readr::write_csv(glucose_df(), file)
        }
      )


  })
}

## To be copied in the UI
# mod_user_view_ui("user_view_ui_1")

## To be copied in the server
# mod_user_view_server("user_view_ui_1")

#' @description Demo for mod_food_compare
#' @noRd
#'
demo_user <- function() {
  ui <- fluidPage(mod_user_view_ui("x"))
  sample_glucose <- cgmr::glucose_df_from_libreview_csv()
  server <- function(input, output, session) {
    mod_user_view_server("x")

  }
  shinyApp(ui, server)
}

