#' filter_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_results_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarLayout(

      sidebarPanel(
    dateInput(ns("start_date"), label = "Start Date", value = lubridate::as_datetime("2021-06-17", tz = Sys.timezone() )),
    sliderInput(ns("start_hour"), label = "Start Time (Hour)", value = 12, min = 0, max = 23),
    sliderInput(ns("time_length"), label = "Time length (Min)", value = 120, min = 10, max = 480, step = 30),
    checkboxInput(ns("zoom_to_date"), label = "Zoom Day", value = FALSE),
    checkboxInput(ns("chk_sleep"), label = "Sleep", value = FALSE),
    textOutput(ns("show_food"))
      ),

    mainPanel(
    plotOutput(ns("glucose_plot"))
    )
  )
  )
}

#' filter_results Server Functions
#'
#' @noRd
mod_filter_results_server <- function(id, glucose_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    s_datetime<- reactive(lubridate::force_tz(input$start_date,
                                    tzone=Sys.timezone()) +
                             lubridate::hours(input$start_hour))


    glucose_new <- reactive({

      if(input$zoom_to_date) {

        glucose_df %>%
          filter(time >= s_datetime()) %>%
          filter(time <= s_datetime() + lubridate::minutes(input$time_length))
      } else  glucose_df %>% filter(.data[["time"]] >= s_datetime())
    }
    )

    output$glucose_plot <- renderPlot({
      cat(file=stderr(), sprintf("rendering glucose_df...%d rows", nrow(glucose_new())))
      plot_glucose(glucose_new())
    })

    return(glucose_new)
  })
}

## To be copied in the UI
# mod_filter_results_ui("filter_results_ui_1")

## To be copied in the server
# mod_filter_results_server("filter_results_ui_1")

#' @description Demo for mod_filter
#' @noRd
#'
demo_filter <- function() {
  ui <- fluidPage(mod_filter_results_ui("x"))
  sample_glucose <- cgmr::glucose_df_from_libreview_csv()
  server <- function(input, output, session) {
    mod_filter_results_server("x", sample_glucose)

  }
  shinyApp(ui, server)
}
