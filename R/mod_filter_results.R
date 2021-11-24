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
        uiOutput(ns("user_selection")),
      hr(),
      checkboxGroupInput(ns("meal_items"),label = "Meal", choices = NULL),
      hr(),
      uiOutput(ns("food_selection")),
    dateInput(ns("start_date"), label = "Start Date", value = lubridate::as_datetime("2021-06-17", tz = Sys.timezone() )),
    sliderInput(ns("start_hour"), label = "Start Time (Hour)", value = 12, min = 0, max = 23),
    sliderInput(ns("time_length"), label = "Time length (Min)", value = 120, min = 10, max = 480, step = 30),
    checkboxInput(ns("zoom_to_date"), label = "Zoom Day", value = FALSE),
    checkboxInput(ns("chk_sleep"), label = "Sleep", value = FALSE),
    actionButton(ns("show_raw"), label = "Show Raw Data"),
    textOutput(ns("show_food")),
    checkboxGroupInput(ns("meal_items"),label = "Meal", choices = NULL)
      ),

    mainPanel(
    plotOutput(ns("glucose_plot")),
    dataTableOutput(ns("raw_data_table")),
    )
  )
  )
}

#' filter_results Server Functions
#'
#' @param con database connection'
#' @param glucose_df a glucose dataframe (not a reactive)
#' @noRd
mod_filter_results_server <- function(id, glucose_df, con, firebase_obj){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    s_datetime<- reactive(lubridate::force_tz(input$start_date,
                                    tzone=Sys.timezone()) +
                             lubridate::hours(input$start_hour))

    filepath<- reactive({

      input$ask_filename})

    # ID() ----
    ID<- reactive( {

      user <- firebase_obj$get_signed_in()
      if(is.null(user)) {
        message("user_id is null")
        user_id <-0
        username <- "<must sign in to see name>"
      }
      else {
        f_id <- db_user_id_from_firebase(user$response$uid)
        user_id <- if(is.na(f_id)) 0 else f_id  # if user isn't registered return user_id = 0

        cat(file=stderr(),sprintf("\nUser %s is signed in\n",user_id))

        username <- db_name_for_user_id(con, f, user_id)
      }


      current_id <- list(id=as.numeric(user_id), name = username)
      message("current ID=",current_id)
      return(current_id)}
    )


    taster_prod_list <- reactive({
      cat(file=stderr(), sprintf("seeking prod list for user %d", ID()[["id"]]))
      db_food_list(con, user_id = ID()[["id"]])}
    )


    # glucose_df_from_user ----
    glucose_user <- reactive({
      input$user_id
    })
    glucose_df_from_user <- reactive({
      if (glucose_user() == 0)
      {
        validate(need(
          nrow(glucose_df) > 0,
          "Please go to the CSV tab and upload a Libreview file."
        ))
        message("user is 0")
        glucose_df
      }
      else {
        db_get_table(con, "glucose_records") %>% filter(user_id == !!glucose_user()) %>% collect()
      }
    })

    # glucose_new ----
    glucose_new <- reactive({

      if(input$zoom_to_date) {

        glucose_df_from_user() %>%
          filter(time >= s_datetime()) %>%
          filter(time <= s_datetime() + lubridate::minutes(input$time_length))
      } else  glucose_df_from_user() %>% filter(.data[["time"]] >= s_datetime())
    }
    )

    # food_times ----
    # generate df of food start times
    food_times <- reactive({
      validate(
        need(input$food_name, "No food selected")
      )

      food_start_times_df <- db_get_table(con, "notes_records") %>% filter(user_id == !!ID()[["id"]] &
                                                               Start >= !!input$start_date &
                                                                 Comment == !!input$food_name) %>%
        collect()

      validate(
        need(!is.null(food_start_times_df), sprintf("No results for food %s", input$food_name))
      )

      observe(sprintf("Found %d examples of food %s", nrow(food_start_times_df), input$food_name))

      food_start_times <- food_start_times_df %>% pull(Start)

      return(food_start_times)
    }
    )

    # output$user_selection ----
    output$user_selection <- renderUI({

      current_user <- ID()[["id"]]
      if(is.null(current_user)) message("user_selection user is null")

      message("Current User=",isolate(ID()[["id"]]))
      visible_users <- db_users_visible(current_user)
      #visible_names <- map_chr(visible_users, function(x) {db_name_for_user_id(con,user_id = x)})

      selectInput(
        ns("user_id"),
        label = "User Name",
        choices = visible_users,
        selected = current_user
      )
    })

    # updateCheckBoxGroupInput input$foodname ----
    observeEvent(input$food_name,{
      validate(
        need(input$food_name, "Waiting on database..."),
        need(!is.null(food_times()), "Problem with food times")
      )
      updateCheckboxGroupInput(inputId = "meal_items",
                               label = "Select Meals",
                               choices = food_times())
    })

    output$glucose_plot <- renderPlot({
      cat(file=stderr(), sprintf("rendering glucose_df...%d rows", nrow(glucose_new())))
      plot_glucose(glucose_new(), subtitle = sprintf("%d values for %s",
                                                     nrow(glucose_df_from_user()),glucose_user()))
    })




    # output$food_selection ----
    output$food_selection <- renderUI({
      validate(
        need(!is.null(taster_prod_list()),sprintf("No foods available for user_id %s",ID()[["id"]]))
      )

      cat(file=stderr(), paste("finding foods for User", isolate(input$user_id)))
      cat(file=stderr(), sprintf("User %s first food is %s",isolate(input$user_id),first(taster_prod_list()) ))
      selectizeInput(NS(id,"food_name"),
                     label = "Food Item",
                     choices = taster_prod_list(),
                     selected = first(taster_prod_list())
      )
    })

    # output$raw_data_table ----
    output$raw_data_table <- renderDataTable({

      validate(
        need(input$show_raw, "Press Show Raw")
      )
      glucose_new() %>%
        mutate(time = lubridate::with_tz(time, tzone = "America/Los_Angeles"))

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

  server <- function(input, output, session) {
    sample_glucose <- cgmr::glucose_df_from_libreview_csv()
    con <- db_connection()
    f <- firebase_setup(con)
    mod_filter_results_server("x", sample_glucose, con = con, firebase_obj = f )

  }
  shinyApp(ui, server)
}
