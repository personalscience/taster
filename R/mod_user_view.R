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
        uiOutput(ns("meal_selection")),
        checkboxInput(ns("smooth"), label = "Smooth"),
        checkboxInput(ns("show_average"), label = "Show Average"),
        checkboxInput(ns("baseline"), label = "Show Baseline"),
        numericInput(ns("prefixLength"), label = "Prefix Minutes", value = 0, width = "30%" ),
        numericInput(ns("timewindow"), label = "Time Window (Minutes)", value = 150, width = "30%"),
        actionButton(ns("show_raw"), label = "Show Raw Data"),
        actionButton(ns("submit_foods"), label = "Calculate Stats"),
        downloadButton(ns("downloadFood_df"), label = "Download Results"),
      ),
      mainPanel(

        plotOutput(ns("glucose_plot")),
        plotOutput(ns("plot_all_foods")),
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
#' @param id Shiny module id
#' @param csv_user_gdf a glucose dataframe (not a reactive)
#' @param con database connection
#' @param GLUCOSE_RECORDS valid glucose df
#' @param NOTES_RECORDS valid notes df
#' @noRd
mod_user_view_server <- function(id, con, f, csv_user_gdf,GLUCOSE_RECORDS, NOTES_RECORDS  ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    con <- db_connection()


    # taster_prod_list ----
    taster_prod_list <- reactive({
      cat(file=stderr(), sprintf("seeking prod list for user %d", ID()[["id"]]))
      foods <- food_list_db(user_id = ID()[["id"]])
      validate(
        need(!is.null(foods),"missing records for user")
      )
      return(foods)}
    )

    # ID() ----
    ID<- reactive( {
      user <- f$get_signed_in()
      if(!is.null(user))
      {cat(file=stderr(),sprintf("\nUser %s is signed in\n",isolate(input$user_id)))
        username <- name_for_user_id(con, f, input$user_id)}
      else {message("\nsigned out")
        username <- "<must sign in to see name>"
      }
      list(id=as.numeric(input$user_id), name = username)}
    )


    output$show_user <- renderText(

      sprintf("\nuser_id = %d, product = %s, range=%s\n", ID()[["id"]],

              input$food_name,
              paste0(glucose_ranges_for_id(ID()[["id"]], GLUCOSE_RECORDS), collapse=" : ")
      )
    )

    # food_start_times ----
    food_start_times <- reactive({
      cat(file=stderr(), sprintf("generating food start times for user %s\n", ID()[["id"]]))
      validate(
        need(!is.null(input$food_name), "waiting for food menu")
      )
      cat(file=stderr(), sprintf("generating notes based on %s\n", input$food_name))
      n_df <- NOTES_RECORDS %>% filter(user_id == ID()[["id"]]) %>% filter(Comment == input$food_name)

      return(n_df)
    })

    # glucose_df ----
    # return a glucose dataframe
    glucose_df <- reactive({
      cat(file=stderr(), sprintf("generating new glucose_df for user %s\n", ID()[["id"]]))
      validate(
        need(!is.null(input$food_name), "waiting for food menu"),
        need(!is.null(input$meal_name), "waiting for meal name")
      )
      meal_datetime <- lubridate::as_datetime(input$meal_name)

      cat(file=stderr(),sprintf("meal_datetime = %s\n", meal_datetime))

      g_df <- GLUCOSE_RECORDS %>% filter(user_id == ID()[["id"]]) %>%
        filter(time >= meal_datetime - lubridate::minutes(input$prefixLength)) %>%
        filter(time <=  meal_datetime + lubridate::minutes(input$timewindow))
      cat(file=stderr(), sprintf("g_df is %d rows\n",nrow(g_df)))

      return(g_df)}
    )

    # food_df ----
    # return all food_times_df() for foodname
    food_df <- reactive({
      validate(
        need(input$food_name, "No food selected")
      )

      message(sprintf("Food selected = %s\n", isolate(input$food_name)))
      one_food_df <-  cgmr::food_times_df_fast(
        glucose_df = GLUCOSE_RECORDS,
        notes_df = NOTES_RECORDS,
        user_id = input$user_id,
        timeLength = 150,
        prefixLength = 30,
        foodname = input$food_name
      )

      validate(
        need(!is.null(one_food_df), sprintf("No glucose results for food %s", input$food_name1))
      )

      df <-  if(input$normalize) {
        cat(file=stderr(), sprintf("normalizing...\n"))
        one_food_df %>% cgmr::normalize_value()
      } else one_food_df

      return(cgmr::combined_food_times_df(df))
    }
    )

    # output$meal_selection ----
    output$meal_selection <- renderUI({
      cat(file=stderr(), paste("trying to render meal selection for ", isolate(input$user_id)))
      # validate(
      #   need(!is.null(food_start_times(), sprintf("No Foods available")))
      # )
      meal_names <- food_start_times()[["Start"]]

      selectizeInput(NS(id,"meal_name"),
                     label = "Meal (Timezone = UTC)",
                     choices = meal_names,
                     selected = first(meal_names)
      )
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

    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",ID()[["id"]]))
    )

    # output$glucose_plot----


      output$glucose_plot <- renderPlot({

        validate(
          need(input$food_name, "Waiting on database...1"),
          need(!is.null(glucose_df()), "Problem with glucose df"),
          need(!is.null(ID()),"No user selected"),
          need(nrow(glucose_df())>0, sprintf("No glucose records found for %s",ID()[["name"]]))
        )
        observe(
          cat(file = stderr(), sprintf("render plot for user_id=%d and food=%s \n",
                                       isolate(ID()[["id"]]),
                                       isolate(input$food_name)))
        )

        cat(file=stderr(), sprintf("rendering glucose_df...%d rows", nrow(glucose_df())))

        g_df <- glucose_df()


       # plot_glucose(glucose_df(), title = sprintf("User %s",user_id=ID()[["name"]]))

        gr <- glucose_ranges_for_id(ID(), GLUCOSE_RECORDS)

        g <- plot_glucose(g_df,
                                  title =  sprintf("User %s",user_id=ID()[["name"]]),
                                  subtitle = sprintf("Food = %s", isolate(input$food_name)))

        g +

          if(input$baseline & !input$show_average){
            annotate("rect",
                     xmin = min(g_df[["time"]]),
                     xmax = max(g_df[["time"]]),
                     ymin = gr$mean - gr$sd*2,
                     ymax = gr$mean + gr$sd*2,
                     fill = "green",
                     alpha = 0.3)
          }
      })




    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",ID()[["id"]]))
    )

    # foods_to_show ----

    foods_to_show <- function()
      foods_to_show <-
        purrr::map_df(food_list_db(ID()[["id"]]), function(x) {
          cgmr::food_times_df(
            glucose_df = GLUCOSE_RECORDS,
            notes_df = NOTES_RECORDS,
            user_id = ID()[["id"]],
            foodname = x
          )
    })


    # output$plot_all_foods ----

    output$plot_all_foods <- renderPlot({

    validate(
      need(nrow(foods_to_show())>0, "Please select a food")
    )

    g <- plot_compare_glucose(foods_to_show(),
                             combine = input$show_average,
                              smooth = input$smooth,
                              title = "Glucose Response",
                              subtitle = sprintf("Food = %s", isolate(input$food_name)),
                             legend_var = "meal"
                             )

    return(g)

    })


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
          sprintf("Food_data-%s-%s.csv", ID()[["id"]], Sys.Date())
        },
        content = function(file) {
          readr::write_csv(foods_to_show(), file)
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
  ui <- fluidPage(    firebase::useFirebase(),
                      firebase::firebaseUIContainer(),
                      mod_user_view_ui("x"))


  sample_glucose <- cgmr::glucose_df_from_libreview_csv()
  server <- function(input, output, session) {
    con <- db_connection()

    GLUCOSE_RECORDS<- db_get_table(con, "glucose_records")
    NOTES_RECORDS <- db_get_table(con, "notes_records")

    f <- firebase_setup(con)
    mod_user_view_server("x", con, f, csv_user_gdf = sample_glucose, GLUCOSE_RECORDS, NOTES_RECORDS)


  }
  shinyApp(ui, server)
}

