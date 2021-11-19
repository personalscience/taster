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
        uiOutput(ns("user_selection")),
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
#' @param GLUCOSE_RECORDS_Partial valid glucose df
#' @param NOTES_RECORDS_Partial valid notes df
#' @noRd
mod_user_view_server <- function(id, con, f, csv_user_gdf,GLUCOSE_RECORDS_Partial, NOTES_RECORDS_Partial  ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # GLUCOSE_RECORDS ----
    GLUCOSE_RECORDS <- reactive({
      message("csv records are nrows", nrow(csv_user_gdf))
      GLUCOSE_RECORDS_Partial  ## %>% rbind(csv_user_gdf)
    })

    # NOTES_RECORDS ----
    NOTES_RECORDS <- reactive({
      message("csv records are nrows", nrow(csv_user_gdf))
      extra_notes <- NULL #cgmr::notes_df_from_glucose_table(csv_user_gdf, user_id = 0)
      return(NOTES_RECORDS_Partial  %>% rbind(extra_notes))
    })


    # taster_prod_list ----
    taster_prod_list <- reactive({
      message("taster_prod_list()")
      cat(file=stderr(), sprintf("seeking prod list for user %s\n", isolate(input$user_id)))
      foods <- db_food_list(user_id = input$user_id)
      validate(
        need(!is.null(foods),"missing records for user")
      )
      return(foods)}
    )

    # ID() ----
    ID<- reactive( {

      user <- f$get_signed_in()
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


    output$show_user <- renderText(

      sprintf("\nuser_id = %d, product = %s, range=%s\n", ID()[["id"]],

              input$food_name,
              paste0(glucose_ranges_for_id(ID()[["id"]], GLUCOSE_RECORDS()), collapse=" : ")
      )
    )

    # food_start_times ----
    food_start_times <- reactive({
      cat(file=stderr(), sprintf("generating food start times for user %s\n", input$user_id))
      validate(
        need(!is.null(input$food_name), "waiting for food menu")
      )
      cat(file=stderr(), sprintf("generating notes based on %s\n", input$food_name))
      n_df <- NOTES_RECORDS() %>% filter(user_id == input$user_id) %>% filter(Comment == input$food_name)

      return(n_df)
    })

    # glucose_df ----
    # return a glucose dataframe
    glucose_df <- reactive({
      cat(file=stderr(), sprintf("generating new glucose_df for user %s\n",input$user_id))
      validate(
        need(!is.null(input$food_name), "waiting for food menu"),
        need(!is.null(input$meal_name), "waiting for meal name")
      )
      meal_datetime <- lubridate::as_datetime(input$meal_name)

      cat(file=stderr(),sprintf("meal_datetime = %s\n", meal_datetime))

      g_df <- GLUCOSE_RECORDS() %>% filter(user_id == input$user_id) %>%
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
        glucose_df = GLUCOSE_RECORDS(),
        notes_df = NOTES_RECORDS(),
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

    # output$meal_selection ----
    output$meal_selection <- renderUI({

      meal_names <- food_start_times()[["Start"]]

      selectizeInput(NS(id,"meal_name"),
                     label = "Meal (Timezone = UTC)",
                     choices = meal_names,
                     selected = first(meal_names)
      )
    })

    # output$food_selection ----
    output$food_selection <- renderUI({

      food_choices <- taster_prod_list()
      validate(
        need(!is.null(food_choices),sprintf("No foods available for user_id %s",ID()[["id"]]))
      )

      cat(file=stderr(), sprintf("finding foods for User %s\n", ID()[["id"]]))
      cat(file=stderr(), sprintf("User %s first food is %s\n",isolate(ID()[["id"]]),first(food_choices) ))
      selectizeInput(NS(id,"food_name"),
                     label = "Food Item",
                     choices = food_choices,
                     selected = first(food_choices)
      )
    })

    # output$glucose_plot----


      output$glucose_plot <- renderPlot({

        validate(
          need(input$food_name, "Waiting on database...1"),
          need(!is.null(glucose_df()), "Problem with glucose df"),
          need(!is.null(ID()),"No user selected"),
          need(nrow(glucose_df())>0, sprintf("No glucose records found for %s",input$user_id))
        )
        observe(
          cat(file = stderr(), sprintf("render plot for user_id=%d and food=%s \n",
                                       isolate(ID()[["id"]]),
                                       isolate(input$food_name)))
        )

        cat(file=stderr(), sprintf("rendering glucose_df...%d rows", nrow(glucose_df())))

        g_df <- glucose_df()


       # plot_glucose(glucose_df(), title = sprintf("User %s",user_id=ID()[["name"]]))

        gr <- glucose_ranges_for_id(input$user_id, GLUCOSE_RECORDS())

        g <- plot_glucose(g_df,
                                  title =  sprintf("User %s",user_id=input$user_id),
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





    # foods_to_show ----

    foods_to_show <- reactive({
      message("foods_to_show for user=",
              if(is.null(isolate(input$user_id)))
                "-null-"
              else isolate(input$user_id))
      validate(
        need(!is.null(input$user_id), "Waiting for user selection")
      )
      food_list <- db_food_list(input$user_id)

        purrr::map_df(food_list, function(x) {
          cgmr::food_times_df(
            glucose_df = GLUCOSE_RECORDS(),
            notes_df = NOTES_RECORDS(),
            user_id = input$user_id,
            foodname = x
          )})
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


    observe(
      if(is.null(isolate(input$user_id)))
         message("observer user_id = NULL") else
      cat(file = stderr(), sprintf("Observe user_id=%s \n",ID()[["id"]]))
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


  sample_glucose <- cgmr::libreview_csv_df()[["glucose_raw"]] %>%
    filter(timestamp>"2021-06-01") %>%     transmute(`time` = `timestamp`,
                                                     scan = glucose_scan,
                                                     hist = glucose_historic,
                                                     strip = strip_glucose,
                                                     value = hist,
                                                     food = notes,
                                                     user_id = 0)

  server <- function(input, output, session) {
    con <- db_connection()

    GLUCOSE_RECORDS<- db_get_table(con, "glucose_records") #%>% bind_rows(sample_glucose)
    NOTES_RECORDS <- db_get_table(con, "notes_records")

    f <- firebase_setup(con)
    mod_user_view_server("x", con, f, csv_user_gdf = sample_glucose, GLUCOSE_RECORDS, NOTES_RECORDS)


  }
  shinyApp(ui, server)
}

