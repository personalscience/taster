#' goddess UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr pull %>%
mod_goddess_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("user_selection")),
        uiOutput(ns("food_selection")),
        uiOutput(ns("food_selection2")),
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

        plotOutput(ns("food1")),
        plotOutput(ns("food2")),
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

#' goddess Server Functions
#' @param id id
#' @param f firebase object
#' @param cgm_data CgmObject
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @noRd
mod_goddess_server <- function(id, f = firebase_obj, cgm_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    con <- cgm_data$con


    # current_user() ----
    current_user<- reactive( {

      user <- f$get_signed_in()
      if(is.null(user)) {
        message("user_id is null")
        user_id <- input$user_id
        username <- "<must sign in to see name>"
      }
      else {
        f_id <- db_user_id_from_firebase(con, user$response$uid)
        user_id <- if(is.na(f_id)) 0 else f_id  # if user isn't registered return user_id = 0

        cat(file=stderr(),sprintf("\nUser %s is signed in\n",user_id))

        username <- db_name_for_user_id(con, f, user_id)
      }


      current_id <- list(id=if(is.null(user_id)) 0 else as.numeric(user_id), name = username)
      message("current ID=",current_id)
      return(current_id)}
    )

    # taster_prod_list() ----
    taster_prod_list <- reactive({
      cat(file=stderr(), sprintf("seeking prod list for user %s", input$user_id))
      foods <- db_food_list(con, user_id = input$user_id)
      validate(
        need(!is.null(foods),"missing records for user")
      )
      return(foods)}
    )


    # show_user (ID, range, etc.)----
    output$show_user <- renderText(

      sprintf("user_id = %s, product = %s, range=%s", input$user_id,

              input$food_name1,
              paste0(glucose_ranges_for_id(input$user_id, cgm_data$glucose_records), collapse=" : ")
      )
    )

    # food_df2() ----
    food_df2 <- reactive({
      validate(
        need(!is.null(taster_prod_list()),"No food times available for this person"),
        need(!is.null(input$user_id), "No user selected"),
        need(input$food_name1, "No food selected")
      )

      one_food_df <-  cgmr::food_times_df_fast(
        glucose_df = cgm_data$glucose_records,
        notes_df = cgm_data$notes_records,
        user_id = input$user_id,
        timeLength = input$timewindow,
        prefixLength = input$prefixLength,
        foodname = input$food_name2
      )

      validate(
        need(!is.null(one_food_df), sprintf("No glucose results for food %s", input$food_name1))
      )


      df <- one_food_df

      df
    }
    )

    # y_scale ----

    y_scale <- reactive({
      foods_all <-bind_rows(food_df2(),food_df())
      foods <- if(input$normalize) {foods_all %>% cgmr::normalize_value()}
      else foods_all
   #   cat(file=stderr(), sprintf("Y Scale Max = %d, Min = %d",  min(foods$value), max(foods$value)))
      list(max = max(foods$value),
           min = min(foods$value))

    })

    # food_df ----
    food_df <- reactive({
      validate(
        need(!is.null(taster_prod_list()), "No food times available for this person"),
        need(!is.null(input$user_id), "No user selected"),
        need(input$food_name1, "No food selected")
      )

      one_food_df <-  cgmr::food_times_df_fast(
        glucose_df = cgm_data$glucose_records,
        notes_df = cgm_data$notes_records,
        user_id = input$user_id,
        timeLength = input$timewindow,
        prefixLength = input$prefixLength,
        foodname = input$food_name1
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


      current_user <- current_user()
      if(is.null(current_user))
        { message("user_selection user is null but I'll change that")
        current_user <- list(id = 0, name = "<sign in please")
      }

      message("Current User=",isolate(current_user))
      visible_users <- db_users_visible(con, current_user)
      #visible_names <- map_chr(visible_users, function(x) {db_name_for_user_id(con,user_id = x)})

      selectInput(
        ns("user_id"),
        label = "User Name",
        choices = visible_users,
        selected = current_user
      )
    })

    # output$food_selection ----
    output$food_selection <- renderUI({
      validate(
        need(!is.null(taster_prod_list()),sprintf("No foods available for user_id %s",input$user_id))
      )

      cat(file=stderr(), paste("finding foods for User", isolate(input$user_id)))
      cat(file=stderr(), sprintf("User %s first food is %s",isolate(input$user_id),first(taster_prod_list()) ))
      selectizeInput(NS(id,"food_name1"),
                     label = "Food Item",
                     choices = taster_prod_list(),
                     selected = first(taster_prod_list())
      )
    })

    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",input$user_id))
    )

    # output$food1 Render Plot----
    output$food1 <- renderPlot({

      validate(
        need(input$food_name1, "Waiting on database...1"),
        need(!is.null(food_df()), "Problem with food times"),
        need(!is.null(input$user_id),"No user selected")
      )
      observe(
        cat(file = stderr(), sprintf("render plot for user_id=%s and food=%s \n",
                                     isolate(input$user_id),
                                     isolate(input$food_name1)))
      )



      food_df <-  if(input$normalize) {food_df() %>% cgmr::normalize_value()}
      else food_df()


      gr <- glucose_ranges_for_id(input$user_id, cgm_data$glucose_records)

      g <- plot_compare_glucose(food_df,
                                combine = FALSE, #input$combine,
                                smooth = input$smooth,
                                title = "Glucose Response",
                                subtitle = sprintf("Food = %s", isolate(input$food_name1)))

      g +
      coord_cartesian(ylim = c(y_scale()[["min"]], y_scale()[["max"]])) +
        if(input$baseline & !input$normalize){
          annotate("rect",
                   xmin = -Inf,
                   xmax = Inf,
                   ymin = gr$mean - gr$sd*2,
                   ymax = gr$mean + gr$sd*2,
                    fill = "green",
                    alpha = 0.3)
        }
    })

    # output$food_selection2 ----
    output$food_selection2 <- renderUI({
      validate(
        need(!is.null(taster_prod_list()),sprintf("No foods available for user_id %s",input$user_id))
      )

      cat(file=stderr(), paste("finding foods for User", isolate(input$user_id)))
      cat(file=stderr(), sprintf("User %s first food is %s",isolate(input$user_id),first(taster_prod_list()) ))
      selectizeInput(NS(id,"food_name2"),
                     label = "Food Item",
                     choices = taster_prod_list(),
                     selected = first(taster_prod_list())
      )
    })

    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",input$user_id))
    )

    # output$food2 Render Plot ----
    output$food2 <- renderPlot({

      validate(
        need(input$food_name1, "Waiting on database...1"),
        need(!is.null(food_df2()), "Problem with food times"),
        need(!is.null(input$user_id),"No user selected")
      )
      observe(
        cat(file = stderr(), sprintf("render plot for user_id=%s and food=%s \n",
                                     isolate(input$user_id),
                                     isolate(input$food_name2)))
      )



      food_df <-  if(input$normalize) {food_df2() %>% cgmr::normalize_value()}
      else food_df2()

      gr <- glucose_ranges_for_id(input$user_id, cgm_data$glucose_records)

      g <- plot_compare_glucose(food_df,
                                combine = FALSE, #input$combine,
                                smooth = input$smooth,
                                title = "Glucose Response",
                                subtitle = sprintf("Food = %s", isolate(input$food_name2)))


      g +
        coord_cartesian(ylim = c(y_scale()[["min"]], y_scale()[["max"]])) +
        if(input$baseline & !input$normalize){
          annotate("rect",
                   xmin = -Inf,
                   xmax = Inf,
                   ymin = gr$mean - gr$sd*2,
                   ymax = gr$mean + gr$sd*2,
                   fill = "green",
                   alpha = 0.3)
        }
    })

    # output$auc_table ----
    output$auc_table <- renderDataTable({
      input$submit_foods
      validate(
        need(input$submit_foods, "Press Calculate Stats")
      )
      bind_rows(food_df(),food_df2()) %>% distinct() %>%
        filter(t >= -5) %>% # only look at the times after the food was eaten.
        filter(t <= 120) %>% # and only the first 2 hours.
        group_by(meal) %>% arrange(t) %>%
        summarize(

          iAUC = cgmr::auc_calc(tibble(time=t,value=value)),
          auc_total = DescTools::AUC(t,value-first(value)),

          min = min(value),
          max = max(value),
          sd = sd(value),
          rise = last(value) - first(value),
          .groups = 'drop') %>%
        #summarize(auc = sum((lag(value)-value)*(t-lag(t)), na.rm = TRUE)) %>%
        arrange(iAUC)

    })

    # output$raw_data_table ----
    output$raw_data_table <- renderDataTable({

      validate(
        need(input$show_raw, "Press Show Raw")
      )
      bind_rows(food_df(),food_df2()) %>%
        mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Los_Angeles")) %>%
        distinct() %>% arrange(meal)

    })

    # output$downloadFood_df ----
    output$downloadFood_df <-
      downloadHandler(
        filename = function() {
          sprintf("Food_data-%s-%s.csv", input$user_id, Sys.Date())
        },
        content = function(file) {
          readr::write_csv(bind_rows(food_df(),food_df2()) %>% distinct() %>% arrange(meal), file)
        }
      )


  })
}

## To be copied in the UI
# mod_goddess_ui("goddess_ui_1")

## To be copied in the server
# mod_goddess_server("goddess_ui_1")

#' @description Demo for mod_food_compare
#' @noRd
#'
demo_goddess <- function() {
  ui <- fluidPage(firebase::useFirebase(),
                  firebase::firebaseUIContainer(),
                  mod_goddess_ui("x"))

  sample_glucose <- cgmr::glucose_df_from_libreview_csv()



  server <- function(input, output, session) {
    cgm_data <- CgmObject(db_connection())

    f <- firebase_setup(cgm_data$con)
    mod_goddess_server("x", f, cgm_data)

  }
  shinyApp(ui, server)
}



