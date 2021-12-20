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
    current_user<- reactive( util_current_user(list(firebase_id = f, con = con)))

    # taster_prod_list() ----
    taster_prod_list <- reactive({
      cat(file=stderr(), sprintf("seeking prod list for user %s\n", input$user_id))
      foods <- db_food_list(con, user_id = input$user_id)
      validate(
        need(!is.null(foods),"missing records for user")
      )
      return(foods)}
    )


    # show_user (ID, range, etc.)----
    output$show_user <- renderText({

      validate(
        need(!is.null(input$user_id), "No Such User")
      )

      sprintf("user_id = %s, product = %s, range=%s", input$user_id,

              input$food_name1,
              paste0(glucose_ranges_for_id(input$user_id,
                                           cgm_data$glucose_records),
                     collapse=" : ")
      )
    }
    )



    # y_scale ----

    y_scale <- reactive({
      validate(
        need(!is.null(input$normalize), "waiting for normalize")
      )
      y_scale_(bind_rows(food_df2(), food_df()), input$normalize)
    }
    )


    # food_df ----

    food_df <- reactive(
      food_df_(cgm_data = cgm_data,
               user_id = input$user_id,
               timewindow = input$timewindow,
               prefixLength = input$prefixLength,
               food_name = input$food_name1,
               normalize = input$normalize
      )
    )

    food_df2 <- reactive(
      food_df_(cgm_data = cgm_data,
               user_id = input$user_id,
               timewindow = input$timewindow,
               prefixLength = input$prefixLength,
               food_name = input$food_name2,
               normalize = input$normalize
      )
    )


    # output$user_selection ----
    output$user_selection <- renderUI({


      current_user <- current_user()
      if(is.null(current_user))
        { message("user_selection user is null but I'll change that")
        current_user <- list(id = 0, name = "<sign in please")
      }

      message("Current User=",isolate(current_user[["id"]]))
      visible_users <- db_users_visible(con, current_user[["id"]])
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
      food_selection(item_id = ns("food_name1"),
                     prod_list = taster_prod_list(),
                     user_id = input$user_id )
    })


    output$food_selection2 <- renderUI({
      food_selection(item_id = ns("food_name2"),
                     prod_list = taster_prod_list(),
                     user_id = input$user_id )
    })

    # output$food Render Plot----

    output$food1 <- renderPlot(
      goddessPlot(user_id = input$user_id,
                        food_times_df = food_df,
                        food_name = input$food_name1,
                        cgm_data = cgm_data,
                        smooth = input$smooth,
                        normalize = input$normalize,
                        baseline = input$baseline)
    )

    output$food2 <- renderPlot(
      goddessPlot(user_id = input$user_id,
                        food_times_df = food_df2,
                        food_name = input$food_name2,
                        cgm_data = cgm_data,
                        smooth = input$smooth,
                        normalize = input$normalize,
                        baseline = input$baseline)
    )


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




#' @title Make Y-Scale Based on All Food Times Values
#' @param foods_all dataframe of `food_times_df`
#' @param normalize logical should return list be normalized
#' @return list
y_scale_ <- function(foods_all, normalize) {

  foods <- if(normalize) {foods_all %>% cgmr::normalize_value()}
  else foods_all

  return(
    list(max = max(foods$value),
         min = min(foods$value))
  )


}

#' @title Food Times Dataframe
#' @param cgm_data cgmObject
#' @param user_id user ID
#' @param timewindow integer minutes to show after start
#' @param prefixLength integer minutes before start
#' @param food_name character string for food name
#' @param normalize logical if output should be normalized
#' @return dataframe of food times
food_df_ <- function(cgm_data,
                     user_id,
                     timewindow,
                     prefixLength,
                     food_name,
                     normalize = FALSE){

    validate(
    #  need(!is.null(taster_prod_list()), "No food times available for this person"),
      need(!is.null(user_id), "No user selected"),
      need(!is.null(food_name), "No food selected")
    )

    one_food_df <-  cgmr::food_times_df_fast(
      glucose_df = cgm_data$glucose_records,
      notes_df = cgm_data$notes_records,
      user_id = user_id,
      timeLength = timewindow,
      prefixLength = prefixLength,
      foodname = food_name
    )

    validate(
      need(!is.null(one_food_df), sprintf("No glucose results for food %s", food_name))
    )

    df <-  if(normalize) {
      cat(file=stderr(), sprintf("normalizing...\n"))
      one_food_df %>% cgmr::normalize_value()
    } else one_food_df

    return(cgmr::combined_food_times_df(df))

}

#' @title Make a Food Selection Object
#' @param item_id character string NS id for UI
#' @param prod_list list of products to choose from
#' @param user_id user ID
#' @return food selection object
food_selection <- function(item_id,
                           prod_list,
                           user_id){
  validate(
    need(!is.null(prod_list),sprintf("No foods available for user_id %s",user_id))
  )

  cat(file=stderr(), sprintf("Food Selection: finding food_name1 for User %s\n", user_id))
  cat(file=stderr(), sprintf("User %s first food is %s\n",user_id,first(prod_list)))
  selectizeInput(item_id,
                 label = "Food Item",
                 choices = prod_list,
                 selected = first(prod_list)
  )

}

#' @title Return a CGM Goddess Plot
#' @param user_id ID
#' @param food_df dataframe of food times
#' @param food_name character string for food name
#' @param cgm_data CgmObject
#' @return ggplot2 object
goddessPlot <- function(user_id ,
                              food_times_df ,
                              food_name,
                              cgm_data,
                              normalize = FALSE,
                              smooth = FALSE,
                              baseline = FALSE) {


    validate(
      need(!is.null(food_name), "Waiting on database...1"),
      need(!is.null(food_times_df()), "Problem with food times"),
      need(!is.null(user_id),"No user selected")
      )
      observe(
        cat(file = stderr(), sprintf("render plot for user_id=%s and food=%s \n",
                                     isolate(user_id),
                                     isolate(food_name)))
      )



      food_df <-  if(normalize) {food_times_df() %>% cgmr::normalize_value()}
      else food_times_df()

      y_scale <- y_scale_(food_times_df(), normalize)
      gr <- glucose_ranges_for_id(user_id, cgm_data$glucose_records)

      g <- plot_compare_glucose(food_df,
                                combine = FALSE, #input$combine,
                                smooth = smooth,
                                title = "Glucose Response",
                                subtitle = sprintf("Food = %s", isolate(food_name)))

      g +
        coord_cartesian(ylim = c(y_scale[["min"]], y_scale[["max"]])) +
        if(baseline & !normalize){
          annotate("rect",
                   xmin = -Inf,
                   xmax = Inf,
                   ymin = gr$mean - gr$sd*2,
                   ymax = gr$mean + gr$sd*2,
                   fill = "green",
                   alpha = 0.3)
        }

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


