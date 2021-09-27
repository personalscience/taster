# Shiny Module and UI to compare foods for a single user

glucose_range_for_id <- function(user_id){

  ID = user_id
  GLUCOSE_RECORDS %>% filter(user_id == ID) %>%

    mutate(time = with_tz(time, tzone="America/Los_Angeles")) %>%
    filter(hour(time) >=1 & hour(time) <=4 & !is.na(value)) %>%
    group_by(date=date(time)) %>% summarize(mean = mean(value, na.rm = TRUE)) %>% ungroup() %>%
    summarize(range = as.integer(range(mean))) %>% pull(range)

}

glucose_ranges_for_id <- function(user_id){

  ID = user_id
  GLUCOSE_RECORDS %>% filter(user_id == ID) %>%

    mutate(time = with_tz(time, tzone="America/Los_Angeles")) %>%
    filter(hour(time) >=1 & hour(time) <=4 & !is.na(value)) %>%
    group_by(date=date(time)) %>% summarize(mean = mean(value, na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>% ungroup() %>%
    select(mean,sd) %>% summarize(mean=mean(mean),sd=mean(sd))


}


#' @title UI for comparing food for a single user
#' @description
#' Plot glucose responses for a foods for a single user.
#' @param id Shiny id
#' @export
mod_goddessUI <- function(id) {
  ns <- NS(id)


  sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("user_id"),
        label = "User Name",
        choices = with(user_df_from_db(), paste(first_name, str_match(last_name,"[:alnum:]{2}"))),
        selected = "Richard Sp"
      ),
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

}


#' @title Shiny server to display two food comparisons for one user.
#' @description
#' Given a (reactive) libreview dataframe, this Shiny module will
#' generate a valid ggplot object and display it in an accompanying UI
#' @param id shiny module id
#' @param glucose_df reactive for a valid glucose dataframe
#' @param title a title for the plot
#' @return ggplot object representing a glucose chart
#' @export
mod_goddessServer <- function(id, title = "Name") {

  moduleServer(id, function(input, output, session) {

    ID<- reactive( {message(paste("Selected User", isolate(input$user_id)))
      lookup_id_from_name(input$user_id[1])}
    )
    taster_prod_list <- reactive({
      message(sprintf("seeking prod list for user %d", ID()))
      food_list_db(user_id = ID())}
      )

    output$show_user <- renderText(

     sprintf("user_id = %d, username = %s, product = %s, range=%s", ID(),
              username_for_id(ID()),
             input$food_name1,
             paste0(glucose_range_for_id(ID()), collapse=":")
              )
      )

# food_df2() ----
    food_df2 <- reactive({
      validate(
        need(!is.null(taster_prod_list()),"No food times available for this person"),
        need(!is.null(ID()), "No user selected"),
        need(input$food_name1, "No food selected")
      )

      one_food_df <-  food_times_df_fast(
        glucose_df = GLUCOSE_RECORDS,
        notes_df = NOTES_RECORDS,
        user_id = ID(),
        timeLength = input$timewindow,
        prefixLength = input$prefixLength,
        foodname = input$food_name2
      )

      validate(
        need(!is.null(one_food_df), sprintf("No glucose results for food %s", input$food_name1))
      )
     one_food_df <-  food_times_df(
        user_id = ID(),
        timeLength = input$timewindow,
        prefixLength = input$prefixLength,
        foodname = input$food_name2
      )

     validate(
       need(!is.null(one_food_df), sprintf("No glucose results after eating %s", input$food_name1))
     )

      df <- one_food_df

      df
      }
      )

    # food_df() ----
    food_df <- reactive({
      validate(
        need(!is.null(taster_prod_list()),"No food times available for this person"),
        need(!is.null(ID()), "No user selected"),
        need(input$food_name1, "No food selected")
      )

      one_food_df <-  food_times_df_fast(
        glucose_df = GLUCOSE_RECORDS,
        notes_df = NOTES_RECORDS,
        user_id = ID(),
        timeLength = input$timewindow,
        prefixLength = input$prefixLength,
        foodname = input$food_name1
      )

      validate(
        need(!is.null(one_food_df), sprintf("No glucose results for food %s", input$food_name1))
      )
      one_food_df <-  food_times_df(
        user_id = ID(),
        timeLength = input$timewindow,
        prefixLength = input$prefixLength,
        foodname = input$food_name1
      )

      validate(
        need(!is.null(one_food_df), sprintf("No glucose results after eating %s", input$food_name1))
      )

      df <- one_food_df

      df
    }
    )
# output$food_selection ----
    output$food_selection <- renderUI({
      validate(
        need(!is.null(taster_prod_list()),sprintf("No foods available for user_id %s",ID()))
      )

      message(paste("finding foods for User", isolate(input$user_id)))
      message(sprintf("User %s first food is %s",isolate(input$user_id),first(taster_prod_list()) ))
      selectizeInput(NS(id,"food_name1"),
                     label = "Food Item",
                     choices = taster_prod_list(),
                     selected = first(taster_prod_list())
      )
    })

    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",ID()))
    )

    # output$food1 Render Plot----
    output$food1 <- renderPlot({

      validate(
        need(input$food_name1, "Waiting on database...1"),
        need(!is.null(food_df()), "Problem with food times"),
        need(!is.null(ID()),"No user selected")
      )
      observe(
        cat(file = stderr(), sprintf("render plot for user_id=%d and food=%s \n",
                                     isolate(ID()),
                                     isolate(input$food_name1)))
      )



      food_df <-  if(input$normalize) {food_df() %>% normalize_value()}
      else food_df()


      gr <- glucose_ranges_for_id(ID())
      g <- food_df %>% ggplot(aes(x=t,y=value, color = date_ch)) +
        if(input$smooth) geom_smooth(method = "loess", aes(fill = date_ch)) else geom_line(size=2)

      gg <- g + psi_theme +
        geom_rect(aes(xmin=0,
                      xmax=120, #max(Date),
                      ymin=-Inf,
                      ymax=Inf),
                  color = "lightgrey",
                  alpha=0.005) +
        labs(title = "Glucose Response", subtitle = str_to_title(isolate(input$food_name1)),
             x = "", y = "")

      gg + if(input$baseline & !input$normalize){
        geom_rect(aes(xmin = -Inf,
                      xmax = Inf,
                      ymin = gr$mean - gr$sd,
                      ymax = gr$mean + gr$sd),
                  fill = "green",
                  alpha = 0.005,
                  inherit.aes = FALSE)
      }
  })

    # output$food_selection2 ----
    output$food_selection2 <- renderUI({
      validate(
        need(!is.null(taster_prod_list()),sprintf("No foods available for user_id %s",ID()))
      )

      message(paste("finding foods for User", isolate(input$user_id)))
      message(sprintf("User %s first food is %s",isolate(input$user_id),first(taster_prod_list()) ))
      selectizeInput(NS(id,"food_name2"),
                     label = "Food Item",
                     choices = taster_prod_list(),
                     selected = first(taster_prod_list())
      )
    })

    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",ID()))
    )

    # output$food2 Render Plot ----
    output$food2 <- renderPlot({

      validate(
        need(input$food_name1, "Waiting on database...1"),
        need(!is.null(food_df2()), "Problem with food times"),
        need(!is.null(ID()),"No user selected")
      )
      observe(
        cat(file = stderr(), sprintf("render plot for user_id=%d and food=%s \n",
                                     isolate(ID()),
                                     isolate(input$food_name2)))
      )



      food_df <-  if(input$normalize) {food_df2() %>% normalize_value()}
      else food_df2()

      gr <- glucose_ranges_for_id(ID())

      g <- food_df %>% ggplot(aes(x=t,y=value, color = date_ch)) +
        if(input$smooth) geom_smooth(method = "loess", aes(fill = date_ch)) else geom_line(size=2)

      gg <- g + psi_theme +
        geom_rect(aes(xmin=0,
                      xmax=120, #max(Date),
                      ymin=-Inf,
                      ymax=Inf),
                  color = "lightgrey",
                  alpha=0.005) +
        labs(title = "Glucose Response", subtitle = str_to_title(isolate(input$food_name2)),
             x = "", y = "")

      gg + if(input$baseline & !input$normalize){
        geom_rect(aes(xmin = -Inf,
                      xmax = Inf,
                      ymin = gr$mean - gr$sd,
                      ymax = gr$mean + gr$sd),
                  fill = "green",
                  alpha = 0.01,
                  inherit.aes = FALSE)
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

          iAUC = auc_calc(tibble(time=t,value=value)),
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
        mutate(timestamp = with_tz(timestamp, tzone = "America/Los_Angeles")) %>%
        distinct() %>% arrange(meal)

    })

    output$downloadFood_df <-
      downloadHandler(
        filename = function() {
          sprintf("Food_data-%s-%s.csv", ID(), Sys.Date())
        },
        content = function(file) {
          write_csv(bind_rows(food_df(),food_df2()) %>% distinct() %>% arrange(meal), file)
        }
      )

  })

}



demo_food2 <- function(){


  ui <- fluidPage(mod_goddessUI("x"))
  server <- function(input, output, session) {
    mod_goddessServer("x", reactiveVal("Username"))
  }
  shinyApp(ui, server)

}

if(interactive()) demo_food2()
