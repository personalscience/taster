# Shiny Module and UI to compare foods for a single user

#' @title List all products consumed by `user_id`
#' @return character vector of product names sorted alphabetically
taster_products <- function(user_id = 1234) {
  conn_args <- config::get("dataconnection")
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  ID <- user_id

  if (is.null(ID)) {
    prods <- tbl(con, "notes_records") %>% filter(Activity == "Food") %>%
      filter(Start > "2021-06-01") %>%
      group_by(Comment) %>% add_count() %>% filter(n > 2) %>% distinct(Comment) %>%
      transmute(productName = Comment, user_id = user_id) %>%
      collect() %>% arrange(productName)
  } else
    prods <-
    tbl(con, "notes_records") %>% filter(Activity == "Food") %>%
    filter(Start > "2021-06-01") %>% filter(user_id == ID) %>% distinct(Comment) %>%
    transmute(productName = Comment, user_id = ID) %>%
    collect() %>% arrange(productName)

  DBI::dbDisconnect(con)
  return(prods)
}


#' @title UI for comparing two foods for a single user
#' @description
#' Plot glucose responses for two foods for a single user.
#' @param id Shiny id
#' @export
mod_goddessUI <- function(id) {
  ns <- NS(id)


  sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("user_id"),
        label = "User Name",
        choices = with(user_df_from_libreview, paste(first_name, str_match(last_name,"[:alnum:]{2}"))),
        selected = "Richard Sp"
      ),
      uiOutput(ns("food_selection1")),
       actionButton(ns("submit_foods"), label = "Calculate Stats"),
      checkboxInput(ns("normalize"), label = "Normalize"),
     numericInput(ns("prefixLength"), label = "Prefix Minutes", value = 0, width = "30%" ),
     numericInput(ns("timewindow"), label = "Time Window (Minutes)", value = 150, width = "30%"),
      downloadButton(ns("downloadFood_df"), label = "Download Results"),
     actionButton(ns("show_raw"), label = "Show Raw Data"),

    ),
    mainPanel(plotOutput(ns("food1")),
              h3("Stats Table"),
              dataTableOutput(ns("auc_table")),
              h3("Raw Data"),
              dataTableOutput(ns("raw_data_table"))
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
mod_goddessServer <- function(id,  glucose_df, title = "Name") {

  moduleServer(id, function(input, output, session) {

    ID<- reactive( {message(paste("Selected User", isolate(input$user_id)))
      lookup_id_from_name(input$user_id[1])}
    )


    observe(
      cat(stderr(), sprintf("username=%s \n",ID()))
    )
    food_df <- reactive(

      food_times_df(
        user_id = ID(),
        foodname = input$food_name1,
        timeLength = input$timewindow,
        prefixLength = input$prefixLength
      )
     %>%
      filter(!is.na(value)))


    output$food_selection1 <- renderUI({
      taster_prod_table <- taster_products(user_id = ID())
      prod_names <- sort(taster_prod_table$productName)
      #message(paste("finding foods for User", isolate(input$user_id)))
      message(sprintf("User %s first food is %s",isolate(input$user_id),last(prod_names) ))
      selectizeInput(NS(id,"food_name1"),
                     label = "Your food 1",
                     choices = prod_names,
                     selected = last(prod_names)
                    )
    })

    output$food_selection2 <- renderUI({
      taster_prod_table <- taster_products(user_id = ID())
      prod_names <- sort(taster_prod_table$productName)
      #message(paste("finding foods for User", isolate(input$user_id)))
      message(sprintf("User %s second food is %s",isolate(input$user_id),last(prod_names) ))
      selectizeInput(NS(id,"food_name2"),
                     label = "Your food 2",
                     choices = prod_names,
                     selected = last(prod_names)
      )
    })


    output$downloadFood_df <-
      downloadHandler(
        filename = function() {
          sprintf("Food_data-%s-%s.csv", ID(), Sys.Date())
        },
        content = function(file) {
          write_csv(food_df(), file)
        }
      )


    output$libreview <- renderPlot({
      input$submit_foods
      if (input$normalize) {
        g <- isolate(food_df()) %>% group_by(meal) %>% arrange(t) %>% mutate(value = value-first(value)) %>%
          ungroup() %>%  arrange(meal, t) %>%
          ggplot(aes(t, value, color = meal)) + geom_line(size = 2)
      } else
      g <-
        isolate(food_df()) %>% ggplot(aes(t, value, color = meal)) + geom_line(size = 2)
      g})


      output$food1 <- renderPlot({

        input$submit_foods
        validate(
          need(input$food_name1, "Waiting on database...1")
        )
        one_food_df <- food_times_df(user_id = ID(),
                                     foodname = input$food_name1,
                                     timeLength = input$timewindow,
                                     prefixLength = input$prefixLength)
        if (input$normalize) {
          g <- one_food_df %>% normalize_value() %>%
            arrange(meal, t) %>%
            ggplot(aes(t, value, color = date_ch)) + geom_line(size = 2) + ylim(-50,50)
        } else
          g <-
            one_food_df %>% ggplot(aes(t, value, color = date_ch)) + geom_line(size = 2)
        g+ psi_theme+
          geom_rect(aes(xmin=0,
                        xmax=120, #max(Date),
                        ymin=-Inf,
                        ymax=Inf),
                    color = "lightgrey",
                    alpha=0.005) +
          labs(title = "Glucose Response", subtitle = str_to_title(isolate(input$food_name1)))

    })
#
#       output$food2 <- renderPlot({
#
#         input$submit_foods
#         validate(
#           need(input$food_name1, "Waiting on database...2")
#         )
#         one_food_df <- food_times_df(user_id = ID(),
#                                      foodname = input$food_name2,
#                                      timeLength = input$timewindow,
#                                      prefixLength = input$prefixLength)
#         if (input$normalize) {
#           g <- one_food_df %>% normalize_value() %>%
#             arrange(meal, t) %>%
#             ggplot(aes(t, value, color = meal)) + geom_line(size = 2)
#         } else
#           g <-
#           one_food_df %>% ggplot(aes(t, value, color = meal)) + geom_line(size = 2)
#         g +
#           geom_rect(aes(xmin=0,
#                         xmax=120, #max(Date),
#                         ymin=-Inf,
#                         ymax=Inf),
#                     color = "lightgrey",
#                     alpha=0.005)
#
#
#       })
    output$auc_table <- renderDataTable({
      input$submit_foods
      validate(
        need(input$submit_foods, "Press Calculate Stats")
      )
      food_df() %>%
        filter(t >= -5) %>% # only look at the times after the food was eaten.
        filter(t <= 120) %>% # and only the first 2 hours.
        group_by(meal) %>%
        summarize(
                  auc = DescTools::AUC(t,value-first(value)),
                  min = min(value),
                  max = max(value),
                  rise = last(value) - first(value),
                  .groups = 'drop') %>%
        #summarize(auc = sum((lag(value)-value)*(t-lag(t)), na.rm = TRUE)) %>%
        arrange(auc)

    })

    output$raw_data_table <- renderDataTable({

      validate(
        need(input$show_raw, "Press Show Raw")
      )
      food_df()

    })
  })

}

demo_food2 <- function(){

  glucose_df <- glucose_df_from_db(user_id = 1235)
  ui <- fluidPage(mod_goddessUI("x"))
  server <- function(input, output, session) {
    mod_goddessServer("x", reactive(glucose_df), reactiveVal("Username"))
  }
  shinyApp(ui, server)

}

if(interactive()) demo_food2()
