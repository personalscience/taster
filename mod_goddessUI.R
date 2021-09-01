# Shiny Module and UI to compare foods for a single user

taster_products <- function(user_id = 1234) {
conn_args <- config::get("dataconnection")
con <- DBI::dbConnect(
  drv = conn_args$driver,
  user = conn_args$user,
  host = conn_args$host,
  port = conn_args$port,
  dbname = conn_args$dbname,
  password = conn_args$password)

  ID <- user_id

  if(is.null(ID)){
  prods <- tbl(con,"notes_records") %>% filter(Activity == "Food") %>%
    filter(Start > "2021-06-01") %>%
    group_by(Comment) %>% add_count() %>% filter(n>2) %>% distinct(Comment) %>%
    transmute(productName = Comment, user_id = user_id) %>%
    collect()
} else
  prods <- tbl(con,"notes_records") %>% filter(Activity == "Food") %>%
    filter(Start > "2021-06-01") %>% filter(user_id == ID) %>% distinct(Comment) %>%
    transmute(productName = Comment, user_id=ID) %>%
    collect()

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

  taster_prod_table <- taster_products()

  #taster_prod_table <- taster_df() %>% distinct(pid, productName) %>% filter(!is.na(productName) & pid!="xxxxx")
  taster_food1 <- taster_prod_table[str_detect(taster_prod_table$productName,"KIND"),1]$productName
  taster_food2 <- taster_prod_table[str_detect(taster_prod_table$productName,"Real Food"),1]$productName

  sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("user_id"),
        label = "User Name",
        choices = with(user_df_from_libreview, paste(first_name, last_name)),
        selected = "Ayumi Blystone"
      ),
     # textInput(ns("food_name1"), label = "Food 1", value = "Real Food Bar"),
     # textInput(ns("food_name2"), label = "Food 2", value = "Kind, nuts & Spices"),
      selectizeInput(ns("food_name1"), label = "Food1", choices = taster_prod_table$productName, selected=taster_food1),
      selectizeInput(ns("food_name2"), label = "Food2", choices = taster_prod_table$productName, selected=taster_food2),
      actionButton(ns("submit_foods"), label = "Submit Foods"),
      checkboxInput(ns("normalize"), label = "Normalize"),
      downloadButton(ns("downloadFood_df"), label = "Download Results")
    ),
    mainPanel(plotOutput(ns("libreview")),
              dataTableOutput(ns("auc_table")))
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
    food_df <- reactive(bind_rows(food_times_df(user_id = ID(),
                                                foodname = input$food_name1),
                                  food_times_df(user_id = ID(),
                                                foodname = input$food_name2)) %>%
                          filter(!is.na(value)))


    output$food_selections <- renderUI({

      selectizeInput(ns("food_name2"), label = "Food2", choices = taster_prod_table$productName, selected=taster_food2)
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
      g

    })
    output$auc_table <- renderDataTable({
      input$submit_foods
      isolate(food_df()) %>% distinct() %>%
        group_by(meal, foodname) %>%
        summarize(
                  auc = DescTools::AUC(t,value-first(value)),
                  min = min(value),
                  max = max(value),
                  rise = last(value) - first(value),
                  .groups = 'drop') %>%
        #summarize(auc = sum((lag(value)-value)*(t-lag(t)), na.rm = TRUE)) %>%
        arrange(auc)

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
