# Shiny Module and UI to compare foods for a single user

#' @title List all products consumed by `user_id`
#' @param user_id user ID or NULL to show all users
#' @return character vector of product names sorted alphabetically
taster_products <- function(user_id = 1234, db_filter = function(){filter(Start > "2021-06-01")} ) {
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
    db_filter() %>% filter(user_id %in% ID) %>% distinct(Comment) %>%
    arrange(Comment) %>% collect() %>% pull(Comment)

  DBI::dbDisconnect(con)
  return(prods)
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
      )),
    mainPanel(
      textOutput(ns('show_user')),
      uiOutput(ns("food_selection"))
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

    output$show_user <- renderText(

     sprintf("user_id = %d, username = %s, first product = %s", ID(),
              username_for_id(ID()),
              first(sort(taster_products(user_id = ID())$productName))
              )
      )

    taster_prod_list <- sort(taster_products(user_id = ID())$productName)

    output$show_food <- renderText(
      validate(
        need()
      )
    )

    output$food_selection <- renderUI({
      taster_prod_table <- taster_products(user_id = ID())
      validate(
        need(nrow(taster_prod_table)>0,"No foods available for this person")
      )
      prod_names <- sort(taster_prod_table$productName)
      message(paste("finding foods for User", isolate(input$user_id)))
      message(sprintf("User %s first food is %s",isolate(input$user_id),last(prod_names) ))
      selectizeInput(NS(id,"food_name1"),
                     label = "Food Item",
                     choices = prod_names,
                     selected = last(prod_names)
      )
    })

    observe(
      cat(file = stderr(), sprintf("user_id=%s \n",ID()))
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
