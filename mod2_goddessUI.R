# Shiny Module and UI to compare foods for a single user

#' @title List all products consumed by `user_id`
#' @param user_id user ID or NULL to show all users
#' @return character vector of product names sorted alphabetically
food_list_db <- function(user_id = 1234  ) {
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
    filter(Start > "2021-06-01") %>% filter(user_id %in% ID) %>% distinct(Comment) %>%
     collect() %>% pull(Comment)

  if(length(prods) > 0)
    return(sort(prods))
  else return(NULL)

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
      ),
    uiOutput(ns("food_selection")),
    checkboxInput(ns("normalize"), label = "Normalize"),
    ),
    mainPanel(
      textOutput(ns('show_user')),
      plotOutput(ns("food1"))
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

     sprintf("user_id = %d, username = %s, product = %s", ID(),
              username_for_id(ID()),
             input$food_name1
              )
      )

    food_df <- reactive({
      validate(
        need(!is.null(taster_prod_list()),"No food times available for this person"),
        need(!is.null(ID()), "No user selected"),
        need(input$food_name1, "No food selected")
      )

      food_times_df(
        user_id = ID(),
        foodname = input$food_name1
      )
      }
      )


    output$food_selection <- renderUI({
      validate(
        need(!is.null(taster_prod_list()),"No foods available for this person")
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
      one_food_df <- food_df()

      df <- if(input$normalize) {
                   one_food_df %>% normalize_value()}
      else one_food_df

      g <- df %>%  ggplot(aes(x=t,y=value, color = date_ch)) + geom_line(size = 2)

      g
  })

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
