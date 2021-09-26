# Okay to delete anything here

library(psiCGM)
library(shiny)
#Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
# Sys.setenv(R_CONFIG_ACTIVE = "local")
# conn_args <- config::get("dataconnection")
# con <- DBI::dbConnect(
#   drv = conn_args$driver,
#   user = conn_args$user,
#   host = conn_args$host,
#   port = conn_args$port,
#   dbname = conn_args$dbname,
#   password = conn_args$password
# )

ui <- fluidPage(titlePanel("hello"),
                numericInput("first",label = "First = ", value = 0),
                textOutput("answer"),
                numericInput("second", label="Second  =", value = 1)
)

server <-function(input,output) {
  a <- reactive(input$first)
  b <- reactive(input$second)
  output$answer <- renderText({
    sprintf("The sum of %d  %d is %d", a(), b(), a()+b())
  })
}

shinyApp(ui,server)
