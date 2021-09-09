#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Sys.setenv(R_CONFIG_ACTIVE = "local")


#' Define server logic required to display CGM information
#' @import shiny
#' @import magrittr dplyr
server <- function(input, output) {

    # datafilepath <- psiCGM:::csvFilePathServer("datafile")
     output$currentDB <- renderText(sprintf("DB=%s. psiCGM version = %s, db latest = %s",
                                            attr(config::get(),"config"),
                                          packageVersion("psiCGM"),
                                          first(tbl(con,"glucose_records") %>%
                                             filter(time == max(time)) %>%
                                             pull(time) %>%
                                             with_tz(tzone=Sys.timezone()))))
   #
   #
    mod_goddessServer("food2_compare_plot", active_glucose_record, title = "Tastermonial" )
   #
    active_glucose_record <- mod_filterServer("psi_filter_plot")
   username<-reactive(username_for_id(active_glucose_record()[["user_id"]][1]))

   observe(
     cat(file = stderr(), sprintf("Username=%s \n",username()))
   )

   g <- mod_libreview_plotServer("modChart", active_glucose_record, title = username)

   food_time_ggplot <- mod_foodTasterServer("food_compare_plot", active_glucose_record, title = username )


}






