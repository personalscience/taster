# Okay to delete anything here

library(psiCGM)
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

glucose_range_for_id <- function(user_id){

  ID = user_id
  GLUCOSE_RECORDS %>% filter(user_id == ID) %>%

    mutate(time = with_tz(time, tzone="America/Los_Angeles")) %>%
    filter(hour(time) >=1 & hour(time) <=4 & !is.na(value)) %>%
    group_by(date=date(time)) %>% summarize(mean = mean(value, na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>% ungroup() %>%
    select(mean,sd) %>% summarize(mean=mean(mean),sd=mean(sd))
   # summarize(range = as.integer(range(mean)))

}

glucose_range_for_id(1234)

sample <- food_times_df()
sample %>% ggplot(aes(x=t,y=value,color=meal)) + geom_line() +
  geom_rect(aes(xmin = -Inf,
                xmax = Inf,
                ymin = gr$mean - gr$sd,
                ymax = gr$mean + gr$sd),
            fill = "green",
            alpha = 0.005,
            inherit.aes = FALSE)

gr <- sample %>% distinct(user_id) %>% pull(user_id) %>% first() %>% glucose_range_for_id()
