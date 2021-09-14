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

be <- food_times_df(prefixLength = 20, foodname = "Beviva")

be %>% ggplot(aes(t,value, color = date_ch)) + geom_line(size=2)

be %>% group_by(meal) %>%  mutate(ave = mean(value))  %>%
  ggplot(aes(t,value, color = date_ch)) + geom_line(size=2) +# geom_point(size = 2, color = "black") +
  geom_point(inherit.aes = FALSE, aes(t,ave)) # + geom_line(inherit.aes = FALSE, aes(t,ave))

be %>% group_by(t,meal) %>% mutate(ave = mean(value))
