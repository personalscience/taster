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

cow <- food_times_df(foodname = "Cream of Wheat")

cow %>% ggplot(aes(x=t, y = value, color = date_ch)) + geom_line() + facet_grid(rows = vars(date_ch))

cow %>% normalize_value() %>% ggplot(aes(x=t, y = value, color = date_ch)) + geom_line() + facet_grid(rows = vars(date_ch))

cow20 %>% select(t, value, date_ch, foodname) %>%
  group_by(date_ch) %>%
  mutate(norm = if_else(t>=0, (value - min(value)) / (max(value) - min(value)),
                        (value-last(value)))) %>%
  ggplot(aes(x=t, y = norm, color = date_ch)) +
  geom_line() + facet_grid(rows = vars(date_ch)) +
  labs(colour = "Cream of Wheat")

tbl(con,"notes_records") %>% filter(user_id %in% 1007:1020) %>% distinct(Comment) %>% pull(Comment)

cow20 <- food_times_df(foodname = "Cream of Wheat", prefixLength = 20)

cowf <- cow20 %>% transmute(t,value,meal = factor(date_ch))

cowf %>% group_by(meal) %>% group_by(time = t>0) %>% mutate(norm1 = first(value), norm2=last(value))


cow20 %>% group_by(meal) %>% count()

cow20 %>% group_by(meal) %>%
   summarize(norm = first(value),
             last = last(value)) # %>% ungroup() #if_else(t>=0, first(value), min2 = last(value)))
  View()

tibble(t = -3:10, y = round(rnorm(length(t))*100)) %>%
  group_by(time = t >= 0) %>%
  mutate(norm = if_else(t>=0, first(y), last(y)))


  #mutate(value = value - min(value) / (max(value) - min(value)))


tibble(t=-3:10,y=t+10) %>%
  group_by(t >=0) %>%
  mutate(ave_from_zero = mean(y)
  )
