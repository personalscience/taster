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

# calculate baseline
lookup_id_from_name("Bude S")

bs <- tbl(con, "glucose_records") %>% filter(user_id == 1014) %>% collect()
rl <- tbl(con, "glucose_records") %>% filter(user_id == 1008) %>% collect()

bs %>% mutate(time = with_tz(time, tzone=Sys.timezone())) %>%
  filter(hour(time) >=0 & hour(time) <=5 & !is.na(value)) %>%
  group_by(date(time)) %>% summarize(mean(value, na.rm = TRUE)) %>% pull(2) %>% range()

rs <- tbl(con, "glucose_records") %>% filter(user_id == 1234) %>% collect()
rs %>% filter(hour(time) >=0 & hour(time) <=5 & !is.na(value)) %>% group_by(date(time)) %>% summarize(mean(value, na.rm = TRUE)) %>% View()


gdf <- tbl(con, "glucose_records")  %>% filter(!is.na(value) ) %>% collect()

gdf  %>% mutate(time = with_tz(time, tzone=Sys.timezone())) %>%
  group_by(user_id) %>% add_count() %>%
  filter(hour(time) >=0 & hour(time) <=5) %>%
  group_by(date = date(time)) %>% summarize(mean = mean(value), user_id, n) %>%
  ungroup() %>% select(mean, user_id)  %>%
  group_by(user_id) %>%
  summarize(mean) %>% distinct(user_id, mean)



