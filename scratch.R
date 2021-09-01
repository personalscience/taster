# Okay to delete anything here

library(psiCGM)
#Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
conn_args <- config::get("dataconnection")
con <- DBI::dbConnect(
  drv = conn_args$driver,
  user = conn_args$user,
  host = conn_args$host,
  port = conn_args$port,
  dbname = conn_args$dbname,
  password = conn_args$password
)

# DBI::dbWriteTable(con, "notes_records", taster_notes_df, append = TRUE)
# tbl(con, "notes_records") %>% distinct(user_id) %>% collect() %>% pull(user_id) #mutate(name=username_for_id(user_id))
#
# psi_fill_taster_notes_from_scratch()

glucose_df_from_db()
tbl(con,"glucose_records") %>% distinct(user_id) %>% show_query()
tbl(con,"notes_records") %>% filter(Activity == "Food") %>%
  filter(Start > "2021-06-01") %>%
  group_by(Comment) %>% add_count() %>% filter(n>2) %>% distinct(Comment) %>% collect()


taster_df(file.path(config::get("tastermonial")$datadir, "table-data.csv"))

taster_prod_table <- taster_df() %>% distinct(productName) %>% filter(!is.na(productName) & productName!="xxxxx")
taster_prod_table$productName %>% str_detect("KIND")

food_times_df(foodname="KIND")
glucose_for_food_df(foodname="KIND")


taster_raw() %>% transmute(Start = with_tz(lubridate::parse_date_time(startEatingDate, orders = "dmY HM p z"),
                                           tzone = Sys.timezone()),
                           End = as_datetime(NA),
                           )


with_tz(lubridate::parse_date_time(taster_raw()$startEatingDate, orders = "dmY HM p z"),
        tzone = Sys.timezone())
