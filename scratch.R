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
tbl(con,"glucose_records") %>% distinct(user_id)
tbl(con,"notes_records") %>% filter(user_id==1235) %>% filter(is.na(Start))
