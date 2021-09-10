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


taster_raw_all$name[210]
taster_classify_food(taster_raw_all$name[210])

taster_classify_food("KIND")

taster_classify_food(taster_notes_df$Comment[100])
sapply(taster_notes_df$Comment, function(x) taster_classify_food(x))

map_chr(taster_notes_df$Comment, taster_classify_food)

taster_notes_df %>% group_by(Comment) %>% summarize(n=n()) %>% clipr::write_clip()

taster_notes_df$Comment
