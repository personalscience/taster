# Okay to delete anything here

library(psiCGM)
#Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
# conn_args <- config::get("dataconnection")
# con <- DBI::dbConnect(
#   drv = conn_args$driver,
#   user = conn_args$user,
#   host = conn_args$host,
#   port = conn_args$port,
#   dbname = conn_args$dbname,
#   password = conn_args$password
# )
#





test <-  function(user = 1014, prefixLength = 0, timeLength = 120,
           bude_notes = tbl(con, "notes_records") %>% filter(user_id == 1014) %>% collect()) {

    notes_df <-
      bude_notes %>% filter(str_detect(str_to_lower(Comment), str_to_lower("SYM")))
    df <- NULL
    fra <-
      tbl(con, "glucose_records") %>% filter(user_id == user) %>% filter(!is.na(value))
    times <- notes_df %>% filter(user_id == user)  %>% pull(Start)


      for (atime in times) {

        t0 <- as_datetime(atime) - minutes(prefixLength)
        tl <- as_datetime(t0 + minutes(timeLength + prefixLength))

        new_df <- fra %>%
          filter(time >= t0 & time <= tl) %>% collect()
      }
    df <- bind_rows(df,new_df)
    return(df)

}

test()

