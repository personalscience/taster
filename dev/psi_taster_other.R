# load additional Tastermonial data files

# This is hard-coded to read specific files.
# DO NOT RUN MORE THAN ONCE

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


nutrisense_andreos <- glucose_df_from_nutrisense(filepath = file.path(config::get("tastermonial")$datadir,
                                                                     "nutrisense_andreos.csv"),
                                                user_id = 1006)

nutrisense_sherri <- glucose_df_from_nutrisense(filepath = file.path(config::get("tastermonial")$datadir,
                                                       "nutrisense_1627653419114_export-SherriJo.csv"),
                                                user_id = 1005)

nutrisense_anthony <- glucose_df_from_nutrisense(filepath = file.path(config::get("tastermonial")$datadir,
                                                       "nutrisense_anthony_davis.csv"),
                                  tz = Sys.timezone(),
                                  user_id = 1021)

new_nutrisense_records <- bind_rows(nutrisense_sherri,
                                    nutrisense_anthony,
                                    nutrisense_andreos
                                    )

# anthony_record <- tibble(first_name = "Anthony", last_name = "Davis", birthdate=as.Date("1900-01-01"),
#                          libreview_status = NA,
#                          user_id = 1021)


if(nrow(tbl(con,"glucose_records") %>% filter(user_id == 1021) %>% collect())) message("Anthony Nutrisense glucose exists") else {

#DBI::dbWriteTable(con, "user_list", anthony_record, append = TRUE )
DBI::dbWriteTable(con, "glucose_records", new_nutrisense_records, append = TRUE )
DBI::dbWriteTable(con, "notes_records",  notes_df_from_glucose_table(user_id = 1005), append = TRUE)

}
