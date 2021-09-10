# add Tastermonial database to notes
# WARNING: This script is intended to be run interactively only.
# uncomment the last line to write to the database.

source("psi_taster_read_data.R")  # load functions to process Tastermonial files


taster_raw_df <- taster_raw(filepath = file.path(config::get("tastermonial")$datadir, "table-data.csv"))
taster_raw2_df <- taster_raw(filepath = file.path(config::get("tastermonial")$datadir, "FoodLogFireBasetable-data_ingestsept6.csv"))
taster_raw_all <- bind_rows(taster_raw_df,taster_raw2_df)
# taster_notes_df2 <- taster_raw %>% transmute(Start = with_tz(lubridate::parse_date_time(startEatingDate, orders = "dmY HM p z"),
#                                                               tzone = Sys.timezone()),
#                                               End = as_datetime(NA),
#                                               Activity = "Food",
#                                               Comment = name,
#                                               Z = as.numeric(NA),
#                                               user_id = map_dbl(user, id_from_taster))

taster_notes2_df <- taster_raw2_df %>% filter(!is.na(user))  %>%  transmute(Start = with_tz(lubridate::parse_date_time(startEatingDate, orders = "Ymd HM"),
                                                            tzone = Sys.timezone()),
                                            End = as_datetime(NA),
                                            Activity = "Food",
                                            Comment = str_to_upper(str_squish(str_replace_all(name, "[^A-Za-z,]", " "))),
                                            Z = as.numeric(NA),
                                            user_id = map_dbl(user, id_from_taster))

taster_notes_df1 <- taster_raw_df %>% transmute(Start = with_tz(lubridate::parse_date_time(startEatingDate, orders = "dmY HM p z"),
                                                         tzone = Sys.timezone()),
                                         End = as_datetime(NA),
                                         Activity = "Food",
                                         Comment = str_to_upper(str_squish(str_replace_all(name, "[^A-Za-z,]", " "))),
                                         Z = as.numeric(NA),
                                         user_id = map_dbl(user, id_from_taster))

taster_notes_df <- bind_rows(taster_notes2_df,taster_notes_df1)
rm(taster_notes_df1,taster_notes2_df,taster_raw2_df,taster_raw_df)


# psi_fill_taster_notes_from_scratch(taster_notes_df)
