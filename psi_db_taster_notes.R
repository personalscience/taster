# add Tastermonial database to notes
# WARNING: This script is intended to be run interactively only.
# uncomment the last line to write to the database.

source("psi_taster_data.R")  # load functions to process Tastermonial files


taster_raw <- taster_raw(filepath = file.path(config::get("tastermonial")$datadir, "table-data.csv"))

taster_notes_df <- taster_raw %>% transmute(Start = with_tz(lubridate::parse_date_time(startEatingDate, orders = "dmY HM p z"),
                                                              tzone = Sys.timezone()),
                                              End = as_datetime(NA),
                                              Activity = "Food",
                                              Comment = name,
                                              Z = as.numeric(NA),
                                              user_id = map_dbl(user, id_from_taster))

# psi_fill_taster_notes_from_scratch(taster_notes_df)
