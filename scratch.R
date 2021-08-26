# Okay to delete anything here

library(psiCGM)

taster_dir <- file.path(Sys.getenv("ONEDRIVE"),"Ensembio/Personal Science/Partners/Tastermonial/data")

list.files(taster_dir) %>% stringr::str_subset("Car")


carles <- glucose_df_from_libreview_csv(list.files(taster_dir, full.names = TRUE) %>% stringr::str_subset("Car"))
