# read Tastermonial users into table `user_list`

extra_user_df <- read_csv(file = system.file("extdata",
                                             package = "psiCGM",
                                             "Tastermonial_Extra_Users.csv"),
                          col_types = "cccccd") %>% mutate(birthdate = lubridate::mdy(birthdate))

#' @title Return user list from Tastermonial Libreview download
#' @description A Libreview "practice" stores all its user information in a single
#' CSV file, which this function will convert into a canonical dataframe.
#' @param file the main file downloaded from a Libreview practice ID
user_df_from_csv <- function(filepath = file.path(config::get("tastermonial")$datadir,"Tastermonial_AllPatients_dashboard.csv")){
  user_df <- readr::read_csv(file = filepath,
                             skip =1,
                             col_types = cols()) %>%
    transmute(first_name = `First Name`,
              last_name = `Last Name`,
              birthdate = lubridate::mdy(`Date of Birth`),
              latest_data = `Last Available Data`,
              libreview_status = `LibreView User Status`
    )

  return(user_df)
}

#' @title Users known to Libreview Practice Portal
#' @description
#' A dataframe of all users and their ids, taken from the Libreview practice portal
user_df_from_libreview <-
  user_df_from_csv() %>% mutate(user_id = row_number() + 1000) %>%
  dplyr::anti_join(extra_user_df,
                   by = c("first_name", "last_name")) %>% bind_rows(extra_user_df)




user_df <-  user_df_from_csv()

user_df
