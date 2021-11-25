#' glucose
#'



#'
#' @title Max and Min Glucose Ranges for `user_id`
#' @param user_id user ID
#' @param glucose_records valid glucose records
glucose_ranges_for_id <- function(user_id, glucose_records){

  GLUCOSE_RECORDS <- glucose_records

  ID = user_id
  GLUCOSE_RECORDS %>% filter(user_id == ID & !is.na(value)) %>%

    mutate(time = lubridate::with_tz(time, tzone="America/Los_Angeles")) %>%
    filter(lubridate::hour(time) >=1 & lubridate::hour(time) <=4 & !is.na(value)) %>%
    group_by(date=lubridate::date(time)) %>%
    summarize(mean = mean(value, na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>%
    ungroup() %>%
    select(mean,sd) %>%
    summarize(mean=mean(mean, na.rm = TRUE),sd=mean(sd, na.rm = TRUE))


}
