#' analysis
#'
#' @description All iAUC values for a vector of food names
#'
#' @return The return value, if any, from executing the function.
#' @param s_list vector of strings with the names of the foods
#' @param glucose_records well-formed glucose dataframe
#' @param notes_records well-formed notes dataframe
#' @return dataframe
#' @import magrittr
build_all_AUC <- function(s_list, glucose_records, notes_records) {
  df <- cgmr::df_for_all_auc(s_list, glucose_records,
                             notes_records)
  return(df %>% mutate(user_id = meal %>% stringr::str_extract("^([:digit:])+(?=-)") %>% as.numeric()))
}


