#' analysis
#'
#' @description All iAUC values for a vector of food names
#'
#' @return The return value, if any, from executing the function.
#' @param s_list vector of strings with the names of the foods
#' @param cgm_data CgmObject
#' @return dataframe
#' @importFrom magrittr %>%
build_all_AUC <- function(s_list, cgm_data) {
  df <- cgmr::df_for_all_auc(s_list, cgm_data)
  return(df %>% mutate(user_id = meal %>% stringr::str_extract("^([:digit:])+(?=-)") %>% as.numeric()))
}


