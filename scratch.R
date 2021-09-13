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

be <- food_times_df(prefixLength = 20, foodname = "Beviva")
be$user_id %>% unique()
paste0(map_chr(be$user_id %>% unique(),
               function(x) {sprintf("%s =(%s)", username_for_id(x), glucose_range_for_id(x) %>% paste0(collapse=","))
                 }
               )
       ) %>% paste0(collapse = ",")
