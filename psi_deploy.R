# deploy to shinyapps.io

# install.packages(c("tidyverse", "DBI", "config", "RPostgres", "shiny", "devtools"))

rsconnect::setAccountInfo(name=config::get("shiny")$name,
                          token=config::get("shiny")$token,
                          secret=config::get("shiny")$secret)


devtools::install_github("personalscience/psi-shiny-cgm",
                         ref = "dev") #577dc4100cac3940") #,
                        # upgrade = "never")


rsconnect::deployApp(#appDir = file.path(getwd(),"R"),
                     appName = "Tastermonial",
                     appFiles = c("ui.R","server.R",
                                  "config.yml"),
                     forceUpdate = TRUE
                     )



