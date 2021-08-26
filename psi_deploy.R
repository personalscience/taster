# deploy to shinyapps.io

# install.packages(c("tidyverse", "DBI", "config", "RPostgres", "shiny", "devtools"))
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

rsconnect::setAccountInfo(name=config::get("shiny")$name,
                          token=config::get("shiny")$token,
                          secret=config::get("shiny")$secret)


remove.packages("psiCGM")
devtools::install_github("personalscience/psi-shiny-cgm",
                        ref = "dev") #577dc4100cac3940") #,
                        # upgrade = "never")


rsconnect::deployApp(#appDir = file.path(getwd(),"R"),
                     appName = "Tastermonial",
                     appFiles = c("ui.R","server.R",
                                  "config.yml"),
                     forceUpdate = TRUE
                     )



devtools::load_all("~/dev/psi/psiCGM/")
