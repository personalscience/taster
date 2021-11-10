# deploy to shinyapps.io

# install.packages(c("tidyverse", "DBI", "config", "RPostgres", "shiny", "devtools"))
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# install.packages(c("bslib"))
# install.packages(c("firebase"))

rsconnect::setAccountInfo(name=config::get("shiny")$name,
                          token=config::get("shiny")$token,
                          secret=config::get("shiny")$secret)



remove.packages("cgmr")
devtools::install_github("personalscience/cgmr",
                         ref = "HEAD",
                         upgrade = "never")

#rsconnect::deployApp()

golem::add_dockerfile(repos = c(REPO_NAME = 'https://packagemanager.rstudio.com/cran/__linux__/focal/2021-11-09'))

