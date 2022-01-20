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


remove.packages("cgmrdb")
devtools::install_github("personalscience/cgmrdb",
                         ref = "HEAD",
                         upgrade = "never"
                         )
# usethis::use_package("cgmrdb") # add to description file

#rsconnect::deployApp()

# works https://packagemanager.rstudio.com/cran/__linux__/focal/2021-11-09
# https://packagemanager.rstudio.com/all/__linux__/focal/2021-12-01+Y3JhbiwyOjQ1MjYyMTU7Qjk2N0NENjk
# old https://packagemanager.rstudio.com/all/__linux__/focal/2022-01-03+Y3JhbiwyOjQ1MjYyMTU7NTY4Qjk1ODA
# https://packagemanager.rstudio.com/client/#/repos/1/overview


golem::add_dockerfile(repos = c(REPO_NAME = 'https://packagemanager.rstudio.com/all/__linux__/focal/2022-01-13+Y3JhbiwyOjQ1MjYyMTU7M0ZDNzkwNzE'))

remove.packages("firebase")
remotes::install_github("JohnCoene/firebase")
