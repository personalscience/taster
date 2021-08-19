# Taster

This is a front end to the R Package [psiCGM](https://github.com/personalscience/psi-shiny-cgm)

## Requirements

To host on `shinyapps.io` you will first need to create an account. I recommend using your Github account credentials. Once you have enabled your `shinyapps` account, follow the instructions to get your "token" and "secret".

This directory needs a `config.yml` file to deploy on <https://shinyapps.io>. Be sure to fill in all the values below correctly:

``` {.yaml}
default:
  tastermonial:
    datadir: "~/path/to/a/local/directory/where/you/store/tastermonial/files"
  shiny:
    name: 'personalscience'
    token: 'yourtoken'
    secret: 'your secret'
  dataconnection:
    driver: !expr RPostgres::Postgres()
    host: "localhost"
    user: "postgres"
    password: 'yourlocalpassword'
    port: 5432
    dbname: 'qsdb'
    glucose_table: 'glucose_records'

shinyapps:
  dataconnection:
    driver: !expr RPostgres::Postgres()
    host: "<IP address for local host>"
    user: "username"
    password: 'password'
    port: 5432
    dbname: 'qsdb'
    glucose_table: 'glucose_records'
```

## Deploy

Run the script `psi_deploy.R`.
