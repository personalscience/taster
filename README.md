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

To run locally, you will need a Postgres database. The app will use the configuration set in the `config.yml`. To use the local database, run

```r
Sys.setenv(R_CONFIG_ACTIVE = "local")
```

## Database

The app requires a database. You can load data with the scripts `psi_db*`

This is easier if you store all the data in the directory with the path at

``` r
config::get("tastermonial")$datadir
```

Download from https://libreview.com the glucose CSV files.  Any CSV file that includes "glucose" in the name will automatically read into the database with script `psi_db_load.R`.

Download Tastermonial food notes and save as a CSV file in the same directory. Run `psi_db_taster_notes.R` to read all results into a dataframe that you can write to the database.



## Deploy

Run the script `psi_deploy.R`.

Go to: <https://personalscience.shinyapps.io/Tastermonial/>
