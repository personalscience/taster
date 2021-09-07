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

``` r
Sys.setenv(R_CONFIG_ACTIVE = "local")
```

## Database

The app requires a database. You can load data with the scripts `psi_db*`

This is easier if you store all the data in the directory with the path at

``` r
config::get("tastermonial")$datadir
```

This data directory should contain:

-   All Libreview CSV files, exactly as downloaded from <https://libreview.com>. Any CSV file that includes "glucose" in the name will automatically read by the script `psi_db_load.R`.
-   Latest Tastermonial firebase output, stored in the file `table-data.csv`. The script `psi_db_taster_notes.R` will read all results into a dataframe.
-   Miscellaneous other raw files including Nutrisense-formatted files.

*Important*: The most important script is `psi_db_load.R`. Run this to drop and then load from scratch everything in the current database.

The very first time you set up a new Postgres database, run the script `psi_db_create.R`. It's okay to run this as many times as you like; it won't do anything if the database is already set up properly.

## Deploy

Run the script `psi_deploy.R`.

Go to: <https://personalscience.shinyapps.io/Tastermonial/>
