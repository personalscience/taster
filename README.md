# Taster

This is a Golem-based implementation of an R Shiny app to view, explore, and analyze CGM glucose results obtained from [Tastermonial](https://tastermonial.com) users.

## Requirements

To host on `shinyapps.io` you will first need to create an account. I recommend using your Github account credentials. Once you have enabled your `shinyapps` account, follow the instructions to get your "token" and "secret".

## Using Config.yml (deprecated in this version)

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

## Docker

Make a Docker version using the `Dockerfile`.

I copy all the files from my local machine to the remote machine like this:

``` sh
scp -r * myname@remoteserver:/path/to/remote/dir
```

Assuming Docker is already available on the remote machine, build the container:

``` sh
docker build -t tastermonial .
```

Keep the database credentials as system environment variables in a file `.env` that looks like this:

``` sh
TASTER_HOST=12.34.56.78
TASTER_USER=postgres
TASTER_PASSWORD=<your password>
TASTER_DBNAME=<db name, usually qsdb>
TASTER_PORT=<port number>
```

Run the docker container while loading the environment variables

``` sh
docker run -p 8085:80 --env-file .env --name taster -d tastermonial
```

## Database

The app requires a database. For testing purposes, a pre-prepared Sqlite database will work fine, though it is currently not included in this repo.

Database related features are handled by the separate package `tasterdb`, which expects all Libreview files and other raw data to be in the directory specified by:

``` r
config::get("tastermonial")$datadir
```

Using `tasterdb`, you can fill the database using a data directory that contains:

-   All Libreview CSV files, exactly as downloaded from <https://libreview.com>. Any CSV file that includes "glucose" in the name will automatically be read into the database.
-   Latest Tastermonial firebase output, stored in the file `table-data.csv`.
-   Miscellaneous other raw files including Nutrisense-formatted files.

Load the local Postgres database automatically like this:

``` r
my_local_db <- tasterdb::load_db("local")
```

To populate the remote, AWS-stored database or any other specified in your `config.yml`, pass the string name to `tasterdb::load_db()`.

See `tasterdb` instructions for more details.

## Testing

Run unit tests with `devtools::test()` (or type CMD-SHIFT-T)

## Deploy

Run the script `psi_deploy.R`.

Go to: <https://personalscience.shinyapps.io/Tastermonial/>
