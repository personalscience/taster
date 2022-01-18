# Taster

This is a Golem-based implementation of an R Shiny app to view, explore, and analyze CGM glucose results obtained from [Tastermonial](https://tastermonial.com) users.

## Requirements

Develop the app as a package in RStudio. Like other Golem-based packages, by convention the `dev` directory is reserved for non-package-related setup and miscellaneous.

## Docker

You should be able to run the app from the Dockerfile, which will take care of all the software versions and setup. You'll need to connect to a database (see below), the credentials of which are kept in the local environment variables.

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

Or use Docker Compose

```yml
version: "3"
services:
  web:
    build: .
    ports:
      - "8086:80"
	  env_file:
      - taster.env
	  volumes:
      - ./config:/inst

```
Assuming you have the above `.env` file in `taster.env`, you can run like this
```sh
docker-compose up -d
```
and the app will appear on port 8086 of your host.


## Database

The app requires a database. For testing purposes, a pre-prepared Sqlite database will work fine, though it is currently not included in this repo.

Pre-loading and other Tastermonial-specific database features are in the separate package [`tasterdb`](https://github.com/personalscience/tasterdb).


## Testing

Run unit tests with `devtools::test()` (or type CMD-SHIFT-T)

## Deploy

See the script [`dev/psi_deploy.R`](./dev/psi_deploy.R) for specific deployment prerequisites. 

I'm able to load and host the web app on DigitalOcean by simply loading this Github repo without changes directly as a [DigitalOcean app](https://cloud.digitalocean.com/apps).  You will of course need to set environment variables in order to run the database.



