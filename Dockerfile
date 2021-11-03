FROM rocker/shiny-verse:latest
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libpq-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN R -e 'install.packages(c("RPostgres", "DescTools", "ggplot2", "golem", "DT", \
                              "magrittr", "tibble", "shiny", "rlang", "lubridate", "DBI", "dplyr", "readr"), \
  repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.1.1")'
RUN Rscript -e 'remotes::install_github("personalscience/cgmr@f4c17f245f6e0ddf3ed32a4257dd9140db74e055")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');tastermonial::run_app()"
