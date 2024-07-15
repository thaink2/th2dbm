FROM rocker/r-ver:4.3.2
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN apt-get update --fix-missing -qq && apt-get install -y -q \
       libsodium-dev \
       libssl-dev \
       libcurl4-gnutls-dev \
       libgit2-dev \
       unixodbc-dev\
       unixodbc \
       odbcinst1debian2 \
       odbcinst \
    && apt-get clean \
    && apt-get purge \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.7.0")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("uuid",upgrade="never", version = "1.2-0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("sodium",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("shinyDatetimePickers",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.3.5")'
RUN Rscript -e 'remotes::install_version("encryptr",upgrade="never", version = "0.1.3")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.31")'
RUN Rscript -e 'remotes::install_version("DatabaseConnector",upgrade="never", version = "6.3.2")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "2.3.3")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
