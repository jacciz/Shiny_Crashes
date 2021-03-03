
FROM rocker/shiny-verse:4.0.3
# Rocker image includes R, Shiny, and tidyverse

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
      org.label-schema.vendor="Rocker Project" \
      maintainer="Carl Boettiger <cboettig@ropensci.org>"

# RUN R -e "install.packages( 'BiocManager', repos='https://cloud.r-project.org/')"
RUN /rocker_scripts/install_geospatial.sh

# copy necessary files
## app folder
COPY . ./app
## renv.lock file
# COPY /renv.lock ./renv.lock

# Don't need tidyverse as that is in the Rocker image
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.5")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.5.3")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.31")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.0")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-7")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.4.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.7")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.6")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.7")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.2.3")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.3")'
RUN Rscript -e 'remotes::install_version("leafgl",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.10")'
RUN Rscript -e 'remotes::install_version("leaflet.extras2",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.17")'
RUN Rscript -e 'remotes::install_version("dashboardthemes",upgrade="never", version = "1.1.3")'

# expose port
EXPOSE 3838

#CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');crash_dashboard::run_app()"

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
