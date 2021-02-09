FROM openanalytics/r-base

LABEL maintainer "Tobias Verbeke <tobias.verbeke@openanalytics.eu>"
# https://github.com/rocker-org/shiny/issues/60 # for spatial stuff
# system libraries of general use
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev
	libudunits2-dev\
	libproj-dev\
	libxml2-dev \
	libgdal-dev

# added last 4 because of sf, a geospatial package

# system library dependency for the crash_dashboard app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install other R packages required
RUN R -e "install.packages(c('littler', 'shinydashboard','shinyWidgets', 'plotly', 'leaflet', 'dplyr', 'ggplot2', 'lubridate', 'leaflet.extras2', 'tibble', 'data.table', 'fst', 'dashboardthemes', 'sf', 'Rmpfr', 'leafgl'), repos='https://cloud.r-project.org/')"

# RUN R -e "devtools::install_github('nik01010/dashboardthemes')"
# RUN installGithub.r nik01010/dashboardthemes

# copy the app to the image
RUN mkdir /root/crash_dashboard
COPY . /root/crash_dashboard

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/crash_dashboard')"]
