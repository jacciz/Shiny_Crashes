FROM openanalytics/r-base

LABEL maintainer "Tobias Verbeke <tobias.verbeke@openanalytics.eu>"
# https://github.com/rocker-org/shiny/issues/60 # for spatial stuff
# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
	
	libudunits2-dev\
	libproj-dev\
	libxml2-dev \
	libgdal-dev
	
# added the last 4 because of sf, a geospatial package

# system library dependency for the crash_dashboard app
RUN apt-get update && apt-get install -y \
    libmpfr-dev
	
# RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
# RUN R -e "install.packages('rgdal', repos='http://R-Forge.R-project.org')"

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install other R packages required 
RUN R -e "install.packages(c('littler', 'shinydashboard','shinyWidgets', 'DT', 'plotly', 'leaflet', 'dplyr', 'ggplot2', 'lubridate', 'leaflet.extras2', 'tibble', 'data.table', 'fst', 'dashboardthemes', 'sf', 'Rmpfr'), repos='https://cloud.r-project.org/')"

# RUN R -e "devtools::install_github('nik01010/dashboardthemes')"
# RUN installGithub.r nik01010/dashboardthemes

# RUN R -e "install.packages('sf', repos='https://cloud.r-project.org/')"

# install dependencies of the crash_dashboard app
# RUN R -e "install.packages('Rmpfr', repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/crash_dashboard
COPY . /root/crash_dashboard

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/crash_dashboard')"]