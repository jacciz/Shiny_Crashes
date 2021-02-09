FROM rocker/shiny

RUN /rocker_scripts/install_geospatial.sh

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
    libssl-dev \
	libudunits2-dev
	#libproj-dev\
	#libxml2-dev \
	#libgdal-dev
# added last 4 because of sf, a geospatial package


## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## app folder
COPY . ./app
## renv.lock file
COPY /renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# install other R packages required
  RUN R -e "install.packages(c('littler', 'shinydashboard','shinyWidgets', 'plotly', 'leaflet', 'dplyr', 'ggplot2', 'lubridate', 'leaflet.extras2', 'tibble', 'data.table', 'fst', 'dashboardthemes', 'sf', 'Rmpfr', 'leafgl'), repos='https://cloud.r-project.org/')"

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
#CMD ["R", "-e", "shiny::runApp('/root/crash_dashboard')"]
