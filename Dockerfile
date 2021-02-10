
FROM rocker/shiny-verse:4.0.3

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
      org.label-schema.vendor="Rocker Project" \
      maintainer="Carl Boettiger <cboettig@ropensci.org>"

RUN R -e "install.packages( 'BiocManager', repos='https://cloud.r-project.org/')"
RUN /rocker_scripts/install_geospatial.sh

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
RUN R -e "install.packages(c('shinydashboard','shinyWidgets', 'plotly', 'leaflet', 'lubridate', 'leaflet.extras2', 'data.table', 'fst', 'dashboardthemes', 'sf', 'leafgl'), repos='https://cloud.r-project.org/')"

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
#CMD ["R", "-e", "shiny::runApp('/root/crash_dashboard')"]
