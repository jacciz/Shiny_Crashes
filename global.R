library(fst) # loads data really fast
library(dplyr)
library(data.table)
library(sf)

# source("data/data_import.R") # this scripts creates data so this script imports data for the app

# This global.R script loads all data files (crash, person, vehicle, crsh_flags) in data/

# This script also loads county_recode, muni_recode, and county.shp

# TO GET THIS DATA YOU MUST RUN data_import.R and data_find_crash_flags.R that loads a CSV
# that grabs columns and adds new ones then it saves as an FST (must do this for each year)
# THEN use combine_data.R to combine all these databases of each year into a large database
# THIS loads that large database of each type (crash,person,vehicle,crsh_flags)

# Note: Creates a newtime variable - time of 0 and 999 is NA
all_crashes <- read_fst("data/all_crash", as.data.table = TRUE)

# Note: Creates an age_group variable
all_persons <- read_fst("data/all_person", as.data.table = TRUE)

all_vehicles <- read_fst("data/all_vehicle", as.data.table = TRUE)

# speedflag, teenflag, olderflag ("Y"), all other crash flags
all_crsh_flags <- read_fst("data/all_crsh_flags", as.data.table = TRUE)

# These are to get county/muni names
county_recode <- readRDS("data/county_recode.rds")
muni_recode <- readRDS("data/muni_recode.rds")

# Read shapefiles for map
# ctv <- st_read("map/ctv.shp")
county <- st_read("map/county.shp")

# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactLog() for issues with reactivity

# options(shiny.reactlog = TRUE) then ctrl-f3 in browser

# all_crashes %>% select(CRSHNMBR, CRSHTIME, newtime) %>% View() # to compare time

