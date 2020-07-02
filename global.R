library(fst) # loads data really fast
library(dplyr)
library(data.table)

# source("data/data_import.R") # this scripts creates data so this scrips imports for the app

Sys.setenv("plotly_username" = "jacciz")   # to use plotly, this is my token
Sys.setenv("plotly_api_key" = "wrczHh7hA58lbPmrZ4jz")

# This script loads all data files (crash, person, vehicle) that is in FST in a single file location
# And combines into a single long format data.table
# This does so by grabbing all files with 'crash' (or person/vehicle) and FST in the name
# This script also loads crsh_flags, county_recode, and muni_recode

# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard") # don't need this when uploading to server
# setwd("C:/W_shortcut/Shiny_Crashes_Dashboard/")

# Function to import all data of type "databasetype" and is a FST and is in /data # https://gist.github.com/aammd/9ae2f5cce9afd799bafb
import_all_databases <- function(databasetype) {
  temp <- list.files(path = "data/", pattern = paste0("^\\", databasetype, ".*\\.fst$"))
  read_fst2 <- function(path) read_fst(paste0("data/", path))
  combined_data <- lapply(temp, read_fst2) %>% rbindlist()
  combined_data
}

# Note: Creates a newtime variable - time of 0 and 999 is NA
all_crashes <- import_all_databases("crash")

# Note: Creates an age_group variable
all_persons <- import_all_databases("person")

all_vehicles <- import_all_databases("vehicle")

# speedflag, teenflag, olderflag ("Y"), all other crash flags
all_crsh_flags <- import_all_databases("crsh_flags")

# These are to get county/muni names
county_recode <- readRDS("data/county_recode.rds")
muni_recode <- readRDS("data/muni_recode.rds")

# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactLog() for issues with reactivity

# options(shiny.reactlog = TRUE) then ctrl-f3 in browser

# all_crashes %>% select(CRSHNMBR, CRSHTIME, newtime) %>% View() # to compare time

