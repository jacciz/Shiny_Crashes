# library(readr)
library(dplyr)
library(data.table) # fread
# library(grDevices)  # for fonts
# source("data/data_import.R") # import functions from this script to import data

Sys.setenv("plotly_username" = "jacciz")   # to use plotly, this is my token
Sys.setenv("plotly_api_key" = "wrczHh7hA58lbPmrZ4jz")


setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard") # don't need this when uploading to server

all_crashes <- readRDS("data/all_crashes.rds")
# Note: Creates a newtime field. time of 0 and 999 will be NA

# all_persons <- readRDS("data/all_persons.rds")
# Note: Creates a age_group field

all_vehicles <- readRDS("data/all_vehicles.rds")
county_recode <- readRDS("data/county_recode.rds")
muni_recode <- readRDS("data/muni_recode.rds")

# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactLog() for issues with reactivity

# options(shiny.reactlog = TRUE) then ctrl-f3 in browser


# all_crashes %>% select(CRSHNMBR, CRSHTIME, newtime) %>% View() #to compare time
