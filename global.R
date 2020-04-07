# library(readr)
library(dplyr)
library(data.table)
# library(grDevices)  # for fonts
source("data/data_import.R") # import functions from this script to import data

Sys.setenv("plotly_username" = "jacciz")   # to use plotly, this is my token
Sys.setenv("plotly_api_key" = "wrczHh7hA58lbPmrZ4jz")


# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R_WD_Data/Shiny_Crashes") # don't need this when uploading to server

all_crashes <- import_all_crashes("2019_crash")
# Note: Creates a newtime field. Should time 0 be NA?

# all_persons <- import_all_persons("2019_person")
# Note: Creates a age_group field

# all_vehicles <- import_all_vehicles("2019_vehicle")

county_recode <- fread("data/county_recode.csv")
muni_recode <- fread("data/muni_recode.csv")


# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactLog() for issues with reactivity

# options(shiny.reactlog = TRUE) then ctrl-f3 in browser


# all_crashes %>% select(CRSHNMBR, CRSHTIME, newtime) %>% View() #to compare time
