library(data.table)
library(dplyr)
library(lubridate) ### MAY have to change date to mdy, ugh formatting
library(memisc)
# library(sjmisc)

year = "20"

# This script basically readies the data for the dashboard and is exported into teh SQLite database.

# This script imports data from a CSV, selects certain columns, add new columns (such as newtime and age group)


# file_loc = "Data Prep for R Shiny/"
# file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny/"
file = "C:/CSV/csv_from_sas/from_sas_csv/" # this is where the raw CSVs are and where data will be saved

import_all_crashes <- function(csv_name, file_loc = file) {
  all_crashes <-
    fread(paste0(file_loc, csv_name, ".csv", sep = ""), sep = ",", header = TRUE,
          select = c("CRSHNMBR", "CRSHSVR", "INJSVR", "CRSHDATE", "CRSHTIME", "CRSHMTH", "TOTINJ", "TOTFATL", "TOTUNIT",
                     "DAYNMBR", "CNTYCODE", "MUNICODE", "URBRURAL", "MNRCOLL", "LATDECDG", "LONDECDG")
          ) #  "ALCFLAG", "DRUGFLAG", "BIKEFLAG", "CYCLFLAG", "PEDFLAG"
  all_crashes <-
    all_crashes %>% mutate(CRSHDATE = as.character(CRSHDATE)) # convert to date type
  all_crashes <- all_crashes %>% mutate(newtime = cut(  # this finds crash time by hour
    CRSHTIME,
    c(
      1,
      100,
      200,
      300,
      400,
      500,
      600,
      700,
      800,
      900,
      1000,
      1100,
      1200,
      1300,
      1400,
      1500,
      1600,
      1700,
      1800,
      1900,
      2000,
      2100,
      2200,
      2300,
      2400
    ),
    labels = c(
      "12am",
      "1am",
      "2am",
      "3am",
      "4am",
      "5am",
      "6am",
      "7am",
      "8am",
      "9am",
      "10am",
      "11am",
      "12pm",
      "1pm",
      "2pm",
      "3pm",
      "4pm",
      "5pm",
      "6pm",
      "7pm",
      "8pm",
      "9pm",
      "10pm",
      "11pm"
    ),
    include.lowest = T
  ))
  
  setnames(all_crashes, "LONDECDG", "lng") # rename so leaflet grabs correct columns
  setnames(all_crashes, "LATDECDG", "lat")
  
 # saveRDS(all_crashes, file = paste0(file_loc, csv_name, ".rds"), compress = FALSE)
 # write_fst(all_crashes, path = paste0(file_loc, csv_name, ".fst"), compress = 0)
}

import_all_persons <- function(csv_name, file_loc = file) {
  all_persons <-
    fread(
      paste0(file_loc, csv_name, ".csv", sep = ""),
      sep = ",",
      header = TRUE,
      # nrows = 10000,
      select = c(
        "CRSHNMBR",
        "CRSHSVR",
        "INJSVR",
        "CRSHDATE",
        "CNTYCODE",
        "MUNICODE",
        "WISINJ",
        "ROLE",
        "SEX",
        "AGE",
        "HLMTUSE",
        paste0("NMTACT", c("01","02","03","04","05","06","07","08","09","10","11","12")),
        "NMTLOC",
        paste0("DRVRPC", c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
      )
    )
  all_persons <-
    all_persons %>% mutate(CRSHDATE = as.character(CRSHDATE)) # convert to date type
  all_persons <- all_persons %>% mutate(age_group = cut( # add age_group column, 5 year intervals
    AGE,
    c(0,
      4,
      9,
      14,
      19,
      24,
      29,
      34,
      39,
      44,
      49,
      54,
      59,
      64,
      69,
      120),
    labels = c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70+"
    ),
    include.lowest = T))

  all_persons <- all_persons %>% mutate(  # relabel ROLE
    ROLE = case_when(
      ROLE == "Driver" ~ "Driver",
      ROLE == "Passenger" ~ "Passenger",
      ROLE == "Pedestrian" ~ "Pedestrian",
      ROLE == "Other Pedestrian" ~ "Pedestrian",
      ROLE == "Bicyclist" ~ "Bicyclist",
      ROLE == "Other Cyclist" ~ "Bicyclist",
      ROLE == "Occupant Of Motor Vehicle Not In Transport" ~ "Other",
      ROLE == "Occupant Of Non-Motor Vehicle Transportation Device" ~ "Other",
      ROLE == "Unknown Type Of Non-Motorist" ~ "Other",
      ROLE == "Pedestrian (Non-Occupant)" ~ "Other",
      ROLE == "Unknown" ~ "Other"
    )
  )
  all_persons <- all_persons %>% mutate(SEX = case_when(SEX == "F" ~ "Female", # relabel SEX
                                                        SEX == "M" ~ "Male",
                                                        SEX == "U" ~ "Unknown")) %>% 
    mutate_at(vars(starts_with(c("DRVRPC","NMTACT"))), as.character)

  # saveRDS(all_persons, file = paste0(file_loc, csv_name, ".rds"), compress = FALSE)
  # write_fst(all_persons, path = paste0(file_loc, csv_name, ".fst"), compress = 0)
}

import_all_vehicles <- function(csv_name, file_loc = file) {
  all_vehicles <-
    fread(paste0(file_loc, csv_name, ".csv", sep = ""), sep = ",", header = TRUE, # nrows = 200,
          select = c("CRSHNMBR", "INJSVR", "CRSHSVR", "CRSHDATE", "CNTYCODE", "MUNICODE", "VEHTYPE")
    )
  all_vehicles <-
    all_vehicles %>% mutate(
      CRSHDATE = as.character(CRSHDATE),
      vehcate = case_when(
        VEHTYPE == "Passenger Car" ~ "Passenger Veh.",
        VEHTYPE == "(Sport) Utility Vehicle" ~ "Passenger Veh.",
        VEHTYPE == "Cargo Van (10,000 Lbs or Less)" ~ "Passenger Veh.",
        VEHTYPE == "Passenger Van" ~ "Passenger Veh.",
        VEHTYPE == "Utility Truck/Pickup Truck" ~ "Light Trucks",
        VEHTYPE == "Straight Truck" ~ "Large Trucks",
        VEHTYPE == "Truck Tractor (Trailer Not Attached)" ~ "Large Trucks",
        VEHTYPE == "Truck Tractor (Trailer Attached)" ~ "Large Trucks",
        VEHTYPE == "Truck Tractor (More Than One Trailer)" ~ "Large Trucks",
        VEHTYPE == VEHTYPE ~ "Other"
      )
    ) # convert to date type
  # saveRDS(all_vehicles, file = paste0(file_loc, csv_name, ".rds"), compress = FALSE)
  # write_fst(all_vehicles, path = paste0(file_loc, csv_name, ".fst"), compress = 0)
}

# input is name of csv, just change year
all_crashes <- import_all_crashes(paste0(year,"crash"))
# Note: Creates a newtime field. time of 0 and 999 will be NA
#
all_persons <- import_all_persons(paste0(year,"person"))
# Note: Creates a age_group field, relabels ROLE, SEX

all_vehicles <- import_all_vehicles(paste0(year,"vehicle"))

# To import county and muni recode to get names
# county_recode <- fread("Data Prep for R Shiny/county_recode.csv")
# muni_recode <- fread("Data Prep for R Shiny/muni_recode.csv")
#
# saveRDS(county_recode, file = "Shiny_Crashes_Dashboard/data/county_recode.rds")
# saveRDS(muni_recode, file = "Shiny_Crashes_Dashboard/data/muni_recode.rds")


#### SAVE TO SQLITE
fname = paste0("20",year)
pool <- pool::dbPool(RSQLite::SQLite(), dbname = "inst/app/www/crash_db.db")

DBI::dbWriteTable(pool, paste0(fname,"crash"), all_crashes)
DBI::dbWriteTable(pool, paste0(fname,"person"), all_persons)
DBI::dbWriteTable(pool, paste0(fname,"vehicle"), all_vehicles)
# make CRSHNMBR primary key