library(data.table)
library(dplyr)
library(lubridate)

setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/CSV") # don't need this when uploading to server
# file_loc = "Data Prep for R Shiny/"
file = "W:/HSSA/Keep/Jaclyn Ziebert/R/CSV/"
# This script imports data from a CSV, selects certain columns, add columns (such as newtime and age group),
# then exports to an RDS file. global.R will read this RDS file. Raw data must be put in 'data/' file
import_all_crashes <- function(csv_name, file_loc = file) {
  all_crashes <-
    fread(paste0(file_loc, csv_name, ".csv", sep = ""), sep = ",", header = TRUE, nrows = 200,
          select = c("CRSHNMBR", "CRSHDATE", "CRSHTIME", "CRSHMTH", "TOTINJ", "TOTFATL",
                     "DAYNMBR", "CNTYCODE", "MUNICODE", "URBRURAL", "CRSHSVR", "MNRCOLL",
                     "ALCFLAG", "DRUGFLAG", "BIKEFLAG", "CYCLFLAG", "PEDFLAG")
          )
  all_crashes$CRSHDATE <- ymd(all_crashes$CRSHDATE)       # convert to date type
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
 saveRDS(all_crashes, file = paste0(file_loc, csv_name, ".rds"))
}

import_all_persons <- function(csv_name, file_loc = file) {
  all_persons <-
    fread(paste0(file_loc, csv_name, ".csv", sep = ""), sep = ",", header = TRUE,
          select = c("CRSHNMBR", "CRSHDATE", "CNTYCODE", "MUNICODE", "WISINJ", "SFTYEQP", "ROLE", "SEX", "AGE", "HLMTUSE")
    )
  all_persons$CRSHDATE <- ymd(all_persons$CRSHDATE)      # convert to date type
  all_persons <- all_persons %>% mutate(age_group = cut(
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
  saveRDS(all_persons, file = paste0(file_loc, csv_name, ".rds"))
}

import_all_vehicles <- function(csv_name, file_loc = file) {
  all_vehicles <-
    fread(paste0(file_loc, csv_name, ".csv", sep = ""), sep = ",", header = TRUE, # nrows = 200,
          select = c("CRSHNMBR", "CRSHDATE", "CNTYCODE", "MUNICODE", "VEHTYPE")
    )
  all_vehicles$CRSHDATE <- ymd(all_vehicles$CRSHDATE)      # convert to date type
  saveRDS(all_vehicles, file = paste0(file_loc, csv_name, ".rds"))
}


all_crashes <- import_all_crashes("crash")
# Note: Creates a newtime field. time of 0 and 999 will be NA
# 
all_persons <- import_all_persons("person")
# Note: Creates a age_group field

all_vehicles <- import_all_vehicles("vehicle")


# county_recode <- fread("Data Prep for R Shiny/county_recode.csv")
# muni_recode <- fread("Data Prep for R Shiny/muni_recode.csv")

saveRDS(county_recode, file = "Shiny_Crashes_Dashboard/data/county_recode.rds")
saveRDS(muni_recode, file = "Shiny_Crashes_Dashboard/data/muni_recode.rds")
test <- readRDS("W:/HSSA/Keep/Jaclyn Ziebert/R/CSV/person.rds")
