library(data.table)
library(dplyr)
library(lubridate)
library(memisc)
# library(sjmisc)

setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny")
# file_loc = "Data Prep for R Shiny/"
file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny/"
# This script imports data from a CSV, selects certain columns, add columns (such as newtime and age group),
# then exports to an RDS file. global.R will read this RDS file. Raw data must be put in 'data/' file
import_all_crashes <- function(csv_name, file_loc = file) {
  all_crashes <-
    fread(paste0(file_loc, csv_name, ".csv", sep = ""), sep = ",", header = TRUE,
          select = c("CRSHNMBR", "CRSHSVR", "INJSVR", "CRSHDATE", "CRSHTIME", "CRSHMTH", "TOTINJ", "TOTFATL",
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
    fread(
      paste0(file_loc, csv_name, ".csv", sep = ""),
      sep = ",",
      header = TRUE,
      nrows = 10000,
      select = c(
        "CRSHNMBR",
        "CRSHSVR",
        "INJSVR",
        "CRSHDATE",
        "CNTYCODE",
        "MUNICODE",
        paste0( "DRVRPC", c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")),
        paste0( "STATNM", c("01","02","03","04","05","06","07","08","09","10")),
        "WISINJ",
        "SFTYEQP",
        "ROLE",
        "SEX",
        "AGE",
        "HLMTUSE"
      )
    )
  all_persons$CRSHDATE <- ymd(all_persons$CRSHDATE)      # convert to date type
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
                                                        SEX == "U" ~ "Unknown"))

  # saveRDS(all_persons, file = paste0(file_loc, csv_name, ".rds"))
}

import_all_vehicles <- function(csv_name, file_loc = file) {
  all_vehicles <-
    fread(paste0(file_loc, csv_name, ".csv", sep = ""), sep = ",", header = TRUE, # nrows = 200,
          select = c("CRSHNMBR", "INJSVR", "CRSHSVR", "CRSHDATE", "CNTYCODE", "MUNICODE", "VEHTYPE")
    )
  all_vehicles$CRSHDATE <- ymd(all_vehicles$CRSHDATE)      # convert to date type
  saveRDS(all_vehicles, file = paste0(file_loc, csv_name, ".rds"))
}


all_crashes <- import_all_crashes("crash")
# Note: Creates a newtime field. time of 0 and 999 will be NA
# 
all_persons <- import_all_persons("person")
# Note: Creates a age_group field, relabels ROLE, SEX

all_vehicles <- import_all_vehicles("vehicle")

# To import county and muni recode to get names
# county_recode <- fread("Data Prep for R Shiny/county_recode.csv")
# muni_recode <- fread("Data Prep for R Shiny/muni_recode.csv")
# 
# saveRDS(county_recode, file = "Shiny_Crashes_Dashboard/data/county_recode.rds")
# saveRDS(muni_recode, file = "Shiny_Crashes_Dashboard/data/muni_recode.rds")

# rbind() to combine df vertically


find_speed_statnm <- function(row) {  # input a row for iteration
  statnm_list <- # all the STATNM columns
    paste0("STATNM",
           c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"))
  statnm = list()
  for (stat in statnm_list) { # loop through columns finding if statutes in row
    statoutput <- grepl("346.56|346.57|346.58|346.59 ", row[, stat])
    statnm <- c(statnm, statoutput)
  }
  if (TRUE %in% statnm) { # return true if at least 1 statutes matches
    return (TRUE)
  } else {
    return (FALSE)
  }
}

get_list_speedflags <- function(persons_df){
  for (i in 1:nrow(persons_df)) {
    row <- persons_df[i, ] #1274
    persons_df[i , "speedflag"] = find_speed_statnm(row)
  }
  speedflag_crshes <-
    persons_df %>% dplyr::select(c(speedflag, ROLE, CRSHNMBR)) %>% filter(speedflag == TRUE, ROLE == "Driver")
  return (speedflag_crshes)
}
speedflag_crshes <- get_list_speedflags(all_persons)
