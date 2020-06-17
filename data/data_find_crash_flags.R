library(data.table)
library(dplyr)
library(lubridate)
library(memisc)
library(gdata)

# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny")
# file_loc = "Data Prep for R Shiny/"
# file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny/"

import_all_persons <- function(csv_name, file_loc = file) {
  all_persons <-
    fread(
      paste0(file_loc, csv_name, ".csv", sep = ""),
      sep = ",",
      header = TRUE,
      # nrows = 10000,
      select = c(
        "CRSHNMBR",
        paste0("DRVRPC", c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")),
        paste0("STATNM", c("01","02","03","04","05","06","07","08","09","10")),
        "SFTYEQP",
        "ROLE",
        "AGE",
        "HLMTUSE"
      )
  )
}

import_all_crashes <- function(csv_name, file_loc = file) {
  all_crashes <-
    fread(
      paste0(file_loc, csv_name, ".csv", sep = ""),
      sep = ",",
      header = TRUE,
      # nrows = 200,
      select = c(
        "CRSHNMBR",
        "ALCFLAG",
        "DRUGFLAG",
        "BIKEFLAG",
        "CYCLFLAG",
        "PEDFLAG"
      )
    )
  
  all_crashes <- all_crashes %>% mutate(
    ALCFLAG = case_when(ALCFLAG == "Yes" ~ "Y",
                        ALCFLAG == "No" ~ "N"),
    DRUGFLAG = case_when(DRUGFLAG == "Yes" ~ "Y",
                         DRUGFLAG == "No" ~ "N")
  )
}


# Import the data
# all_crashes <- import_all_crashes("crash")
# all_persons <- import_all_persons("person")
# all_persons <- readRDS(file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/all_persons_crsh_flags.rds") #alternatively, load this


# Functions to get a list when flag == Y #### grepl("^346.55|^346.56|^346.57|^346.58|^346.59(1)|^346.59(2)", thisrow)
get_list_speedflags <- function(persons_df) {
  speedflags <-
    persons_df %>% filter(ROLE == 'Driver', apply(., 1, function(thisrow)
      # this selects all rows where a speeding criteria is met
      any(   # the criteria (any of these)
        thisrow %in% c("Exceed Speed Limit",
                       "Speed Too Fast/Cond") |
          grepl("^346.55|^346.56|^346.57|^346.58|^346.59(1)|^346.59(2)", thisrow) #^ means that beginning must match
      )) == TRUE) %>% mutate(speedflag = "Y")
  speedflags <-
    speedflags %>% dplyr::select(CRSHNMBR, speedflag) # %>% group_by(CRSHNMBR)
  speedflags <- unique(speedflags[])
  return (speedflags)
}
get_list_teendrvrflags <- function(persons_df) {
  teendrvrflags <- persons_df %>% dplyr::select(CRSHNMBR, AGE, ROLE) %>% filter(AGE %in% c(16, 17, 18, 19), ROLE == 'Driver') %>% mutate(teenflag = "Y")
  teendrvrflags <- teendrvrflags %>% dplyr::select(CRSHNMBR, teenflag) # %>% group_by(CRSHNMBR)
  teendrvrflags <- unique(teendrvrflags[]) # dplyr::filter(!is.na(CRSHNMBR))
  return (teendrvrflags)
}
get_list_olderdrvrflags <- function(persons_df) {
  olderdrvrflags <- persons_df %>% dplyr::select(CRSHNMBR, AGE, ROLE) %>% filter(AGE >= 65, ROLE == 'Driver') %>% mutate(olderflag = "Y")
  olderdrvrflags <- olderdrvrflags %>% dplyr::select(CRSHNMBR, olderflag) #dplyr::filter(!is.na(CRSHNMBR))# %>% group_by(CRSHNMBR)
  olderdrvrflags <- unique(olderdrvrflags[])
  return (olderdrvrflags)
}
get_list_crashflags <- function(crashes_df){
  crash_flags <- crashes_df %>% filter(apply(., 1, function(thisrow)
    any(thisrow %in% "Y"))) # returns any row where there is at least 1 flag
}

# Run the functions
# speedflag_crshes <- get_list_speedflags(all_persons) # list of crshnmbers
# teenflag_crshes <- get_list_teendrvrflags(all_persons) # list of crshnmbers
# olderflag_crshes <- get_list_olderdrvrflags(all_persons) # list of crshnmbers
# allcrashflag_crshes <- get_list_crashflags(all_crashes) # list of crshnmbers


# Combine and save crash flags as an RDS
# all_flags <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "CRSHNMBR"), list(speedflag_crshes, teenflag_crshes, olderflag_crshes, allcrashflag_crshes)) # combine to one df
# saveRDS(all_flags, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/crsh_flags.rds") # save final crash flags df into rds
# saveRDS(all_persons, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/all_persons_crsh_flags.rds") # save this once, just in case