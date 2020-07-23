library(data.table)
library(dplyr)
library(lubridate)
library(memisc)
library(gdata)
library(fst)

# This script return a dataframe with a list of crshnmbr with flags for each year. The script finds crashes with a certain flag (i.e. older driver, speed),
# adds a column for each flag type with "Y" denotes crshnmb has that flag. Then it combines all flags into one dataframe and saves as an FST. 
# Exported data must be put in 'data/' file. Do this for each year.

# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny")  # set WD
# file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny/" # where CSV are saved
# file = "C:/CSV/csv_from_sas/from_sas_csv/"                    # where CSV are saved

# Functions to import databases from a CSV, grabs FLAG fields and fields to determine if a type of FLAG
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
        "CRSHDATE",
        "ALCFLAG",
        "DRUGFLAG",
        "BIKEFLAG",
        "CYCLFLAG",
        "PEDFLAG",
        "CMVFLAG"
      )
    )
  # Relabel so in consistent format
  all_crashes <- all_crashes %>% mutate(
    ALCFLAG = case_when(ALCFLAG == "Yes" ~ "Y",
                        ALCFLAG == "No" ~ "N"),
    DRUGFLAG = case_when(DRUGFLAG == "Yes" ~ "Y",
                         DRUGFLAG == "No" ~ "N")
  )
}

# Import the data from 'file', select the crash year CSV
all_crashes <- import_all_crashes("crash20")
all_persons <- import_all_persons("person20")

# Functions to get a list when a flag is found
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

# Run the functions, these all return a list of crshnmbers and a column of the respected flag(s)
speedflag_crshes <- get_list_speedflags(all_persons)
teenflag_crshes <- get_list_teendrvrflags(all_persons)
olderflag_crshes <- get_list_olderdrvrflags(all_persons)
allcrashflag_crshes <- get_list_crashflags(all_crashes)


# Combine dataframes
all_flags <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "CRSHNMBR"), list(speedflag_crshes, teenflag_crshes, olderflag_crshes, allcrashflag_crshes)) # combine to one df

#  Save crash flags as an RDS or FST, no compression so they open faster and are larger in size
# saveRDS(all_flags, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/crsh_flags17.rds") # save final crash flags df into rds, CHANGE YEAR
# saveRDS(all_flags, file = "C:/W_shortcut/Shiny_Crashes_Dashboard/data/crsh_flags20.rds", compress = FALSE) # save final crash flags df into rds, CHANGE YEAR

# write_fst(all_flags, path = "C:/W_shortcut/Shiny_Crashes_Dashboard/data/crsh_flags20.fst", compress = 0) # save final crash flags df into rds, CHANGE YEAR
# saveRDS(all_persons, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/all_persons_crsh_flags.rds") # save this once, just in case
