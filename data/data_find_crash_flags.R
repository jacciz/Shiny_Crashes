library(data.table)
library(dplyr)
library(lubridate)
library(memisc)

setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny")
# file_loc = "Data Prep for R Shiny/"
file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny/"

import_all_persons <- function(csv_name, file_loc = file) {
  all_persons <-
    fread(
      paste0(file_loc, csv_name, ".csv", sep = ""),
      sep = ",",
      header = TRUE,
      nrows = 10000,
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
# saveRDS(all_persons, file = paste0(file_loc, csv_name, ".rds"))

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
