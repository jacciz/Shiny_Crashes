library(data.table)
library(dplyr)
library(memisc)
# library(sjmisc)

# don't need this scrips anymore

setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny")
# file_loc = "Data Prep for R Shiny/"
file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny/"
# This script imports data from a CSV, selects certain columns, add columns (such as newtime and age group),
# then exports to an RDS file. global.R will read this RDS file. Raw data must be put in 'data/' file
import_all_crashes <- function(csv_name, file_loc = file) {
  all_crashes <-
    fread(
      paste0(file_loc, csv_name, ".csv", sep = ""),
      sep = ",",
      header = TRUE,
      select = c("CRSHNMBR",
                 "CRSHSVR",
                 "LATDECDG",
                 "LONDECDG")
    )

  saveRDS(all_crashes, file = paste0(file_loc, "crash_lat_long", ".rds"))
}


# input is name of csv
all_crashes <- import_all_crashes("crash")
# Note: Creates a newtime field. time of 0 and 999 will be NA
