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
all_persons <- import_all_persons("person")
# all_persons <- readRDS(file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/all_persons_crsh_flags.rds") #alternatively, load this

get_list_speedflags <- function(persons_df) {
  speedflags <-
    persons_df %>% filter(ROLE == 'Driver', apply(., 1, function(thisrow)
      # this selects all rows where a speeding criteria is met
      any(   # the criteria (any of these)
        thisrow %in% c("Exceed Speed Limit",
                       "Speed Too Fast/Cond") |
          grepl("^346.56|^346.57|^346.58|^346.59", thisrow) & !grepl("^346.595", thisrow) #^ means that beginning must match
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

speedflag_crshes <- get_list_speedflags(all_persons) # list of crshnmbers
teenflag_crshes <- get_list_teendrvrflags(all_persons) # list of crshnmbers
olderflag_crshes <- get_list_olderdrvrflags(all_persons) # list of crshnmbers


# all_flags <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "CRSHNMBR"), list(speedflag_crshes, teenflag_crshes, olderflag_crshes)) # combine to one df
# saveRDS(all_flags, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/crsh_flags.rds")
# saveRDS(all_persons, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/all_persons_crsh_flags.rds") # save this once, just in case



# DELETE
# find_speed_drvrpc <- function(iirow) {  # input a row for iteration - returns TRUE or FALSE
#   drvrpc_list <- # all the STATNM columns
#     paste0("DRVRPC", c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
#     drvrpc = list()
#   for (iidrvrpc in drvrpc_list) { # loop through columns finding if statutes in row
#     drvrpc_output <- grepl("Exceed Speed Limit|Speed Too Fast/Cond", iirow[, iidrvrpc, with = FALSE])
#     drvrpc <- c(drvrpc, drvrpc_output)
#   }
#   if (TRUE %in% drvrpc) { # return true if at least 1 statutes matches
#     return (TRUE)
#   } else {
#     return (FALSE)
#   }
# } # inputs a iirow for iteration - returns TRUE or FALSE
# find_speed_statnm <- function(iirow) {  # input a iirow for iteration
#   statnm_list_nmbr <- # all the STATNM columns
#     paste0("STATNM",
#            c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"))
#   statnm_list = list()
#   for (statnm in statnm_list) { # loop through columns finding if statutes in iirow
#     statoutput <- grepl("346.56|346.57|346.58|346.59 ", iirow[, statnm, with = FALSE]) #with=FALSE since statnm in unquoted
#     statnm_list <- c(statnm_list, statoutput)
#   }
#   if (TRUE %in% statnm_list) { # return true if at least 1 statutes matches
#     return (TRUE)
#   } else {
#     return (FALSE)
#   }
# } # inputs a iirow for iteration - returns TRUE or FALSE
# 
# get_list_speedflags <- function(persons_df){
#   for (i in 1:nrow(persons_df)) {
#     iirow <- persons_df[i, ] #1274
#     persons_df[i , "stat"] = find_speed_statnm(iirow)
#     persons_df[i , "drvrpc"] =find_speed_drvrpc(iirow)
#   }
#   speedflag_crshes <-
#     persons_df %>% dplyr::select(c(stat, drvrpc, ROLE, CRSHNMBR)) %>% filter(stat == TRUE | drvrpc == TRUE, ROLE == "Driver") %>% mutate(speedflag = "Y")
#   return (speedflag_crshes)
# } # gets list of crshnmbrs of speed drvrpc/statnm


# "Exceed Speed Limit", DRVRPC01
# "Speed Too Fast/Cond",
# "346.56", STATNM01 # none
# "346.57",
# "346.58",
# "346.59("
# grepl("346.56")
# speedflags_test %>% filter(grepl("^346.56", STATNM01), speedflag == FALSE)
# speedflags_test %>% filter(startsWith(STATNM01, "346.57"))
# speedflags_test %>% filter(startsWith(STATNM01, "346.56"), speedflag == FALSE) # should return 0
# speedflags_test %>% filter(startsWith(DRVRPC01, "Exceed Speed"), speedflag == FALSE) # should return 0
# # 
# # 
# speedflags_test <- all_persons %>% filter(ROLE == 'Driver') %>% mutate(speedflag = apply(., 1, function(thisrow) # this selects all rows where a speeding criteria is met
#   any(thisrow %in% c(
#     "Exceed Speed Limit",
# "Speed Too Fast/Cond") | grepl("^346.56", thisrow) | grepl("^346.57", thisrow) | grepl("^346.58", thisrow) | grepl("^346.59", thisrow) & !grepl("^346.595", thisrow))))
# speedflags_test %>% filter(speedflag == TRUE) %>% dplyr::distinct(CRSHNMBR) %>% nrow()
