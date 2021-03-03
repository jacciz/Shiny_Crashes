library(data.table)
library(dplyr)
library(lubridate)
library(memisc)
library(gdata)
library(fst)

year = "20"
# This script return a dataframe with a list of crshnmbr with flags for each year. The script finds crashes with a certain flag (i.e. older driver, speed),
# adds a column for each flag type with "Y" denotes crshnmb has that flag. Then it combines all flags into one dataframe to save to SQLite db.

# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny")  # set WD
# file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Data Prep for R Shiny/" # where CSVs are saved
file = "C:/CSV/csv_from_sas/from_sas_csv/"                    # where CSVs are saved

# Functions to import databases from a CSV, grabs FLAG fields and fields to determine if a type of FLAG
import_all_crashes <- function(csv_name, file_loc = file) {
  all_crashes <-
    fread(
      paste0(file_loc, csv_name, ".csv", sep = ""),
      sep = ",",
      header = TRUE,
      # nrows = 200,
      select = c(
        "CRSHNMBR",
        "CRSHTYPE",
        "CRSHSVR",
        "CRSHDATE",
        "CNTYCODE",
        paste0("ANMLTY", c("01","02","03","04","05","06","07")),
        "MNRCOLL",
        "TOTUNIT",
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
                         DRUGFLAG == "No" ~ "N"),
    CRSHDATE = as.character(CRSHDATE)
  )
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
        "UNITNMBR",
        paste0("DRVRPC", c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")),
        paste0("STATNM", c("01","02","03","04","05","06","07","08","09","10")),
        paste0("DRVRDS", c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")),
        "SFTYEQP",
        "ROLE",
        "AGE",
        "HLMTUSE",
        "DISTACT"
      )
  )
}

# Import the data from 'file', select the crash year CSV. Must do each year separately.
all_crashes <- import_all_crashes(csv_name = paste0(year,"crash"))
all_persons <- import_all_persons(csv_name = paste0(year,"person"))

# Functions to find flags, returns flag column and crshnmbr. By list I mean a long df.
get_list_speedflags     <- function(persons_df) {
  speedflags <-
    persons_df %>% filter(
      ROLE == 'Driver') %>% 
        # this selects all rows where a speeding criteria is met
        filter_all(any_vars(
          grepl(
            "Exceed Speed Limit|Speed Too Fast/Cond|^346.55|^346.56|^346.57|^346.58|^346.59(1)|^346.59(2)",
            .
          ) #^ means that beginning must match
        )) %>% mutate(speedflag = "Y")
      speedflags <-
        speedflags %>% dplyr::select(CRSHNMBR, speedflag) # %>% group_by(CRSHNMBR)
      speedflags <- unique(speedflags[])
      return (speedflags)
}

get_list_distractedflags     <- function(persons_df) {
  distracted <- persons_df %>%
    dplyr::select(ROLE,
           CRSHNMBR,
           UNITNMBR,
           any_of(starts_with(c(
             "DISTACT", "DRVRDS"
           )))) %>%
    filter(ROLE == 'Driver') %>%
    filter_all(any_vars(
      grepl(
        "Talking|Manually Operating|Other Action|Manually Operating|Electronic Device|Passenger|Eating|Outside|Vehicle|Looked|Moving Object|Adjusting Audio|Outside Person|Smoking|Other Cellular|Inattention|Careless|Details Unknown$|Daydreaming|Other Distraction|Distraction/Inattention",
        .
      )
    ))
  # Find where 'Not distracted' is listed even though a distraction may have been listed
  not_distracted <- persons_df %>%
    dplyr::select(ROLE,
           UNITNMBR,
           CRSHNMBR,
           any_of(starts_with(c(
             "DISTACT", "DRVRDS"
           )))) %>%
    filter(ROLE == 'Driver') %>% filter_all(any_vars(!grepl(
      "Not Distracted|Unknown If Distracted", .
    ) == FALSE))
  combine <-
    # Remove all 'Not distracted' and add a column of distracted_flag
    anti_join(distracted, not_distracted, by = c("CRSHNMBR", "UNITNMBR")) %>%
    dplyr::select(CRSHNMBR, UNITNMBR, ROLE) %>% mutate(distracted_flag = "Y")
  
  distractflags <-
    left_join(persons_df, combine, by = c("CRSHNMBR", "UNITNMBR", "ROLE")) %>%
    mutate(distracted_flag = tidyr::replace_na(distracted_flag, "N")) %>%
    dplyr::select(CRSHNMBR, distracted_flag) # %>% group_by(CRSHNMBR)
  
  distractflags <- unique(distractflags[])
  return (distractflags)
}

get_list_teendrvrflags  <- function(persons_df) {
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
get_list_singlevehflags <- function(crashes_df) {
  singlevehflags <- crashes_df %>% dplyr::select(CRSHNMBR, TOTUNIT) %>% filter(TOTUNIT == 1) %>% mutate(singlevehflag = "Y")
  singlevehflags <- singlevehflags %>% dplyr::select(CRSHNMBR, singlevehflag) #dplyr::filter(!is.na(CRSHNMBR))# %>% group_by(CRSHNMBR)
  return (singlevehflags)
}
get_list_deerflags      <- function(crashes_df) {
  deerflags <- crashes_df %>%
    dplyr::select(CRSHTYPE, ANMLTY01:ANMLTY07, CRSHNMBR) %>%
    filter((
      CRSHTYPE == "Non Domesticated Animal (Alive)" |
        CRSHTYPE == "Non Domesticated Animal (Dead)"
    ) & apply(., 1, function(thisrow)
      any(thisrow %in% "Deer"))) %>% mutate(deerflag = "Y")
  deerflags <- deerflags %>% dplyr::select(CRSHNMBR, deerflag) #dplyr::filter(!is.na(CRSHNMBR))# %>% group_by(CRSHNMBR)
  return (deerflags)}
get_list_lanedepflags   <- function(crashes_df){
  lanedep_crshtypes = c( "Motor Veh Tran Other Rdwy", "Other Object - Not Fixed", "Traffic Sign Post",
                         "Traffic Signal", "Utility Pole", "Lum Light Support", "Other Post, Pole or Support",
                         "Tree", "Mailbox", "Guardrail Face", "Guardrail End", "Bridge Parapet End",
                         "Bridge/Pier/Abut", "Impact Attenuator/Crash Cushion", "Overhead Sign Post",
                         "Bridge Rail", "Culvert", "Ditch", "Curb", "Embankment", "Fence",
                         "Other Fixed Object", "Overturn/Rollover", "Jackknife", "Cable Barrier",
                         "Concrete Traffic Barrier", "Other Traffic Barrier", "Fire Hydrant", "Unknown")
  lanedep_mnrcoll = c("Front To Front", "Sideswipe/Same Direction", "Sideswipe/Opposite Direction")
  lanedepflags <- crashes_df %>% dplyr::select(CRSHNMBR, CRSHTYPE, MNRCOLL) %>% filter(CRSHTYPE %in% lanedep_crshtypes | MNRCOLL %in% lanedep_mnrcoll) %>% mutate(lanedepflag = "Y")
  lanedepflags <- lanedepflags %>% dplyr::select(CRSHNMBR, lanedepflag) #dplyr::filter(!is.na(CRSHNMBR))# %>% group_by(CRSHNMBR)
  return (lanedepflags)
}
# This finds crashes with a crash flag already in the crash db (i.e. BIKEFLAG)
get_list_crashflags     <- function(crashes_df){
  crash_flags <- crashes_df %>% dplyr::select(CRSHNMBR, ALCFLAG, DRUGFLAG, BIKEFLAG, CYCLFLAG, PEDFLAG, CMVFLAG)
  crash_flags <- crash_flags %>% filter(apply(., 1, function(thisrow)
    any(thisrow %in% "Y"))) # returns any row where there is at least 1 flag
}

# Run the functions, these all return a list of crshnmbers and a column of the respected flag(s)
speedflag_crshes    <- get_list_speedflags(all_persons)
distractflag_crshes <- get_list_distractedflags(all_persons)
teenflag_crshes     <- get_list_teendrvrflags(all_persons)
olderflag_crshes    <- get_list_olderdrvrflags(all_persons)
singleveh_crshes    <- get_list_singlevehflags(all_crashes)
deer_crshes         <- get_list_deerflags(all_crashes)
lanedep_crshes      <- get_list_lanedepflags(all_crashes)
allcrashflag_crshes <- get_list_crashflags(all_crashes)

# Combine dataframes - make sure all df are here
all_flags <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "CRSHNMBR"),
                    list(speedflag_crshes, distractflag_crshes, teenflag_crshes, olderflag_crshes,
                         singleveh_crshes, lanedep_crshes, deer_crshes, allcrashflag_crshes)) # combine to one df
all_flags_com = all_crashes %>% dplyr::select(CRSHNMBR, CRSHDATE, CNTYCODE, CRSHSVR) %>% left_join(all_flags, ., by = "CRSHNMBR")

# Save crash flags as an RDS or FST, no compression so they open faster and are larger in size
# saveRDS(all_flags, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/crsh_flags17.rds") # save final crash flags df into rds, CHANGE YEAR
# saveRDS(all_flags, file = "C:/W_shortcut/Shiny_Crashes_Dashboard/data/crsh_flags20.rds", compress = FALSE) # save final crash flags df into rds, CHANGE YEAR

# USE FST
# write_fst(all_flags, path = "C:/W_shortcut/Shiny_Crashes_Dashboard/crash_databases/17crsh_flags.fst", compress = 0) # save final crash flags df into rds, CHANGE YEAR

# saveRDS(all_persons, file = "W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/data/all_persons_crsh_flags.rds") # save this once, just in case

#### SAVE TO SQLITE
fname = paste0("20",year,"crsh_flags")
pool <- pool::dbPool(RSQLite::SQLite(), dbname = "inst/app/www/crash_db.db")
DBI::dbWriteTable(pool, fname, all_flags_com)
# make CRSHNMBR primary key
