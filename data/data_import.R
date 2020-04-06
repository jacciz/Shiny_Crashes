library(data.table)
library(dplyr)

import_all_crashes <- function(csv_name) {
  # read_csv(
  #   paste("data/",csv_name,".csv", sep = ""),
  #   n_max = 200,
    # col_names = c("CRSHNMBR",
    #               "CRSHDATE",
    #               "CRSHTIME",
    #               "CRSHMTH"
                  # "TOTINJ",
                  # "TOTFATL",
                  # "DAYNMBR",
                  # "CNTYCODE",
                  # "MUNICODE",
                  # "URBRURAL",
                  # "CRSHSVR",
                  # "MNRCOLL",
                  # "ALCFLAG",
                  # "DRUGFLAG",
                  # "BIKEFLAG",
                  # "CYCLFLAG",
                  # "PEDFLAG"
    # ),
    # col_types = cols(
    #   CRSHNMBR = col_character(),
    #   CRSHDATE = col_date(),
    #   CRSHTIME = col_double(),
    #   CRSHMTH = col_character(),
    #   TOTINJ = col_double(),
    #   TOTFATL = col_integer()
            # DAYNMBR = col_character(),
            # CNTYCODE = col_integer(),
            # MUNICODE = col_integer(),
            # URBRURAL  = col_character(),
            # CRSHSVR = col_character(),
            # MNRCOLL = col_character(),
            # ALCFLAG = col_character(),
            # DRUGFLAG = col_character(),
            # BIKEFLAG = col_character(),
            # CYCLFLAG = col_character(),
            # PEDFLAG = col_character()
  #   )
  # )
  all_crashes <-
    fread(paste("data/", csv_name, ".csv", sep = ""), nrows = 200,
          select = c("CRSHNMBR", "CRSHDATE", "CRSHTIME", "CRSHMTH", "TOTINJ", "TOTFATL",
                     "DAYNMBR", "CNTYCODE", "MUNICODE", "URBRURAL", "CRSHSVR", "MNRCOLL",
                     "ALCFLAG", "DRUGFLAG", "BIKEFLAG", "CYCLFLAG", "PEDFLAG")
          )
  all_crashes <- all_crashes %>% mutate(newtime = cut(
    CRSHTIME,
    c(
      0,
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
  return (all_crashes)
}

import_all_persons <- function(csv_name) {
  all_persons <-
    fread(paste("data/", csv_name, ".csv", sep = ""), nrows = 200,
          select = c("ROLE", "SEX")
    )
  all_persons <- all_persons %>% mutate(age_group = cut(
    AGE,
    c(0,
      5,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      55,
      60,
      65,
      70,
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
      "65-60",
      "70+",
    ),
    include.lowest = T))
  return (all_persons)
}

import_all_vehicles <- function(csv_name) {
  all_vehicles <-
    fread(paste("data/", csv_name, ".csv", sep = ""), nrows = 200,
          select = c()
    )
  return (all_vehicles)
}