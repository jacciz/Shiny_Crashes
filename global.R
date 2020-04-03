library(readr)
library(dplyr)

# library(grDevices)  # for fonts
# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R_WD_Data/Shiny_Crashes")
# all_crashes <- read.csv("data/2020_crash.csv")    # pull either 2019 or 2020
all_crashes <- read_csv("data/2019_crash.csv", n_max = 20000)
# all_vehicles <- read_csv("data/2019_vehicle.csv", n_max = 1000)
# all_persons <- read_csv("data/2019_person.csv", n_max = 1000)
county_recode <- read_csv("data/county_recode.csv")
muni_recode <- read_csv("data/muni_recode.csv")

# windowsFonts("Cambria" = windowsFont("Cambria"))
# 
# theme_traffic <- function(base_size = 11, base_family = ""){
#   
# }

# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactLog() for issues with reactivity

# options(shiny.reactlog = TRUE) then ctrl-f3 in browser

Sys.setenv("plotly_username" = "jacciz")   # to use plotly, this is my token
Sys.setenv("plotly_api_key" = "wrczHh7hA58lbPmrZ4jz")

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

# all_persons <- all_persons %>% mutate(age_group = cut(
#         AGE,
#         c(0,
#           5,
#           10,
#           15,
#           20,
#           25,
#           30,
#           35,
#           40,
#           45,
#           50,
#           55,
#           60,
#           65,
#           70,
#           120),
#         labels = c(
#                 "0-4",
#                 "5-9",
#                 "10-14",
#                 "15-19",
#                 "20-24",
#                 "25-29",
#                 "30-34",
#                 "35-39",
#                 "40-44",
#                 "45-49",
#                 "50-54",
#                 "55-59",
#                 "60-64",
#                 "65-60",
#                 "70+",
#         ),
#         include.lowest = T))

# all_crashes %>% select(CRSHTIME, newtime) %>% View()