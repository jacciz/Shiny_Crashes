# library(grDevices)  # for fonts
# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R_WD_Data/Shiny_Crashes")
# all_crashes <- read.csv("data/2020_crash.csv")    # pull either 2019 or 2020
all_crashes <- read.csv("data/2019_crash.csv")
# all_vehicles <- read.csv("data/2019_vehicle.csv")
county_recode <- read.csv("data/county_recode.csv")
muni_recode <- read.csv("data/muni_recode.csv")

# windowsFonts("Cambria" = windowsFont("Cambria"))
# 
# theme_traffic <- function(base_size = 11, base_family = ""){
#   
# }




# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactLog() for issues with reactivity

# options(shiny.reactlog = TRUE) then ctrl-f3 in browser