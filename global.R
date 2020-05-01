# source("data/data_import.R") # import functions from this script to import data

Sys.setenv("plotly_username" = "jacciz")   # to use plotly, this is my token
Sys.setenv("plotly_api_key" = "wrczHh7hA58lbPmrZ4jz")


# setwd("W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard") # don't need this when uploading to server

all_crashes <- readRDS("data/crash.rds")
# Note: Creates a newtime variable - time of 0 and 999 is NA

all_persons <- readRDS("data/person.rds")
# Note: Creates an age_group variable

all_vehicles <- readRDS("data/vehicle.rds")

crsh_flags <- readRDS("data/crsh_flags.rds")
# speedflag, teenflag, olderflag ("Y")

# These are to get county/muni names
county_recode <- readRDS("data/county_recode.rds")
muni_recode <- readRDS("data/muni_recode.rds")

crash_lat_long <- readRDS("data/crash_lat_long.rds")

# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactLog() for issues with reactivity

# options(shiny.reactlog = TRUE) then ctrl-f3 in browser

# all_crashes %>% select(CRSHNMBR, CRSHTIME, newtime) %>% View() # to compare time


# crsh_flags <- c("ALCFLAG", "DRUGFLAG")
# crsh_flag_text <- c("Alcohol-related", "Drug-related")
# crsh_flag_recode <- data.frame(crsh_flags, crsh_flag_text)

# input$crsh_flags
# input_crsh_flags = c("Teen driver", "Speeding")
# 
# for (flag in input_crsh_flags){
#   crsh_nmbr_list = list()
# if (flag == "Teen driver"){
#   flags <- crsh_flags %>% filter(teenflag == "Y") %>% dplyr::select(CRSHNMBR)
#   crsh_nmbr_list = rbind(crsh_nmbr_list, flags)
# }  else if (flag == "Speeding"){
#     flags <- crsh_flags %>% filter(speedflag == "Y") %>% dplyr::select(CRSHNMBR)
#     crsh_nmbr_list = rbind(crsh_nmbr_list, flags)
#   }
#   test <- unique(crsh_nmbr_list[])
#   return(print(unique(crsh_nmbr_list[])))
# }
# 
# 
# rename_crsh_flags <- # rename inputs so we can select flag columns
#   c(
#     'Speeding' = 'speedflag',
#     'Teen driver' = 'teenflag',
#     'Older driver' = 'olderflag'
#   )
# new_crsh_flags <- rename_crsh_flags[input_crsh_flags] # apply the rename to get a list
# 
# seleced_crash_flag_crshes <- crsh_flags[apply(crsh_flags [new_crsh_flags],1,function(x) any(x == "Y")),] %>% dplyr::filter(!is.na(CRSHNMBR))
# seleced_crash_flag_crshes$CRSHNMBR
  
  # new_crsh_flags %>%
  # crsh_flags[new_crsh_flags] %>%  # selects columns of flags
  # filter(apply(., 1, function(thisrow)
  #   any(thisrow == "Y")) == TRUE) # this selects all rows where a speeding criteria is met
