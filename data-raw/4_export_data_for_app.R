# This scrips opens raw data in /data-raw and exports it into a different format to /data so the app can use the data.

# Note: Creates a newtime variable - time of 0 and 999 is NA
all_crashes <- fst::read_fst("data-raw/all_crash", as.data.table = TRUE)

# Note: Creates an age_group variable
all_persons <- fst::read_fst("data-raw/all_person", as.data.table = TRUE)

all_vehicles <- fst::read_fst("data-raw/all_vehicle", as.data.table = TRUE)

# speedflag, teenflag, olderflag ("Y"), all other crash flags
all_crsh_flags <- fst::read_fst("data-raw/all_crsh_flags", as.data.table = TRUE)

# These are to get county/muni names
county_recode <- readRDS("data-raw/county_recode.rds")
muni_recode <- readRDS("data-raw/muni_recode.rds")

# Read shapefiles for map
# ctv <- st_read("map/ctv.shp")
county <- sf::st_read("data-raw/county.shp")

# To update crash data
usethis::use_data(all_crashes)
usethis::use_data(all_persons)
usethis::use_data(all_vehicles)
usethis::use_data(all_crsh_flags)

# Probably never has to be run again
usethis::use_data(county_recode)
usethis::use_data(muni_recode)
usethis::use_data(county)
