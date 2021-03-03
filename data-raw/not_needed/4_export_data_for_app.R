# This scrips opens raw data in /data-raw and exports it into a different format to /data so the app can use the data.

#### now only using the recode files as the data is in the SQLite db

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

# Chaanges the lat lng to a geometry type
export_crash_as_geom_st <- function(crash_df) {
  crash_lat_long_j_st <- sf::st_as_sf(
    x = all_crashes,
    coords = c("lng", "lat"),
    crs = 4326,
    na.fail = FALSE
  )
  sf <-
    sf::st_transform(crash_lat_long_j_st, "+proj=longlat +datum=WGS84")
  sf
}
all_crashes <- export_crash_as_geom_st(all_crashes)
# then convert this
usethis::use_data(all_crashes, overwrite = TRUE)

# fst::write_fst(all_crashes, path = "crash_geom.fst")

fst::read_fst("C:/W_shortcut/Shiny_Crashes_Dashboard/data-raw/crash_geom.fst")
