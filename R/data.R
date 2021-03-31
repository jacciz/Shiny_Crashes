# TO GET THIS DATA YOU MUST FIRST RUN data_import.R and data_find_crash_flags.R on every single year
# These scripts loads a CSV, selects certain columns, and adds new ones. Then it saves as FST
# THEN use combine_data.R to combine all these databases of each year into a large database

# THIS script loads that large database of each type (crash,person,vehicle,crsh_flags)

#' county recode
#'
#' @format shp
#' \describe{
#' }
#' @source \url{}
"county_recode"

#' muni recode
#'
#' @format shp
#' \describe{
#' }
#' @source \url{}
"muni_recode"
