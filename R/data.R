# TO GET THIS DATA YOU MUST FIRST RUN data_import.R and data_find_crash_flags.R on every single year
# These scripts loads a CSV, selects certain columns, and adds new ones. Then it saves as FST
# THEN use combine_data.R to combine all these databases of each year into a large database

# THIS script loads that large database of each type (crash,person,vehicle,crsh_flags)


#' All crash data
#'
#' A dataset containing crash data from WisDOT.
#'
#' @format A data frame with of crash data:
#' \describe{
#' }
#' @source \url{http://www.diamondse.info/}
"all_crashes"