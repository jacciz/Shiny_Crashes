#' Charts modules for server-side processings
#'
#' This module produces all charts with the crash_df/people_df/Vehicles_df based on variables selected by the user.
#' 

# Put this in the actual app to reference these modules
# UI: crsh_svr_mth_ui("crsh_svr_mth")
# Server: crsh_svr_mth_server("crsh_svr_mth", filtered_crashes())

# To add a new chart - use this
# crsh_svr_mth_server <- function(id, crash_df()) {
#   moduleServer(id, function(input, output, session) {
#     plotly stuff goes here
#   })
#   }


# Example use
# select_county_input("cntycode_input") # UI
# county_input <- select_county_server("cntycode_input") # Server

