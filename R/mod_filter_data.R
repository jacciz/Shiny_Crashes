#' filter_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_data_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}
    
#' filter_data Server Function
#'
#' Filters data based on user input
#' @importFrom lubridate %within%
#' @param id Internal parameters for {shiny}.
#' @param data_input crash, vehicle, or person data
#' @param min_year min year selected
#' @param max_year max year selected
#' @param county county selected
#' @param muni municipality selected (not used)
#' @param crsh_svr crash severity selected
#'
#' @noRd 
mod_filter_data_server <-
  function(id,
           data_input,
           min_year,
           max_year,
           county,
           muni,
           crsh_svr) {
    shiny::moduleServer(id, function(input, output, session) {
      
      # Make into data table. Can then use keys, indexing, .
      data_input = data.table::as.data.table(data_input)
      # Set keys for fast indexing
      keycols = c("CNTYCODE", "CRSHDATE", "CRSHSVR")
      data.table::setkeyv(data_input, keycols)
      
      # Get range of dates
      yearrange <-
        reactive(lubridate::interval(lubridate::mdy(paste0(
          "01-01-", min_year()
        )), lubridate::mdy(paste0(
          "12-31-", max_year()
        ))))
      # reactive(data_input)
      # %>% filter(.data[[CNTYCODE]] %in% county() )
      reactive(data_input[CNTYCODE %in% county() &
                            CRSHSVR %in% crsh_svr() &
                            CRSHDATE %within% yearrange()])
    }
    )
  }
    
## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")
    
## To be copied in the server
# callModule(mod_filter_data_server, "filter_data_ui_1")
 
