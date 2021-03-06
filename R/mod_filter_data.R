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
#' @param db_type crash, vehicle, or person data
#' @param county county selected#'
#' @param crsh_svr crash severity selected
#' @param years min year selected
#'
#' @noRd 
mod_filter_data_server <-
  function(id,
           db_type,
           county,
           crsh_svr,
           years
           # muni
           ) {
    shiny::moduleServer(id, function(input, output, session) {
      min = min(years())
      max = max(years())
      
      county_tbl = county()
      crsh_svr_tbl = crsh_svr()
      # Get all years from min to max and adds db type - i.e. char list of "2017crash
      get_all_years_to_select <- paste0(seq(min,max, by = 1), db_type, sep = "")
     
      read_db_tables <- function(db_name){
        # Speed test found that dplyr::tbl is 10x faster than pool::dbReadTable. Just can't handle reactives and need to make it into a df.
        dplyr::tbl(pool, db_name) %>% filter(CNTYCODE %in% county_tbl, CRSHSVR %in% crsh_svr_tbl) %>% as.data.frame() 
      }
      
      # Iterates each year/db type and returns a combined df
      do.call(dplyr::bind_rows, lapply(get_all_years_to_select, read_db_tables))
      # DBI::dbDisconnect(conn)
      
      
      ## OLD WAY
      
      # # Make into data table. Can then use keys, indexing, .
      # data_input = data.table::data.table(data_input)
      # 
      # # Set keys for fast indexing
      # keycols = c("CNTYCODE", "CRSHDATE", "CRSHSVR")
      # data.table::setkeyv(data_input, keycols)
      # 
      # # Get range of dates
      # yearrange <-
      #   reactive(lubridate::interval(lubridate::mdy(paste0(
      #     "01-01-", min_year()
      #   )), lubridate::mdy(paste0(
      #     "12-31-", max_year()
      #   ))))
      # # reactive(data_input)
      # # %>% filter(.data[[CNTYCODE]] %in% county() )
      # reactive(data_input[CNTYCODE %in% county() &
      #                       CRSHSVR %in% crsh_svr()# &
      #                       # CRSHDATE %within% yearrange()
      #                     ]) #lubridate::mdy(crash$CRSHDATE)

    })
  }
    
## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")
    
## To be copied in the server
# callModule(mod_filter_data_server, "filter_data_ui_1")
 
