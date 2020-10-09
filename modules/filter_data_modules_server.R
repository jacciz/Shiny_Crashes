#' Charts modules for filtering data based on user selection
#'
#' This module produces all charts with the crash_df based on variables selected by the user.
#' 
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df data frame (non-reactive) with variables necessary for charts
#' @param plot1_vars list containing reactive x-variable name (called `xvar`) and y-variable name (called `yvar`) for plot 1
#' @param plot2_vars list containing reactive x-variable name (called `xvar`) and y-variable name (called `yvar`) for plot 2# library(shinydashboard)

################### Filters raw database #######################

filter_data <-
  function(id,
           data,
           min_year,
           max_year,
           county,
           muni,
           crsh_svr) {
    moduleServer(id, function(input, output, session) {
      
      # Set keys for fast indexing
      keycols = c("CNTYCODE", "CRSHDATE", "CRSHSVR")
      setkeyv(data, keycols)
      
      # Get range of dates
      yearrange <-
        reactive(interval(mdy(paste0(
          "01-01-", min_year()
        )), mdy(paste0(
          "12-31-", max_year()
        ))))

      reactive(
        data[CNTYCODE %in% county() &
               CRSHSVR %in% crsh_svr() &
               CRSHDATE %within% yearrange()
             ]
        )
    }
    )
  }