#' chart_timeofday_heat UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_timeofday_heat_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "timeofday_heat"), height = "240px"))
}
    
#' chart_timeofday_heat Server Function
#'
#' @noRd 
mod_chart_timeofday_heat_server <- function(id, crash_df) 
  shiny::moduleServer(id, function(input, output, session) {
    
    output$timeofday_heat <- renderPlotly({
      
      if (dim(crash_df())[1] == 0) { # or no crashes with a time ??
        plotly_empty(type = "heatmap") %>% layout(
          title = list(text ="Time of Day", font = chart_title, x = 0),
          plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        df <- crash_df() %>% as.data.table()
        day_time_data <- df[ , .(.N), by = .(newtime, DAYNMBR)]
        day_time_data[DAYNMBR == "", DAYNMBR := NA] # if DAYNMBR not exist, make it NA
        day_time_data <- day_time_data %>% stats::na.omit() # remove all NA values
        day_time_data <- data.table::dcast(day_time_data, newtime ~ DAYNMBR, # reshape to long table
                               value.var = "N", fill = 0)
        
        # Used to create the empty tibble
        x <- c("Sunday", "Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday") #newtime
        y = c("12am","1am","2am","3am", "4am","5am", "6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm",
              "5pm","6pm","7pm","8pm","9pm","10pm","11pm")    
        
        # create an empty tibble so we get a full matrix for heat map
        empty_tibble <- tibble::tibble(newtime = y)
        
        # Combine empty tibble with data, use mutate to ensure levels match
        time_tibble <- dplyr::left_join(dplyr::mutate(empty_tibble, newtime=factor(newtime, levels=y)), day_time_data, by = c("newtime" = "newtime"))
        
        # function to find if column exists, if not, adds column with NA values
        fncols <- function(data, cname) {
          add <-cname[!cname%in%names(data)]
          if(length(add)!=0) data[add] <- 0
          data
        }
        
        day_time <- fncols(time_tibble, x) # apply function to get all columns
        day_time[is.na(day_time)] = 0 # NA will be 0
        
        day_time <-
          day_time[, c( # reorder columns
            "newtime","Sunday", "Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday"
          )]
        names(day_time) <-
          c("newtime", "Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat") # rename columns
        
        m <- day_time[, 2:8] %>% as.matrix()
        
        # get blue color gradient
        vals <- unique(scales::rescale(m))
        o <- order(vals, decreasing = FALSE)
        cols <- scales::col_numeric("Blues", domain = NULL)(vals)
        colz <- stats::setNames(data.frame(vals[o], cols[o]), NULL)
        
        plot_ly(
          x = colnames(day_time[2:8]),
          y = day_time$newtime,
          z = m, # crash count
          type = "heatmap",
          colorscale = colz,
          showscale = FALSE, # No legend
          hovertemplate = paste('%{x} %{y}<br>',
                                '<b>%{z:.0f} Crashes')
        ) %>% 
          layout(
            title = list(text ="Time of Day", font = chart_title, x = 0),
            margin = list(r = 0,l = 0, b = 0
            ),
            xaxis = list(tickfont = chart_axis, tickangle = 0, tickcolor = "white"),
            yaxis = list(tickfont = chart_axis, tickcolor = "white"),
            plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Time of Day Crashes", scale = 2)
          )
      }
    })
  })
## To be copied in the UI
# mod_chart_timeofday_heat_ui("chart_timeofday_heat_ui_1")
    
## To be copied in the server
# callModule(mod_chart_timeofday_heat_server, "chart_timeofday_heat_ui_1")
 
