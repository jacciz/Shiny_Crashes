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
        df <-
          crash_df() %>% mutate(
            newtime = factor(newtime, levels = newtime_factor_levels),
            DAYNMBR = factor(DAYNMBR, levels = day_factor_levels)
          )
        
        day_time_data = df %>%  dplyr::count(newtime, DAYNMBR, .drop = FALSE)
        
        # get blue color gradient
        vals <- unique(scales::rescale(day_time_data$n))
        o <- order(vals, decreasing = FALSE)
        cols <- scales::col_numeric("Blues", domain = NULL)(vals)
        colz <- stats::setNames(data.frame(vals[o], cols[o]), NULL)
        
        day_time_data %>%
          plotly::plot_ly(
            x = ~ DAYNMBR,
            y =  ~ newtime,
            z = ~ n,
            # crash count
            colorscale = colz,
            type = "heatmap",
            # showscale = FALSE, # No legend
            hovertemplate = paste('%{x} %{y}<br>',
                                  '<b>%{z:.0f} Crashes<extra></extra>')
          ) %>%
          layout(
            title = list(
              text = "Time of Day",
              font = chart_title,
              x = 0
            ),
            margin = list(r = 0, l = 0, b = 0),
            xaxis = list(
              title = "",
              tickfont = chart_axis,
              tickangle = 0,
              tickcolor = "white"
            ),
            yaxis = list(
              title = "",
              tickfont = chart_axis,
              tickcolor = "white"
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%
          config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Time of Day Crashes",
              scale = 2
            )
          )
      }
    })
  })
## To be copied in the UI
# mod_chart_timeofday_heat_ui("chart_timeofday_heat_ui_1")
    
## To be copied in the server
# callModule(mod_chart_timeofday_heat_server, "chart_timeofday_heat_ui_1")
 
