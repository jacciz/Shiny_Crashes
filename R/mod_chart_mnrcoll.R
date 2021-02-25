#' chart_mnrcoll UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_mnrcoll_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "mnrcoll"), height = "340px"))
}

    
#' chart_mnrcoll Server Function
#'
#' @noRd 
mod_chart_mnrcoll_server <- function(id, crash_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$mnrcoll <- renderPlotly({
      if (dim(crash_df())[1] == 0) { # if no crashes, show empty plot, else make plot
        # hide("mnrcoll")
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nManner of Collision",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        mnr_crashes <- crash_df() %>%
          dplyr::filter(MNRCOLL != "Unknown")
        
        mnr_crashes_table <-
          table(mnrcoll = mnr_crashes$MNRCOLL) %>% tibble::as_tibble()
        
        plot_ly(
          mnr_crashes_table,
          type = 'bar',
          orientation = 'h',
          x = ~ n,
          # y = ~ reorder(mnrcoll, n),
          y = ~ reorder(stringr::str_wrap(mnrcoll, width = 30), n), 
          # reorder from big to small values
          marker = list(color = "#4fb9db"),
          # blue!
          hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
          text = ~sprintf("<b>%s</b>", format(n, big.mark = ",")),
          # bar end number
          textfont = chart_axis_bar,
          textposition = 'outside',
          cliponaxis = FALSE
        ) %>%
          layout(
            title = list(
              text = "\nManner of Collision",
              font = chart_title,
              x = 0,
              y = .99
            ),
            margin = list(r = 40, # set to 40 so labels don't get cut off
                          l = 200, # so axis label don't get cut off
                          t = 40,
                          pad = 5, # axis label to bar padding
                          b = 0),
            xaxis = list(
              title = "",
              zerolinecolor = "white",
              showgrid = FALSE,
              showticklabels = FALSE # remove axis labels
            ),
            yaxis = list(title = "", tickfont = chart_axis),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>% config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Manner of Collision",
              scale = 2
            )
          )
      }
    })
  })
}
    
## To be copied in the UI
# mod_chart_mnrcoll_ui("chart_mnrcoll_ui_1")
    
## To be copied in the server
# callModule(mod_chart_mnrcoll_server, "chart_mnrcoll_ui_1")
 
