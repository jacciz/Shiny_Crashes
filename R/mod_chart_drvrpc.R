#' chart_drvrpc UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_drvrpc_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "drvrpc_chart"), height = "340px"))
}
    
#' chart_drvrpc Server Function
#'
#' @noRd 
mod_chart_drvrpc_server <- function(id, persons_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$drvrpc_chart <- renderPlotly({
      if (dim(persons_df())[1] == 0) {
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nNo Driver Contributing Circumstances Found",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        drvrpc <- persons_df() %>%
          dplyr::select(dplyr::starts_with("DRVRPC")) %>%
        tidyr::pivot_longer(DRVRPC01:DRVRPC24) %>%
          dplyr::filter(value !=
                                                                                     '')
        # make freq table, remove variables, arrange and take top 8
        drvrpc_table <-
          table(drvrpc_count = drvrpc$value) %>% tibble::as_tibble() %>%
          dplyr::filter(drvrpc_count != "No Contributing Action",
                        drvrpc_count != "Unknown") %>%
          dplyr::arrange(dplyr::desc(n)) %>% utils::head(., 8)
        #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
        drvrpc_table$drvrpc_count <-
          stats::reorder(drvrpc_table$drvrpc_count, drvrpc_table$n)  # reorder from big to small values
        plot_ly(
          drvrpc_table,
          type = 'bar',
          orientation = 'h',
          x = ~ n,
          # y = ~ reorder(drvrpc_count, n),
          y = ~ stats::reorder(stringr::str_wrap(drvrpc_count, width = 30), n), # Break line after every 20 characters
          # reorder from big to small values, also wrap text
          marker = list(color = "#4fb9db"),
          # blue!
          hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
          text = ~sprintf("<b>%s</b>", format(n, big.mark = ",")),
          # bar end number
          textfont = chart_axis_bar,
          textposition = 'outside',
          cliponaxis = FALSE
        ) %>% #labels = function(x) str_wrap(drvrpc_count, width = 15)
          layout(
            title = list(
              text = "\nTop Driver Contributing Circumstance",
              font = chart_title,
              x = 0,
              y = .99
            ),
            margin = list(r = 35, # set to 30 so labels don't get cut off
                          l = 200, # so axis label don't get cut off
                          t = 40,
                          pad = 5, # axis label to bar padding
                          b = 0),
            xaxis = list(
              title = "",
              showgrid = FALSE,
              zerolinecolor = "white",
              showticklabels = FALSE # remove axis labels
            ),
            yaxis = list(title = "", tickfont = chart_axis, tickson = "labels"), # make sure labels are in the center of the bar
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Driver Contributing Circumstance",
              scale = 2
            )
          )
      }
    })
  })
}
## To be copied in the UI
# mod_chart_drvrpc_ui("chart_drvrpc_ui_1")
    
## To be copied in the server
# callModule(mod_chart_drvrpc_server, "chart_drvrpc_ui_1")
 
