#' chart_nmtact UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_nmtact_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "nmtact_chart"), height = "340px"))
}
    
#' chart_nmtact Server Function
#'
#' @noRd 
mod_chart_nmtact_server <- function(id, persons_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$nmtact_chart <- renderPlotly({
      if (dim(
        persons_df() %>% dplyr::select(NMTACT01:NMTACT12) %>% tidyr::pivot_longer(NMTACT01:NMTACT12)
        %>% filter(value != "Unknown", value != '')
      )[1] == 0) {
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nNo Pedestrians or Cyclists",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        nmtact <- persons_df() %>%
          dplyr::select(NMTACT01:NMTACT12) %>% tidyr::pivot_longer(NMTACT01:NMTACT12) %>% filter(value !=
                                                                                     '')
        # make freq table, remove variables, arrange and take top 8
        nmtact_table <-
          table(nmtact_count = nmtact$value) %>% tibble::as_tibble() %>%
          dplyr::filter(# nmtact_count != "No Improper Action",
            nmtact_count != "Unknown") %>%
          dplyr::arrange(desc(n)) %>% head(., 8)
        #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
        plot_ly(
          nmtact_table,
          type = 'bar',
          orientation = 'h',
          x = ~ n,
          # y = ~ reorder(nmtact_count, n),
          y = ~ reorder(stringr::str_wrap(nmtact_count, width = 30), n), 
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
              text = "\nTop Actions of Pedestrians and Cyclists",
              font = chart_title,
              x = 0,
              y = .99
            ),
            margin = list(r = 30, # set to 30 so labels don't get cut off
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
            yaxis = list(title = "", tickfont = chart_axis),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Actions of Pedestrians and Cyclists",
              scale = 2
            )
          )
      }
    })
  })
} 
## To be copied in the UI
# mod_chart_nmtact_ui("chart_nmtact_ui_1")
    
## To be copied in the server
# callModule(mod_chart_nmtact_server, "chart_nmtact_ui_1")
 
