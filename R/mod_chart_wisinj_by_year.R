#' chart_wisinj_by_year UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_wisinj_by_year_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "wisinj_by_year"), height = "240px"))
}
    
#' chart_wisinj_by_year Server Function
#'
#' @noRd 
mod_chart_wisinj_by_year_server <- function(id, person_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$wisinj_by_year <- renderPlotly({
      if (dim(person_df())[1] == 0) {  # or no crashes with a time ??
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nPersons Injured each Year",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        
        wisinj_table = person_df() %>%
          dplyr::filter(WISINJ != "No Apparent Injury") %>% dplyr::mutate(CRSHDATE = lubridate::mdy(CRSHDATE)) %>% dplyr::count(year = lubridate::year(CRSHDATE),WISINJ)
        
        # df = person_df() %>%
        #   dplyr::filter(.data$WISINJ != "No Apparent Injury") %>% dplyr::mutate(CRSHDATE = lubridate::mdy(CRSHDATE)) 
        # wisinj_table <-  
        #   table(year = lubridate::year(df$CRSHDATE), inj = factor(df$WISINJ, levels = wisinj_factor_levels)) %>%
        #   tibble::as_tibble()# get counts, put in a tibble
        # crshsvr_table$month <-
        # factor(crshsvr_table$month, levels = month.name) # factors month names, in month.name order
        
        plot_ly(
          wisinj_table,
          type = 'bar',
          x = ~ year,
          y = ~ .data$n,
          color = ~ factor(WISINJ, levels = wisinj_factor_levels),
          colors = ~ color_map_wisinj,
          text = ~sprintf("<b>%s</b>", format(.data$n, big.mark = ",")),
          # bar end number
          textfont = list(size = 14, color = color_map_wisinj[wisinj_table$inj], family = "Verdana", face = "bold"),
          textposition = 'outside',
          cliponaxis = FALSE,
          # assign colors, this will give a warning 'Duplicate levels detected'
          hovertemplate = paste('%{x}<br>',
                                '<b>%{y: .0f} Persons')
        ) %>% #Price: %{y:$.2f}<extra></extra>
          layout(
            title = list(
              text = "\nPersons Injured each Year",
              font = chart_title,
              y = 1,
              x = 0
            ),
            showlegend = TRUE,
            legend = list(
              x = .5,
              y = 1.2,
              orientation = 'h',
              font = chart_axis,
              traceorder = "normal" # alphabetical legend order
            ),
            margin = list(
              r = 0,
              l = 0,
              b = 0,
              t = 45
            ),
            xaxis = list(
              title = "",
              tickfont = chart_axis,
              tickangle = -0,
              # categoryarray = ~ month_order,
              categoryorder = "array" # sets order
              # ticktext = ~month.abb[crshsvr_table$month],
              # automargin = TRUE,
              # dtick = 5 # every 5 months are labeled
            ),
            yaxis = list(
              showgrid = FALSE,
              zerolinecolor = "white",
              tickfont = chart_axis,
              title = ""
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)',
            barmode = 'group'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Persons Injured each Year",
              scale = 2
            )
          )
      }
    })
  })
}
    
## To be copied in the UI
# mod_chart_wisinj_by_year_ui("chart_wisinj_by_year_ui_1")
    
## To be copied in the server
# callModule(mod_chart_wisinj_by_year_server, "chart_wisinj_by_year_ui_1")
 
