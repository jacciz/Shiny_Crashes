#' chart_crsh_svr_mth UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
   mod_chart_crsh_svr_mth_ui <- function(id) {
    tagList(plotlyOutput(shiny::NS(id, "crsh_svr_mth"), height = "240px"))
  }

#' chart_crsh_svr_mth Server Function
#'
#' @noRd 
mod_chart_crsh_svr_mth_server <- function(id, crash_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$crsh_svr_mth <- renderPlotly({
      if (dim(crash_df())[1] == 0) {  # or no crashes with a time ??
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nCrash Severity by Month",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
       
        crash = crash_df() %>%
          dplyr::mutate(svr = factor(.data$CRSHSVR, levels = crshsvr_factor_levels),
                        CRSHMTH = factor(.data$CRSHMTH, levels = base::month.name))
        
        crash$CRSHMTH <- month.abb[crash$CRSHMTH]
        crshsvr_table <- 
          crash %>%
          dplyr::mutate(CRSHMTH = factor(.data$CRSHMTH, levels = base::month.abb)) %>%
          dplyr::count(month = .data$CRSHMTH, svr = .data$svr, .drop = FALSE)
        
        plot_ly(
          crshsvr_table,
          type = 'bar',
          x = ~ month,
          y = ~ n,
          color = ~ svr,
          colors = ~ color_map_svr,
          # assign colors, this will give a warning 'Duplicate levels detected'
          hovertemplate = paste('%{x}<br>',
                                '<b>%{y: .0f} Crashes')
        ) %>% #Price: %{y:$.2f}<extra></extra>
          layout(
            title = list(
              text = "\nCrash Severity by Month",
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
              categoryarray = ~ base::month.abb,
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
            barmode = 'stack'
          ) %>%
          config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Crash Severity by Month",
              scale = 2
            )
          )
      }
      # scale_x_discrete(limits = month.name, name = "", labels = function(labels) {  # scatter labels
      # sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}
    })
  })
}
## To be copied in the UI
# mod_chart_crsh_svr_mth_ui("chart_crsh_svr_mth_ui_1")
    
## To be copied in the server
# callModule(mod_chart_crsh_svr_mth_server, "chart_crsh_svr_mth_ui_1")
 
