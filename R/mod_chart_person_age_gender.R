#' chart_person_age_gender UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_person_age_gender_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "person_age_gender"), height = "240px"))
}
    
#' chart_person_age_gender Server Function
#'
#' @noRd 
mod_chart_person_age_gender_server <- function(id, persons_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$person_age_gender <- renderPlotly({
      if (dim(persons_df())[1] == 0) {
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nAge and Gender of All Persons",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        person <-
          persons_df()[, .(age_group, SEX)]
        
        age_sex_table <-
          table(age = person$age_group, sex = person$SEX) %>% tibble::as_tibble() # get counts, put in a tibble
        
        plot_ly(
          age_sex_table,
          type = 'bar',
          x = ~ age,
          y = ~ n,
          color = ~ sex,
          colors = ~ color_map_gender,
          hovertemplate = paste('<br>Age %{x}<br>',
                                '<b>%{y: .0f} people<b>')
        ) %>%
          layout(
            title = list(
              text = "\nAge and Gender of All Persons",
              font = chart_title,
              y = 1,
              x = 0
            ),
            showlegend = TRUE,
            legend = list(
              x = .5,
              y = 1.2,
              orientation = 'h',
              font = chart_axis
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
              tickangle = -45,
              categoryarray = ~ age,
              categoryorder = "array"
            ),
            yaxis = list(
              title = "",
              showgrid = FALSE,
              tickfont = chart_axis,
              zerolinecolor = "white"
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)',
            barmode = 'stack'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Age and Gender of All Persons",
              scale = 2
            )
          )
        # labels = function(labels) { # this scatters labels so they fit on two lines
        #   sapply(seq_along(labels), function(i)
        #     paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        # }
      }
    })
  })
}
    
## To be copied in the UI
# mod_chart_person_age_gender_ui("chart_person_age_gender_ui_1")
    
## To be copied in the server
# callModule(mod_chart_person_age_gender_server, "chart_person_age_gender_ui_1")
 
