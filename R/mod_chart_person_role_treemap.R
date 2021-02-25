#' chart_person_role_treemap UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_person_role_treemap_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "person_role_treemap"), height = "250px"))
}
    
#' chart_person_role_treemap Server Function
#'
#' @noRd 
mod_chart_person_role_treemap_server <- function(id, persons_df) {
  # stopifnot(is.reactie(x))
  shiny::moduleServer(id, function(input, output, session) {
    output$person_role_treemap <- renderPlotly({
      if (dim(persons_df())[1] == 0) {  # or no crashes with a time ??
        plotly_empty(type = "treemap") %>% layout(
          title = list(
            text = "\nRole of All Persons",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        role_table <-
          table(role = persons_df()$ROLE) %>%
          tibble::as_tibble() %>%
          dplyr::mutate(parent = "role")
        plot_ly(
          role_table,
          type = 'treemap',
          textfont = list(size = 14, family = "Verdana"),
          outsidetextfont = list(color = "rgba(0,0,0,0)"), # transparent title
          labels = ~ role,
          parents = ~ parent,
          values = ~ n,
          hoverlabel = list(font=list(size = 16, family = "Verdana")),
          hoverinfo = "label+value+percent root",
          textinfo = "label+value+percent root"
        ) %>%
          layout(colorway=c("#5a77db", "#F9C218", "#4DB848","#D50032","#4fb9db"),
                 uniformtext=list(minsize=14),
                 title = list(
                   text = "\nRole of All Persons",
                   font = chart_title,
                   y = 1,
                   x = 0
                 ),
                 margin = list(
                   r = 0,
                   l = 0,
                   b = 10,
                   t = 0
                 ),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 # make transparent background
                 paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Role of All Persons",
              scale = 2
            )
          )
      }
    })
  })
}
    
## To be copied in the UI
# mod_chart_person_role_treemap_ui("chart_person_role_treemap_ui_1")
    
## To be copied in the server
# callModule(mod_chart_person_role_treemap_server, "chart_person_role_treemap_ui_1")
 
