#' chart_vehicle_treemap UI Function
#'
#' @description A shiny Module.
#' @import plotly 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_vehicle_treemap_ui <- function(id) {
  tagList(plotlyOutput(shiny::NS(id, "vehicle_treemap")))
}
    
#' chart_vehicle_treemap Server Function
#'
#' @noRd 
mod_chart_vehicle_treemap_server <- function(id, vehicles_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$vehicle_treemap <- renderPlotly({
      if (dim(vehicles_df())[1] == 0) {
        plotly_empty(type = "treemap") %>% layout(
          title = list(
            text = "\nAll Vehicles Involved",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        newcar <- vehicles_df() %>% # put this is data import  # vehcate
          dplyr::mutate(
            cate = dplyr::case_when(
              VEHTYPE == "Passenger Car" ~ "Passenger Vehicle",
              VEHTYPE == "(Sport) Utility Vehicle" ~ "Passenger Vehicle",
              VEHTYPE == "Cargo Van (10,000 Lbs or Less)" ~ "Passenger Vehicle",
              VEHTYPE == "Passenger Van" ~ "Passenger Vehicle",
              VEHTYPE == "Utility Truck/Pickup Truck" ~ "Light Trucks",
              VEHTYPE == "Straight Truck" ~ "Large Trucks",
              VEHTYPE == "Truck Tractor (Trailer Not Attached)" ~ "Large Trucks",
              VEHTYPE == "Truck Tractor (Trailer Attached)" ~ "Large Trucks",
              VEHTYPE == "Truck Tractor (More Than One Trailer)" ~ "Large Trucks",
              VEHTYPE == VEHTYPE ~ "Other"
            )
          )
        car_tib <- # then change newcar to vehicles_df()
          table(vehtype = newcar$VEHTYPE, parent = newcar$cate) %>%
          tibble::as_tibble() %>% dplyr::filter(.data$n != 0)
        
        parent_tib <-
          stats::xtabs(car_tib$n ~ car_tib$parent) %>% tibble::as_tibble()# then rbind
        names(parent_tib)[names(parent_tib) == "car_tib$parent"] <-
          "vehtype"
        parent_tib <- parent_tib %>% dplyr::mutate(parent = "Vehicle Type")
        
        car_tib <- rbind(car_tib, parent_tib)
        
        plot_ly(
          car_tib,
          type = 'treemap',
          branchvalues = "total",
          textfont = list(size = 14, family = "Verdana"),
          outsidetextfont = list(color = "rgba(0,0,0,0)"), # transparent title
          tiling = list(packing = "ratio"),
          ids = ~ vehtype,
          labels = ~ vehtype,
          parents = ~ parent,
          values = ~ .data$n,
          hoverlabel = list(font=list(size = 16, family = "Verdana")), # NEW
          hoverinfo = "label+value+percent root", # NEW
          textinfo = "label+value+percent root" # NEW
        ) %>%
          layout(colorway=c("#5a77db", "#F9C218", "#4DB848","#D50032"),
                 uniformtext=list(minsize=14),
                 title = list(
                   text = "\nAll Vehicles Involved",
                   font = chart_title,
                   y = 1,
                   x = 0
                 ),
                 margin = list(
                   r = 0,
                   l = 0,
                   b = 0,
                   t = 0
                 ),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 # make transparent background
                 paper_bgcolor = 'rgba(0,0,0,0)'
          )  %>% config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "All Vehicles Involved",
              scale = 2
            )
          )
      }
    })
  })
}
    
## To be copied in the UI
# mod_chart_vehicle_treemap_ui("chart_vehicle_treemap_ui_1")
    
## To be copied in the server
# callModule(mod_chart_vehicle_treemap_server, "chart_vehicle_treemap_ui_1")
 
