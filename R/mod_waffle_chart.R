#' waffle_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import ggplot2
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_waffle_chart_ui <- function(id){
  tagList(plotOutput(shiny::NS(id, "waffle_chart"), height = "500px"))
}
    
#' waffle_chart Server Function
#'
#' @noRd 
mod_waffle_chart_server <- function(id, bikepedcount){
  shiny::moduleServer(id, function(input, output, session) {
    # ERROR: Error in eval(`_inherit`, env, NULL) : object 'GeomText' not found
    # Solution: @import ggplot2
    get_number_of_rows_size <- function(role_count) {
      if (is.na((any(role_count[ "n"])))){
        para = c(rows = 1, size = 8)
      }
      else if (any(role_count[ "n"] == 1)) {
        para = c(rows = 1, size = 8)
      }
      else if
      (any(role_count[ "n"] == 2)){
        para = c(rows = 2, size = 6)
      }
      else if
      (any(role_count[ "n"] < 20)){
        para = c(rows = 5, size = 4)
      }
      else if
      (any(role_count["n"] >= 20)){
        para = c(rows = round(max(role_count["n"])/100,0), size = 3)
      }
      para
    }
    
    output$waffle_chart <- renderPlot({
      bikeped_title <- sprintf(
        "<span style='font-size:16pt;font-family:Verdana;color:#666666'>
        %s pedestrians were <span style='color:#Db7e65;'>**killed,**</span>
        %s <span style='color:#4AAECF;'>**injured**<br></span> 
        %s bicyclists were <span style='color:#Db7e65;'>**killed,**</span>
        %s <span style='color:#3D8DA8;'>**injured**</span>  </span>",
        bikepedcount()[for_colors == "PedestrianKilled", n],
        bikepedcount()[for_colors == "PedestrianInjured", n],
        bikepedcount()[for_colors == "BicyclistKilled", n],
        bikepedcount()[for_colors == "BicyclistInjured", n]
      )
      # count_of_roles <- person_df() %>% dplyr::filter(ROLE %in% c("Bicyclist", "Pedestrian")) %>%
      #   dplyr::count(.data$ROLE)
      
      # para <- get_number_of_rows_size(count_of_roles)
      
      bikepedcount() %>% 
        ggplot2::ggplot(aes(label = .data$ROLE, values = .data$n)) +
        waffle::geom_pictogram(aes(color = for_colors),
                               n_rows = round(sqrt(sum(bikepedcount()$n)),0),
                               size = para["size"],
                               # size = 4,
                               flip = TRUE,
                               # size = ".4vw",
                               show.legend = FALSE) +
        ggplot2::scale_color_manual(
          name = NULL,
          # values = c("#a40000", "#c68958")
          values = color_map_waffle_inj[bikepedcount()$for_colors]
          # labels = c("Fruit", "Sammich")
        ) +
        waffle::scale_label_pictogram(
          name = NULL,
          values = c("bicycle", "walking")
          # labels = c("Bicyclists", "Pedestrians")
        ) + #coord_equal() + # this makes it a square
        theme_classic() +
        theme(axis.text = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill =  "#f8f8f8"),
              plot.background = element_rect(fill =  "#f8f8f8"),
              plot.title = ggtext::element_markdown(lineheight = 1.1)) +
        labs(
          title = bikeped_title
        )
        
      # bike <- person_df() %>% dplyr::filter(ROLE %in% c("Bicyclist"), WISINJ !="No Apparent Injury")
      # bike_table <- table(bike$ROLE)
      # waffle::waffle(bike_table, get_number_of_rows(bike_table["Bicyclist"]), legend_pos = "none", use_glyph = "bicycle", colors = c("#4fb9db", "white"), glyph_size = 6)
    })
  
  })
}
    
## To be copied in the UI
# mod_waffle_chart_ui("waffle_chart_ui_1")
    
## To be copied in the server
# callModule(mod_waffle_chart_server, "waffle_chart_ui_1")
 
