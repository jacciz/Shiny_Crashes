#' waffle_chart UI Function
#'
#' @description A shiny Modul
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_waffle_chart_ui <- function(id){
  tagList(plotOutput(shiny::NS(id, "waffle_chart"), height = "500px"))
}
    
#' waffle_chart Server Function
#' 
#' ERROR: Error in eval#(_inherit`, env, NULL) : object 'GeomText' not found.
#' Solution: add if(require("waffle")).
#' Error in loadNamespace: there is no package called ‘plyr’
#' Solution: add plyr to DESC, I think waffle uses plyr
#' @noRd 
mod_waffle_chart_server <- function(id, bikepedcount){
  shiny::moduleServer(id, function(input, output, session) {
    
    get_number_of_rows_size <- function(role_count) {
      total_roles = sum(role_count$n)
      if (total_roles == 1) {
        return(90)
      }
      else if
      (total_roles == 2) {
        return(90)
      }
      else if
      (total_roles < 20) {
        return(30)
      }
      else if
      (total_roles < 50) {
        return(15)
      }
      else if
      (total_roles >= 20) {
        return(3)
      }
    }
    
    output$waffle_chart <- renderPlot({
      # library(waffle)
      if(require("waffle")) {
      # if (requireNamespace("waffle", quietly = TRUE)) {
        if (sum(bikepedcount()$n) == 0) {
          ggplot2::ggplot() +
            waffle::geom_pictogram() + 
            ggplot2::theme(axis.text = ggplot2::element_blank(),
                  axis.line = ggplot2::element_blank(),
                  axis.ticks = ggplot2::element_blank(),
                  panel.background = ggplot2::element_rect(fill =  "#f8f8f8"),
                  plot.background = ggplot2::element_rect(fill =  "#f8f8f8"),
                  plot.title = ggtext::element_markdown(lineheight = 1.1)) +
            ggplot2::labs( title = "<span style='font-size:16pt;color:#666666'>No pedestrians or bicyclists</span>"
          )
        } else {
        
        bikeped_title <- sprintf( # not found font-family:Verdana
          "<span style='font-size:16pt;color:#666666'>
          %s pedestrians were <span style='color:#Db7e65;'>**killed,**</span>
          %s <span style='color:#4AAECF;'>**injured**<br></span> 
          %s bicyclists were <span style='color:#Db7e65;'>**killed,**</span>
          %s <span style='color:#3D8DA8;'>**injured**</span>  </span>",
          bikepedcount()[for_colors == "PedestrianKilled", n],
          bikepedcount()[for_colors == "PedestrianInjured", n],
          bikepedcount()[for_colors == "BicyclistKilled", n],
          bikepedcount()[for_colors == "BicyclistInjured", n]
        )
      
        bikepedcount() %>% 
          ggplot2::ggplot() +
          waffle::geom_pictogram(ggplot2::aes(label = .data$ROLE, values = .data$n, color = .data$for_colors),
                                 n_rows = round(sqrt(sum(bikepedcount()$n)), 0), # sqr root so we can make a square
                                 size = get_number_of_rows_size(bikepedcount()),
                                 # family = "FontAwesome5Free-Solid",
                                 # size = 4,
                                 flip = TRUE,
                                 # size = ".4vw",
                                 show.legend = FALSE) +
          # scale_label_pictogram()
          ggplot2::scale_color_manual(
            name = NULL,
            values = color_map_waffle_inj[bikepedcount()$for_colors]
            # labels = c("Fruit", "Sammich")
          ) +
          waffle::scale_label_pictogram(
            name = NULL,
            values = c("bicycle", "walking"),
            labels = c("Bicyclists", "Pedestrians")
          ) + #coord_equal() + # this makes it a square
          ggplot2::theme_classic() +
          waffle::theme_enhance_waffle() +
          ggplot2::theme(panel.background = ggplot2::element_rect(fill =  "#f8f8f8"),
                plot.background = ggplot2::element_rect(fill =  "#f8f8f8"),
                axis.line = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                plot.title = ggtext::element_markdown(lineheight = 1.1)) +
          
          ggplot2::labs(
            title = bikeped_title
          )
        }
      }
      })
  
  })
}
    
## To be copied in the UI
# mod_waffle_chart_ui("waffle_chart_ui_1")
    
## To be copied in the server
# callModule(mod_waffle_chart_server, "waffle_chart_ui_1")
 
