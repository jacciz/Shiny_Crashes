#' siderbar_select_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_siderbar_select_year_ui <- function(id) {
  # moduleServer(id, function(input, output, session) {
  shiny::sliderInput(
    shiny::NS(id, "year"),
    label = "Select Year",
    value = c(2020,2020), # starts with 2020 selected
    min = 2017,
    max = 2020,
    sep = "",
    ticks = FALSE
  )
  # return(reactive(input$year))
}
    
#' siderbar_select_year Server Function
#'
#' @noRd 
mod_siderbar_select_year_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    return(shiny::reactive({
      input$year
    }))
  })
}
    
## To be copied in the UI
# mod_siderbar_select_year_ui("siderbar_select_year_ui_1")
    
## To be copied in the server
# callModule(mod_siderbar_select_year_server, "siderbar_select_year_ui_1")