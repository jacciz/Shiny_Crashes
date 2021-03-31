#' siderbar_select_county UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_siderbar_select_county_ui <- function(id) {
  shinyWidgets::pickerInput(
    shiny::NS(id, "cntycode"),
    "County",
    multiple = TRUE,
    selected = 40,
    # selectize = FALSE,
    choices = NULL,#stats::setNames(county_recode$CountyCode, county_recode$CountyName),
    options = shinyWidgets::pickerOptions(
      actionsBox = TRUE, noneSelectedText = "Select County",  liveSearch = TRUE, size = 10, showContent= FALSE, liveSearchStyle = 'startsWith')
  )
}

#' siderbar_select_county Server Function
#'
#' @noRd 
mod_siderbar_select_county_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # observeEvent(input$cntycode, {
    shiny::observe({
      shinyWidgets::updatePickerInput(
        session,
        "cntycode",
        selected = 40, # select Milwaukee
        choices = stats::setNames(county_recode$CountyCode, county_recode$CountyName)
      )
    })
    return(shiny::reactive({
      input$cntycode
      # .env$input$cntycode_input
    }))
  })
}

## To be copied in the UI
# mod_siderbar_select_county_ui("siderbar_select_county_ui_1")
    
## To be copied in the server
# callModule(mod_siderbar_select_county_server, "siderbar_select_county_ui_1")
 
