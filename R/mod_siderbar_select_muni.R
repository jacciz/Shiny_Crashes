#' siderbar_select_muni UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_siderbar_select_muni_ui <- function(id) {
  shinyWidgets::pickerInput(
    shiny::NS(id, "municode_input"),
    "Municipality",
    choices = NULL,
    multiple = FALSE,
    # selectize = FALSE
  )
}
    
#' siderbar_select_muni Server Function
#'
#' @noRd 
mod_siderbar_select_muni_server <- function(id, county_input) {
  shiny::moduleServer(id, function(input, output, session) {
    observeEvent(county_input(), {
      # observe({
      muni_cnty_list <-
        muni_recode %>% dplyr::filter(.data$CntyCode %in% county_input())
      shinyWidgets::updatePickerInput(
        session,
        "municode_input",
        selected = 004,
        choices = stats::setNames(
          muni_cnty_list$MuniCode,
          muni_cnty_list$Municipality_CTV
        )
      )
    })
    return(shiny::reactive({
      input$municode_input
    }))
  })
}
## To be copied in the UI
# mod_siderbar_select_muni_ui("siderbar_select_muni_ui_1")
    
## To be copied in the server
# callModule(mod_siderbar_select_muni_server, "siderbar_select_muni_ui_1")
 
