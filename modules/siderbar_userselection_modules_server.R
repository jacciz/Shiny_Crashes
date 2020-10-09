#' Charts modules for user selection processings
#'
#' This module produces all charts with the crash_df based on variables selected by the user.
#' 
#' @param input,output,session standard \code{shiny} boilerplate

# Example use
# select_county_input("cntycode_input") # UI
# county_input <- select_county_server("cntycode_input") # Server

################### Select County #######################
select_county_input <- function(id) {
  selectInput(
    NS(id, "cntycode_input"),
    "County",
    multiple = TRUE,
    selectize = FALSE,
    choices = NULL
  )
}
select_county_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # observeEvent(input$cntycode, {
      observe({
      updateSelectInput(session, "cntycode_input", choices = setNames(county_recode$CountyCode, county_recode$CountyName))
    })
    return(reactive({
      input$cntycode_input
    }))
  })
}

################### Select Municipality #######################
select_municode_input <- function(id) {
  selectInput(
    NS(id, "municode_input"),
    "Municipality",
    choices = NULL,
    multiple = FALSE,
    selectize = FALSE
  )
}
select_municode_server <- function(id, county_input) {
  moduleServer(id, function(input, output, session) {
    observeEvent(county_input(), {
    # observe({
      muni_cnty_list <-
        muni_recode %>% filter(CntyCode %in% county_input())
      updateSelectInput(
        session,
        "municode_input",
        choices = setNames(muni_cnty_list$MuniCode, muni_cnty_list$Municipality_CTV)
      )
    })
    return(reactive({
      input$municode_input
    }))
  })
}

################### Select Year #######################
select_year <- function(id) {
  # moduleServer(id, function(input, output, session) {
  sliderInput(
    NS(id, "year"),
    label = "Select Year",
    value = c(2017, 2019),
    min = 2017,
    max = 2019,
    sep = ""
  )
  # return(reactive(input$year))
}
select_year_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive({
      input$year
    }))
  })
}
