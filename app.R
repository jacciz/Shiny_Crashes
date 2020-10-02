library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(plotly)
library(leaflet)
library(shiny)

CrashApp <- function() {
  ui <- fluidPage(
    # sidebarLayout(
    #   sidebarPanel(
        # plotlyOutput("crsh_svr_mth")
        # datasetInput("data", is.data.frame),
        # selectVarInput("var"),
      # ),
      mainPanel(
        crsh_svr_mth_ui("crsh_svr_mth", height = "240px")
      )
    # )
  )
  
  server <- function(input, output, session) {
    data <- read_fst("data/all_crash", as.data.table = TRUE)
    # data <- datasetServer("data")
    # x <- selectVarServer("var", data)
    crsh_svr_mth_server("crsh_svr_mth", data)
  }
  shinyApp(ui, server)
} 

CrashApp()
