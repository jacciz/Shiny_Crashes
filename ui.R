#load css, html here. may have to packages in both if html, css is used
#charts must be rendered in server file then displayed in outpur fct
library(shinydashboard)
library(shinyWidgets)
#
county_list = c(2,34,35)

ui <- dashboardPage(
  dashboardHeader(title = "My first app"),
  
  dashboardSidebar(                                     # SIDEBAR
    # The dynamically-generated user panel
    # uiOutput("userpanel")
    pickerInput("cntynum", "County", sort(as.character(county_list)), options = list("actions-box" = TRUE), multiple = FALSE, selected="34")),
    
  dashboardBody(                                    # BODY
      sliderInput(
        "exponent",
        label = "Choose 1",
        min = 1,
        max = 5,
        value = 2
      ),
      plotOutput("curve_plot"),
      
      plotOutput("bikeflag")
    )
  )

restr
# dashboardPage(
#   dashboardHeader(
# 
#   title = "My first app"),
#   # tabPanel("crash")),
#   
#   
#   dashboardBody(
    
  # sliderInput("exponent",
  #             label = "Choose 1",
  #             min = 1,
  #             max = 5,
  #             value = 2),
  # plotOutput("curve_plot")
# ))
# navBar  - can use other layouts
