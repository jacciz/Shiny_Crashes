#load css, html here. may have to packages in both if html, css is used
#charts must be rendered in server file then displayed in outpur fct
library(shinydashboard)
library(shinyWidgets)
library(DT)

county_list = c(2, 13, 34,35)

ui <- dashboardPage(
  dashboardHeader(title = "My first app"),
  
  dashboardSidebar(                                 # SIDEBAR
    # The dynamically-generated user panel
    # uiOutput("userpanel")
    pickerInput("cntynum", "County",
                choices = sort(unique(all_crashes$CNTYCODE)),
                options = list("actions-box" = TRUE),
                multiple = FALSE,
                selected = "42")  # this does not work after I set in cnty names
    ),
  dashboardBody(                                    # BODY
      
      plotOutput("bikeflag"),
      
      DTOutput("biketable")
    )
  )