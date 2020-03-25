#load css, html here. may have to packages in both if html, css is used
#charts must be rendered in server file then displayed in outpur fct
library(shinydashboard)
library(shinyWidgets)
library(DT)

ui <- dashboardPage(
  #                                                                 HEADER
  dashboardHeader(title = "Crash Data Dashboard"),
  #                                                                 SIDEBAR
  dashboardSidebar(
    # The dynamically-generated user panel
    # uiOutput("userpanel")
    pickerInput(
      "cntynum",
      "County",
      choices = sort(unique(all_crashes$CNTYCODE)),
      options = list("actions-box" = TRUE),
      multiple = FALSE,
      selected = "42"
    )  # this does not work after I set in cnty names
  ),
  #                                                                BODY first row
  dashboardBody(
    fluidRow(
      box(
        title = "Bike Crashes",
        width = 4,
        solidHeader = TRUE,
        plotOutput("bikeflag")
      ),
      box(
        title = "Pedestrian Crashes",
        width = 4,
        solidHeader = TRUE,
        plotOutput("pedflag")
      ),
      box(
        title = "Alcohol Flag Crashes",
        width = 4,
        solidHeader = TRUE,
        plotOutput("alcflag")
      )
    ),
    #                                                             BODY second row
    fluidRow(box(
      width = 6, DTOutput("biketable", height = 600)
    )))
)