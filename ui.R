library(shinydashboard)
library(shinyWidgets)
library(DT)
#                                                   SIDEBAR
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  
  menuItem(
    "Tables",
    icon = icon("th"),
    tabName = "tables",
    badgeColor = "green"
  ),    pickerInput(
    "cntynum",
    "County",
    choices = sort(unique(all_crashes$CNTYCODE)),
    options = list("actions-box" = TRUE),
    multiple = FALSE,
    selected = "Dane"
    ),  pickerInput(
      "muni_names",
      "Municipality",
      choices = sort(unique(all_crashes$MUNICODE)),
      options = list("actions-box" = TRUE),
      multiple = FALSE
    )
))
#                                                     BODY
body <- dashboardBody(tabItems(
  tabItem(tabName = "dashboard",
          h2("Dashboard tab content"),
  #                                                     FIRST TAB First row
          fluidRow(
            box("Vehicle Type", width = 12)
            ,
            # Dynamic infoBoxes
            valueBoxOutput("passveh_box", width = 2),
            valueBoxOutput("light_truck_box", width = 2),
            valueBoxOutput("large_truck_box", width = 2),
            valueBoxOutput("motorcycle_box", width = 2),
            valueBoxOutput("bike_box", width = 2),
            valueBoxOutput("ped_box", width = 2)
          ),
  #                                                     FIRST TAB Second row          
          fluidRow(
            box(
              title = "Bike and Pedestrian Crashes",
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
  #                                                     FIRST TAB Third row          
  
    fluidRow(
      box(
        title = "Manner of Collision",
        width = 4,
        solidHeader = TRUE,
        plotOutput("mnrcoll")
      )
      
    )
          #                                                             BODY second row
          ),
  #                                            SECOND TAB    First row
  tabItem(tabName = "tables",
          h2("Tables tab content"),
          
          fluidRow(box(
            width = 6, DTOutput("biketable", height = 600)
          ))
          )
))


# Put them together into a dashboardPage
dashboardPage(dashboardHeader(title = "Crash Data Dashboard"),
              sidebar,
              body)