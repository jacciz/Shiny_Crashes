library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
#                                                      SIDEBAR
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
  ),
  pickerInput(
    "cntynum",
    "County",
    choices = NULL,
    #now null sort(unique(all_crashes$CNTYCODE)
    options = list("actions-box" = TRUE),
    multiple = FALSE,
    selected = "Dane"
  ),
  pickerInput(
    "muni_names",
    "Municipality",
    choices = NULL,
    #muni_recode$MUNICIPALITY
    options = list("actions-box" = TRUE),
    multiple = FALSE
  )
))
#                                                     BODY
body <- dashboardBody(tabItems(
  tabItem(
    tabName = "dashboard",
    h2("County, data type, year here"),
    #                                                     FIRST TAB X row
    fluidRow(
      column(width = 4,
        valueBoxOutput("tot_crash"), # I want these stacked
        valueBoxOutput("tot_inj")
        # ,
        # valueBoxOutput("tot_fatal"),
        # valueBoxOutput("tot_some")
      ),
      
      box(
        title = "Bike and Pedestrian Crashes",
        width = 4,
        solidHeader = TRUE,
        plotOutput("bike_ped_flag", height = "300px")
      ),
      box(
        title = "Time of Day Crashes",
        width = 4,
        solidHeader = TRUE,
        d3heatmapOutput("timeofday_heat", height = "300px")
      )
    ),
    # #                                                     FIRST TAB X row
    # fluidRow(
    #   box(
    #     title = "Alcohol Flag Crashes",
    #     width = 4,
    #     solidHeader = TRUE,
    #     plotOutput("alcflag", height = "300px")
    #   )
    # ),

    #                                                     FIRST TAB X row
    fluidRow(
      box(
        title = "Manner of Collision",
        width = 4,
        solidHeader = TRUE,
        plotlyOutput("mnrcoll", height = "300px")
      )
    ),
    
    #                                                     FIRST TAB X row
    fluidRow(
      box("Vehicle Type Involved", width = 12)
      ,
      # Dynamic infoBoxes
      valueBoxOutput("passveh_box", width = 2),
      valueBoxOutput("light_truck_box", width = 2),
      valueBoxOutput("large_truck_box", width = 2),
      valueBoxOutput("motorcycle_box", width = 2),
      valueBoxOutput("bike_box", width = 2),
      valueBoxOutput("ped_box", width = 2)
    ),
  
  #                                                     FIRST TAB X row
  fluidRow(
    box("Role of Persons Involved", width = 12)
    ,
    box(
      title = "Role of Persons Chart",
      width = 4,
      solidHeader = TRUE,
      plotlyOutput("", height = "300px")
    )
  )
  ),
  #                                                   SECOND TAB    First row
  tabItem(tabName = "tables",
          h2("Tables tab content"),
          
          fluidRow(box(
            width = 6, DTOutput("biketable", height = 600)
          )))
))


# Put them together into a dashboardPage
dashboardPage(dashboardHeader(title = "WisDOT Vision Zero Dashboard"),
              sidebar,
              body)