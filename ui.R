library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(d3heatmap)
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
  ),
  pickerInput(
    "year",
    "Year",
    choices = NULL,
    options = list("actions-box" = TRUE),
    multiple = FALSE
  )
))
#                                                     BODY
body <- dashboardBody(tabItems(
  tabItem(
    tabName = "dashboard",
    h2("Dane County, 2019, All Crashes"),
    #                                                     FIRST TAB X row
    fluidRow(
      column(width = 2,
        valueBoxOutput("tot_crash", width = NULL), # for column, width = NULL
        valueBoxOutput("tot_inj",  width = NULL),
        valueBoxOutput("tot_fatal", width = NULL)
        # valueBoxOutput("tot_some", width = NULL)
      ),
      column(width = 5,
      box(
        title = "Bike and Pedestrian Crashes",
        width = NULL,
        solidHeader = TRUE,
        plotOutput("bike_ped_flag", height = "300px")
      )),
      column(width = 5,
      box(
        title = "Time of Day Crashes",
        width = NULL,
        solidHeader = TRUE,
        d3heatmapOutput("timeofday_heat", height = "300px")
      ))
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
        width = 5,
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
      title = "Role of Persons Bar Chart",
      width = 5,
      solidHeader = TRUE,
      plotlyOutput("", height = "300px")
    ),
    box(
      title = "Age/Gender Histogram",
      width = 5,
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