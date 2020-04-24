library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(DT)
library(plotly)
library(d3heatmap)
# library(fresh)    # adds theme, colors
# layout is Bootstrap (i,e, row widths must add up to 12), helpful to know a little CSS, HTML
source("www/theme_grey_dark.R")  # adds a cool theme

#                                                      SIDEBAR
sidebar <- dashboardSidebar(
  width = "250px", # sidebar width
  sidebarMenu(
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
  selectInput(
    "cntynum",
    "County",
    choices = NULL,
    # options = list("actions-box" = TRUE),
    multiple = FALSE, selectize = FALSE
  ),
  selectInput(
    "muni_names",
    "Municipality",
    choices = NULL,
    #muni_recode$MUNICIPALITY
    # options = list("actions-box" = TRUE),
    multiple = FALSE, selectize = FALSE
  ),
  selectInput(
    "year",
    "Year",
    choices = NULL,
    # options = list("actions-box" = TRUE),
    multiple = FALSE, selectize = FALSE
  ),
  checkboxGroupButtons(
    inputId = "crsh_svr",
    label = "Crash Severity:",
    choices = c(
      "Fatal Injury",
      "Suspected Serious Injury",
      "Suspected Minor Injury",
      "Possible Injury",
      "No Apparent Injury"
    ),
    size = 'sm',
    direction = 'vertical',
    justified = TRUE,
    status = "success",
    individual = TRUE,
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon"),
      no = icon("remove", lib = "glyphicon")
    # verbatimTextOutput("crsh_svr_out")
    ),
    selected = c(
      "Fatal Injury",
      "Suspected Serious Injury",
      "Suspected Minor Injury",
      "Possible Injury",
      "No Apparent Injury"
    )
  ), 
  checkboxGroupButtons(
    inputId = "crsh_flags",
    label = "Flag:",
    choices = c(
      "Alcohol-related",
      "Drug-related",
      "Distracted driving",
      "Speeding",
      "Teen driver",
      "Older driver",
      "Bicyclist",
      "Pedestrian",
      "Motorcycle",
      "Seat belt",
      "Intersection & lane dep?"
    ),
    size = 'sm',
    justified = TRUE,
    status = "primary",
    individual = TRUE,
    direction = 'vertical',
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon"),
      no = icon("remove", lib = "glyphicon")
      # verbatimTextOutput("crsh_flags_out")
    )
  ), 
  materialSwitch(inputId = "id", label = "Test switch", status = "danger")
))
#                                                     BODY
body <- dashboardBody(mytheme_grey_dark,  # the awesome theme
  tabItems(
  tabItem(
    tabName = "dashboard",
    tags$h5("xxxx County, 2019, All Crashes"),
    #                                                     FIRST TAB X row
    fluidRow(
       # tags$head(tags$style(HTML(".small-box {height: 80px;} .fa {font-size: 10px; vertical-align: middle;} "))), # change height, icon size of all value boxes
        valueBoxOutput("tot_crash", width = 2), # for column, width = NULL
        valueBoxOutput("tot_inj",  width = 2),
        valueBoxOutput("tot_fatal", width = 2),
        valueBoxOutput("passveh_box", width = 2),
        valueBoxOutput("light_truck_box", width = 2),
        valueBoxOutput("large_truck_box", width = 2),
        # valueBoxOutput("tot_some", width = NULL)
    ),
    fluidRow(
      # Dynamic infoBoxes
      valueBoxOutput("", width = 2),
      valueBoxOutput("", width = 2),
      valueBoxOutput("", width = 2),
      valueBoxOutput("motorcycle_box", width = 2),
      valueBoxOutput("bike_box", width = 2),
      valueBoxOutput("ped_box", width = 2)
    ),
      fluidRow(
      box(
        title = ("Crash Severity by Month"),
        width = 4,
        solidHeader = TRUE,
        HTML("<div style='height: 200px;'>"),
        plotlyOutput("crsh_svr_mth", height = "220px"),
        HTML("</div>")
        # plotlyOutput("crsh_svr_mth", height = "200px", inline = T)
      ),
      box(
        title = ("Time of Day Crashes"),
        width = 4,
        solidHeader = TRUE,
        HTML("<div style='height: 200px;'>"),
        d3heatmapOutput("timeofday_heat", height = "250px", width = "115%"),
        HTML("</div>")
        # d3heatmapOutput("timeofday_heat", height = "200px", width = "110%")
      ),
      box("Manner of Collision",
        width = 4,
        solidHeader = TRUE,
        HTML("<div style='height: 220px;'>"),
        plotlyOutput("mnrcoll", height = "240px", inline = T),
        HTML("</div>")
        # plotlyOutput("mnrcoll", height = "200px", inline = T)
    )),
    # div(style = " padding: 0px 0px; margin-top:-2em"),
  fluidRow(
    box(
      title = "Role of Persons",
      width = 4,
      solidHeader = TRUE,
      plotlyOutput("person_role", height = "200px", inline = T)
    ),
    box(
      title = "Age and Gender of Persons Involved",
      width = 4,
      solidHeader = TRUE,
      plotlyOutput("person_age_gender", height = "200px", inline = T)
    )
  )
  ),
# Table Tab
  tabItem(tabName = "tables",
          h2("Tables tab content"),
          
          fluidRow(box(
            width = 6, DTOutput("biketable", height = 600)
          )))
))

# Put them together into a dashboardPage
dashboardPage(
  # skin = "blue_gradient",   #add a theme
  dashboardHeader(title = logo_mytheme),
              sidebar,
              body)