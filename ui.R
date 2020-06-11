library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(DT)
library(plotly)
library(d3heatmap)
library(leaflet)
library(htmltools)
library(htmlwidgets)
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
    inputId = "inj_svr",
    label = "Injury Severity:",
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
    # verbatimTextOutput("inj_svr_out")
    ),
    selected = c(
      "Fatal Injury",
      "Suspected Serious Injury",
      "Suspected Minor Injury",
      "Possible Injury",
      "No Apparent Injury"
    )
  ),
  # column(12, h5("Alcohol-related", style="display: inline-block; text-align:top; width: 0px;" ), 
  # switchInput(inputId = "alcflag", value = FALSE, label = "", size = 'mini', inline = TRUE, width = "125px"),
  # h5("Drug-related", style="display: inline-block; text-align:top; width: 0px;"),
  # switchInput(inputId = "drugflag", value = FALSE, label = "", size = 'mini', inline = TRUE, width = "125px")
  # ),
 #  tags$head(
 #    tags$style(type="text/css","label{ display: table-cell; text-align: center;vertical-align: middle; } .form-group { display: table-row;}") 
 #  ),
 # br(),
  tags$table(width = "100%",
    tags$tr(
            tags$td(width = "50%", tags$div(style = "text-align:center;", "Alcohol-related")),
            tags$td(width = "50%", tags$div(style = "text-align:center;", "Drug-related"))),
    tags$tr(
            tags$td(width = "50%",  switchInput(inputId = "drugflag1", value = FALSE, size = 'mini', inline = TRUE)),
            tags$td(width = "50%",  switchInput(inputId = "drugflag2", value = FALSE, size = 'mini', inline = TRUE))),
 
 tags$tr(
   tags$td(width = "50%", tags$div(style = "text-align:center;", "Distracted driving")),
   tags$td(width = "50%", tags$div(style = "text-align:center;", "Speeding"))),
 tags$tr(
   tags$td(width = "50%",  switchInput(inputId = "drugflag3", value = FALSE, size = 'mini', inline = TRUE)),
   tags$td(width = "50%",  switchInput(inputId = "drugflag4", value = FALSE, size = 'mini', inline = TRUE)))
  ),
  # materialSwitch(inputId = "alcflag", label = "Alcohol-related", status = "primary", inline = TRUE, width = "125px"),
  # materialSwitch(inputId = "drugflag", label = "Drug-related", status = "primary", inline = FALSE, width = "125px"),
  # materialSwitch(inputId = "distflag", label = "Distracted driving", status = "primary", inline = FALSE, width = "125px"),
  # materialSwitch(inputId = "speedflag", label = "Speeding", status = "primary", inline = TRUE, width = "125px"),
  
  checkboxGroupButtons(
    inputId = "crsh_flags",
    label = "Flag: (Selection = Or)",
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
    size = 'xs',
    justified = FALSE,
    status = "primary",
    individual = FALSE, # all the same width if false
    direction = 'vertical',
    # width = '50%',
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon"),
      no = icon("remove", lib = "glyphicon")
      # verbatimTextOutput("crsh_flags_out")
    )
  )
))
#                                                     BODY
body <- dashboardBody(mytheme_grey_dark,  # the awesome theme
                      # tags$head(tags$script(src = jsfile)), # for hex js file for map
  tabItems(
    tabItem(
      tabName = "dashboard",
      # tags$h5("xxxx County, 2019, All Crashes"),
      #                                                     FIRST TAB X row
      fluidRow(
        # tags$head(tags$style(HTML(".small-box {height: 60px;} .fa {font-size: 60px; vertical-align: middle;} "))), # change height, icon size of all value boxes
        valueBoxOutput("tot_crash", width = 2),
        # # for column, width = NULL
        valueBoxOutput("tot_inj", width = 2),
        valueBoxOutput("tot_fatal", width = 2),
        valueBoxOutput("passveh_box", width = 2),
        valueBoxOutput("light_truck_box", width = 2),
        valueBoxOutput("large_truck_box", width = 2),
        # valueBoxOutput("tot_some", width = NULL) total cars?

        # column(
        #   width = 2,
        #   # Dynamic infoBoxes

        #   valueBoxOutput("motorcycle_box", width = NULL),
        #   valueBoxOutput("ped_box", width = NULL),
        #   valueBoxOutput("bike_box", width = NULL)
        # )
      ),
      fluidRow(
        column(
          width = 3,
          box(
            width = NULL,
            HTML("<div style='height: 220px;'>"), # this make chart fit in box
            plotlyOutput("crsh_svr_mth", height = "240px"),
            HTML("</div>")
          ),
          box(
            width = NULL,
            plotlyOutput("timeofday_heat", height = "240px", inline = T)
          ),
        ),
        column(
          # p(
          #   actionButton("map_btn", "Show Hex")
          # ),
          width = 6,
          box(
            width = NULL,
            # solidHeader = TRUE,
            uiOutput("map"),
            checkboxInput("hex", "Show Hex", FALSE),
            sliderInput(
              "hexsize",
              "Change Hex Size:",
              min = 1,
              max = 30,
              value = 10
            )
          )
        ),
        column(
          width = 3,
          box(
            width = NULL,
            plotlyOutput("mnrcoll", height = "240px"),
            # plotlyOutput("mnrcoll", height = "200px", inline = T)
          ),
          box(
            width = NULL,
            plotlyOutput("person_role", height = "240px", inline = T)
          ),
          box(
            width = NULL,
            plotlyOutput("person_age_gender", height = "240px", inline = T)
          )
        )
      )
    ),
# Table Tab
tabItem(tabName = "tables",
        h2("Tables tab content"),
        fluidRow(box(
          width = 6, DTOutput("biketable", height = 600)
        )))
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "WisDOT Vision Zero"),
              sidebar,
              body)
