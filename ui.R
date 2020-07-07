library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(DT)
library(plotly)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# layout is Bootstrap (i,e, row widths must add up to 12), helpful to know a little CSS, HTML
source("www/theme_grey_dark.R")  # adds a cool theme

# Style for crash flag table
# table_style = "text-align:center; margin:0px;"

################### SIDEBAR #######################
sidebar <- dashboardSidebar(
  width = "250px", # sidebar width
  sidebarMenu(
  selectInput(
    "cntynum",
    "County",
    choices = NULL,
    # options = list("actions-box" = TRUE),
    multiple = TRUE, selectize = FALSE
  ),
  selectInput(
    "muni_names",
    "Municipality (not working)",
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
    multiple = TRUE, selectize = FALSE
  ),
  checkboxGroupButtons(
    inputId = "crsh_svr",
    label = "Crash Severity:",
    width = "60%",
    choices = c(
      "Fatal",
      "Injury",
      "Property Damage"
    ),
    size = 'sm',
    direction = 'vertical',
    justified = TRUE,
    status = "success",
    individual = FALSE,
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon"),
      no = icon("remove", lib = "glyphicon")
    # verbatimTextOutput("inj_svr_out")
    ),
    selected = c(
      "Fatal",
      "Injury",
      "Property Damage"
    )
  ),
 #  tags$h5(style = "text-align:left; padding: 15px;", "Crash Flags"),
 #  tags$table(width = "100%",
 #    tags$tr(
 #            tags$td(width = "50%", tags$h5(style = table_style, "Alcohol-related")),
 #            tags$td(width = "50%", tags$h5(style = table_style, "Drug-related"))),
 #    tags$tr(
 #            tags$td(width = "50%",  switchInput(inputId = "DRUGFLAG", value = FALSE, size = 'mini', inline = TRUE)),
 #            tags$td(width = "50%",  switchInput(inputId = "drugflag2", value = FALSE, size = 'mini', inline = TRUE))),
 # 
 # tags$tr(
 #   tags$td(width = "50%", tags$h5(style = table_style, "Distracted driving")),
 #   tags$td(width = "50%", tags$h5(style = table_style, "Speeding"))),
 # tags$tr(
 #   tags$td(width = "50%",  switchInput(inputId = "drugflag3", value = FALSE, size = 'mini', inline = TRUE)),
 #   tags$td(width = "50%",  switchInput(inputId = "drugflag4", value = FALSE, size = 'mini', inline = TRUE)))
 #  ),
  checkboxGroupButtons(
    inputId = "crsh_flags",
    label = "Select crashes involving...",
    choices = c(
      "Alcohol-related", # icon("calendar")  
      "Distracted driving (NO)",
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
    )
  )
))
################### BODY #######################
# The body is separated by tabs
body <- dashboardBody(mytheme_grey_dark,  # the awesome theme
      
      # This disables scrollbar inside app, or does it?
      # tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }" ))),
      
      # valueBoxOutput("tot_crash", width = 2),
      # # # for column, width = NULL
      # valueBoxOutput("tot_inj", width = 2),
      # valueBoxOutput("tot_fatal", width = 2),
      column(width = 6,#style='padding:5px;',
        fluidRow(valueBoxOutput("tot_crash"),
        # # for column, width = NULL
        valueBoxOutput("tot_inj"),
        valueBoxOutput("tot_fatal")),
        # tags$head(tags$style(HTML(".small-box {color: rgba(0,0,0,1)}"))), # change height, icon size of all value boxes
        # tags$style(".small-box {background-color: rgb(52,62,72) !important; color: rgb(52,62,72)!important; }"),
        # tags$head(tags$style(HTML(".small-box {height: 60px;} .fa {font-size: 60px; vertical-align: middle;} "))), # change height, icon size of all value boxes
        tags$head(tags$style(HTML(".small-box {height: 90px; text-align:center;} .fa {vertical-align: middle;} "))), # change height, icon size of all value boxes
        
        tabBox(
          title = NULL,
          width = NULL,
          # The id lets us use input$tabset1 on the server to find the current tab
          # id = "tabset1",
          # height = "600px",
          tabPanel(
            tagList(shiny::icon("paper-plane"), "Main"),
            tags$h3(
              "Welcome to the Wisconsin Department of Transportation Crash Statistics Dashboard."
            ),
            tags$h5(
              "You can explore crash statistics and locations from 2017 - 2019, click on the tabs to display specific data. Data is provided by TOPS."
            ),
            tags$h5("For data requests, email BOTSTS (link).")
          ),
          tabPanel( # NOTE   br(), adds space between charts
            tagList(shiny::icon("car-crash"), "Crash trends"),
            plotlyOutput("crsh_svr_mth", height = "240px"), br(),
            plotlyOutput("timeofday_heat", height = "240px")
          ),
          tabPanel(
            tagList(shiny::icon("car-crash"), "Crash types"),
            plotlyOutput("mnrcoll", height = "240px")
          ),
          tabPanel(
            tagList(shiny::icon("users"), "People involved"),
            plotlyOutput("person_role_treemap", height = "250px"),  br(),
            # plotlyOutput("person_role", height = "240px"),
            plotlyOutput("person_age_gender", height = "240px")
          ),
          tabPanel(
            tagList(shiny::icon("users"), "Driver Behaviour")
          ),
          tabPanel(
            tagList(shiny::icon("car"), "Vehicles involved"),
            plotlyOutput("vehicle_treemap", height = "240px")
                   # valueBoxOutput("passveh_box"),
                   # valueBoxOutput("light_truck_box"),
                   # valueBoxOutput("large_truck_box")
                   ),
          tabPanel(
            tagList(shiny::icon("map"), "Map Settings/Analysis"),
                   checkboxInput("hex", "Show Hex", FALSE),
                   sliderInput(
                     "hexsize",
                     "Change Hex Size:",
                     min = 1,
                     max = 30,
                     value = 10
                   ))
        )), 
      column(width = 6,
            uiOutput("map"),
            leafletOutput("map1", height = "600px") #from 680 # try this instead
            
      )
        )

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "WisDOT Vision Zero"),
              sidebar,
              body)
