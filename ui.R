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
table_style = "text-align:center; margin:0px;"

################### SIDEBAR #######################
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
    label = "Select Flags",
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
  ),
 
  tags$h5(style = "text-align:left; padding: 15px;", "Map Settings"),
 checkboxInput("hex", "Show Hex", FALSE),
 sliderInput(
   "hexsize",
   "Change Hex Size:",
   min = 1,
   max = 30,
   value = 10
 )
))
################### BODY #######################
body <- dashboardBody(mytheme_grey_dark,  # the awesome theme
  tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow( #style='padding:5px;',
        # tags$head(tags$style(HTML(".small-box {color: rgba(0,0,0,1)}"))), # change height, icon size of all value boxes
        tags$style(".small-box {background-color: rgb(52,62,72) !important; color: rgb(52,62,72)!important; }"),
        # tags$head(tags$style(HTML(".small-box {height: 60px;} .fa {font-size: 60px; vertical-align: middle;} "))), # change height, icon size of all value boxes
        valueBoxOutput("tot_crash", width = 2),
        # # for column, width = NULL
        valueBoxOutput("tot_inj", width = 2),
        valueBoxOutput("tot_fatal", width = 2),
        valueBoxOutput("passveh_box", width = 2),
        valueBoxOutput("light_truck_box", width = 2),
        valueBoxOutput("large_truck_box", width = 2)
      ),
      fluidRow(
        column( # column layout
          width = 3, offset = 0, style='padding:5px;',
          box(
            width = NULL, # MUST BE NULL FOR COLUMN LAYOUT
            HTML("<div style='height: 220px;'>"), # this makes chart fit in box
            plotlyOutput("crsh_svr_mth", height = "240px"),
            HTML("</div>")
          ),
          box(
            width = NULL,
            # HTML("<div style='height: 220px;'>"), # this makes chart fit in box
            plotlyOutput("timeofday_heat", height = "240px")
            # HTML("</div>")
          )
        ),
        column(
          # p(
          #   actionButton("map_btn", "Show Hex")
          # ),
          width = 6, offset = 0, style='padding:5px;',
          box(
            width = NULL,
            HTML("<div style='height: 660px;'>"),
            uiOutput("map"),
            HTML("</div>")
          )
        ),
        column(
          width = 3, offset = 0, style='padding:5px;',
          box(
            width = NULL,
            HTML("<div style='height: 220px;'>"), # this makes chart fit in box
            plotlyOutput("mnrcoll", height = "240px"),
            HTML("</div>")
            # plotlyOutput("mnrcoll", height = "200px", inline = T)
          ),
          box(
            width = NULL,
            HTML("<div style='height: 220px;'>"),
            plotlyOutput("person_role", height = "240px"),
            HTML("</div>")
          ),
          box(
            width = NULL,
            HTML("<div style='height: 220px;'>"),
            plotlyOutput("person_age_gender", height = "240px"),
            HTML("</div>")
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
