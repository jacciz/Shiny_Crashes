library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
# library(DT)
library(plotly)
library(leaflet)
library(shiny)
# library(bsplus)

# layout is Bootstrap (i,e, row widths must add up to 12), helpful to know a little CSS, HTML
source("www/theme_grey_dark.R")  # adds a cool theme

# Style for crash flag table
# table_style = "text-align:center; margin:0px;"

# tags$head(tags$style(HTML(".small-box {color: rgba(0,0,0,1)}"))), # change height, icon size of all value boxes
# tags$style(".small-box {background-color: rgb(52,62,72) !important; color: rgb(52,62,72)!important; }"),
# tags$head(tags$style(HTML(".small-box {height: 60px;} .fa {font-size: 60px; vertical-align: middle;} "))), # change height, icon size of all value boxes

################### SIDEBAR #######################
sidebar <- dashboardSidebar( # .fa-car-band-aid {vertical-align: middle;}
  # changes sliderInput,  materialSwitch, fa is icon,   font size (right click and inspect element to find tag) # fa color: rgb(143,155,179) ??  .fa-car.fa-inverse::before {font-size: 6px;}
  # https://www.w3schools.com/css/ a resource for CSS
  tags$style(type = "text/css", "
      
      .irs-grid-text { font-size: 12pt;}
      .irs-min {font-size: 12pt;}
      .irs-max {font-size: 12pt;}
      .treeview-menu.menu-open {padding: 15px;}
      .Plotly.toImage(gd,{format:'png',height:800,width:800});
      
      .shiny-input-container { font-size: 12pt;}
      .material-switch {font-size: 12pt; float:right; margin-right: 25px;}
      .btn.checkbtn.btn-primary {font-size: 12pt; text-align: left; padding-top:5px; padding-bottom:5px; float:right;}
      
      .fa-calendar {color: rgb(143,155,179)}
      .fa-map-marked-alt {color: rgb(143,155,179)}
      .fa-map {color: rgb(143,155,179)}
      .fa-car-crash {color: rgb(143,155,179)}
      .fa-flag {color: rgb(143,155,179)}
      .fa-angle-left {color: rgb(143,155,179)}
      
      .fa-paper-plane {color: rgb(143,155,179)}
      .fa-users {color: rgb(143,155,179)}
      .fa-bicycle {color: rgb(143,155,179)}
      .fa-walking {color: rgb(143,155,179)}
      .fa-car {color: rgb(143,155,179)}
      
      .small-box .icon-large{top: 0px;}
      .small-box.bg-red {height: 90px; text-align:center; background-color: rgb(34,43,69) !important; color: rgb(34,43,69) !important;}
      .skin-blue .wrapper {background-color: rgb(22,26,48);}

      .main-header .logo {font-family: Arial; font-size: 20px; font-weight: bold}
      .skin-blue .main-header .navbar {background: rgb(34,43,69);}

    "), # add tab margins, 'label' is labelcontrol on map
  width = "250px",  # sidebar width
  sidebarMenu(
    # div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    menuItem(
      strong("Year"),
      tabName = "year",
      icon = icon("calendar"),
      # startExpanded = TRUE, # start expanded
      sliderInput(
        inputId = "year",
        label = "",
        # options = list("actions-box" = TRUE),
        value = c(2017, 2019),
        min = 2017,
        max = 2019,
        # multiple = TRUE, selectize = FALSE,
        # step = 1,
        sep = ""
      )
      
      # selectInput(
      #   "year",
      #   "Year",
      #   choices = NULL,
      #   # options = list("actions-box" = TRUE),
      #   multiple = TRUE, selectize = FALSE
      # )
    ),
    menuItem(
      strong("Location"), # strong makes it bold
      tabName = "location",
      icon = icon("map-marked-alt"),
      # startExpanded = TRUE, # start expanded
      selectInput(
        "cntynum",
        "County",
        choices = NULL,
        # options = list("actions-box" = TRUE),
        multiple = TRUE,
        selectize = FALSE
      ),
      selectInput(
        "muni_names",
        "Municipality (not working)",
        choices = NULL,
        #muni_recode$MUNICIPALITY
        # options = list("actions-box" = TRUE),
        multiple = FALSE,
        selectize = FALSE
      )
    ),
    menuItem(
      strong("Crash Type"),
      tabName = "crash_type",
      icon = icon("car-crash"),
      # startExpanded = TRUE, # start expanded
      
      materialSwitch(inputId = "fatal", label = "Fatal", status = "primary", value = TRUE),
      materialSwitch(inputId = "injury", label = "Injury", status = "primary", value = TRUE),
      materialSwitch(inputId = "propertydamage", label = "Property Damage", status = "primary", value = TRUE)
      ),
    menuItem(
      strong("Flags"),
      tabName = "crash_flags",
      icon = icon("flag"),
      # startExpanded = TRUE, # start expanded
      
      # trying to insert table for icons
      # column(      # # for column, width = NULL
      #   width = 2,
      #   # tags$h5(style = "text-align:left; padding: 15px;", "Crash Flags"),
      #   tags$table(width = "100%",
      #              tags$tr(
      #                tags$td(width = "50%", tags$h5(style = "text-align:center; margin:5px;", icon("paper-plane")))),
      #              tags$tr(
      #                tags$td(width = "50%", tags$h5(style = "text-align:center; margin:5px;", icon("paper-plane"))))
      #   )),
      
      # awesomeCheckboxGroup(
      #   inputId = "id1",
      #   label = "Make a choice:",
      #   choices = c("graphics", "ggplot2")
      # ),
      # prettyCheckboxGroup(
      #   inputId = "checkgroup1",
      #   label = "Click me!",
      #   choices = c("Click me !", "Me !", "Or me !")
      # ),
      checkboxGroupButtons(
        inputId = "crsh_flags",
        label = "Select crashes involving...",
        choices = c(
          "Alcohol-related",
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
        individual = FALSE,
        # all the same width if false
        direction = 'vertical',
        # width = '50%',
        checkIcon = list(
          yes = icon("ok", lib = "glyphicon"),
          no = icon("remove", lib = "glyphicon")
        )
      )
    ),
    
    menuItem(
      strong("Map Settings/Analysis"),
      tabName = "map_sett",
      icon = icon("map"),
      # startExpanded = TRUE, # start expanded

      # checkboxInput("hex", "Show Hex", FALSE),
      materialSwitch("hex", label = "Hex Bins", status = "primary", value = FALSE),
      sliderInput(
        "hexsize",
        "Change Hex Size:",
        min = 1,
        max = 30,
        value = 10,
        ticks = FALSE
      )
    )
    # footer - wisdot logo is here
    # tags$footer(
    #   img(
    #     src = 'zero-logo.png',
    #     style = "width: 100px; display: block;
    #                 margin-left: auto; margin-right: auto; background: grey"
    #   ),
    #   align = "center",
    #   style = "
    #           position:absolute;
    #           bottom:0;
    #           width:100%;
    #           height:150px;   /* Height of the footer */
    #           color: grey;
    #           padding: 10px;
    #           z-index: 1000;"
    # )
    
    
    # tags$h5(style = "text-align:left; padding: 15px;", "Crash Flags"),
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
    #  )
    
  )
)
################### BODY #######################

# The body is separated by tabs
body <- dashboardBody(
  mytheme_grey_dark,  # the awesome theme
  
  # This disables scrollbar inside app, or does it?
  # tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }" ))),

  column(      # # for column, width = NULL
    width = 6,
    #style='padding:5px;',
    fluidRow(
      valueBoxOutput("tot_crash"),
      valueBoxOutput("tot_inj"),
      valueBoxOutput("tot_fatal")
    ),
    tabBox(
      title = NULL,
      width = NULL,
      # The id lets us use input$tabset1 on the server to find the current tab
      # id = "tabset1",
      # height = "600px",
      tabPanel(
        tagList(shiny::icon("paper-plane"), strong("Main")),
        tags$h3(
          "Welcome to the WisDOT Crash Statistics Dashboard."
        ),
        tags$h5(
          "You can explore crash statistics and locations from 2017 - 2019, click on the tabs to display specific data. For charts,
          single click to remove variable and double click to isolate variable.
          Data is provided by TOPS."
        ),
        tags$h5("For data requests, email BOTSTS (link).")
      ),
      tabPanel(
        # NOTE   br(), adds space between charts
        tagList(shiny::icon("car-crash"), strong("Crash trends")),
        plotlyOutput("crsh_svr_mth", height = "240px"),
        br(),
        plotlyOutput("timeofday_heat", height = "240px")
      ),
      tabPanel(
        tagList(shiny::icon("car-crash"), strong("Crash types")),
        plotlyOutput("mnrcoll", height = "240px")
      ),
      tabPanel(
        tagList(shiny::icon("users"), strong("People involved")),
        plotlyOutput("person_role_treemap", height = "250px"),
        br(),
        # plotlyOutput("person_role", height = "240px"),
        plotlyOutput("person_age_gender", height = "240px")
      ),
      tabPanel(
        tagList(shiny::icon("users"), strong("Driver Behavior")),
        tags$h5("Top Driver Contributing Circumstance (DRVRPC)")
        ),
      tabPanel(
        tagList(shiny::icon("bicycle"), shiny::icon("walking"), strong("Bike and Ped. Behavior")),
        tags$h5("Top Actions of Pedestrians and Cyclists (NMTACT)"),
        tags$h5("Top Locations (NMTLOC)")
      ),
      tabPanel(
        tagList(shiny::icon("car"), strong("Vehicles involved")),
        plotlyOutput("vehicle_treemap", height = "240px")
      )
    )
  ),
  column(width = 6,
         leafletOutput("map1", height = "600px") #from 680 # try this instead)
  ))

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "WisDOT Vision Zero"), # text for browser tab
              sidebar,
              body)
