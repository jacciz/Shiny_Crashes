library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(plotly)
library(leaflet)
library(shiny)
# library(bsplus)
library(leafgl)

# layout is Bootstrap (i,e, row widths must add up to 12), helpful to know a little CSS, HTML
source("www/theme_grey_dark.R")  # adds a cool theme

################### SIDEBAR #######################

sidebar <- dashboardSidebar( # .fa-car-band-aid {vertical-align: middle;}
  # changes sliderInput,  materialSwitch, fa is icon,   font size (right click and inspect element to find tag) # fa color: rgb(143,155,179) ??  .fa-car.fa-inverse::before {font-size: 6px;}
  # https://www.w3schools.com/css/ a resource for CSS
  # https://codepen.io/sosuke/pen/Pjoqqp to get FILTER colors for SVG (like hexbin) - plug in color and scroll down
  # https://icons8.com/ free icons - recolor (#8F9BB3) before download at 60pxPNG
  includeCSS("www/widgets.css"), # this applies css to certain shinyWidgets, I basically just changed the default settings
  tags$style(type = "text/css", "
      
      .irs-grid-text {font-size: 12pt;}
      .irs-min {font-size: 12pt;}
      .irs-max {font-size: 12pt;}

      .hexbin_svg {display: inline-block; width: 15px; height: 15px; background-size: cover;
      filter: invert(80%) sepia(84%) saturate(3572%) hue-rotate(177deg) brightness(102%) contrast(92%);}

      .cluster_on_svg {display: inline-block; width: 20px; height: 20px; background-size: cover;
      filter: invert(71%) sepia(82%) saturate(941%) hue-rotate(323deg) brightness(97%) contrast(96%);}

      .cluster_off_svg {display: inline-block; width: 20px; height: 20px; background-size: cover;
      filter: invert(48%) sepia(65%) saturate(0%) hue-rotate(167deg) brightness(92%) contrast(92%);}

      .Plotly.toImage(gd,{format:'png',height:800,width:800});
      .shiny-input-container {font-size: 12pt;}
      
      .sidebar-menu .treeview-menu {padding-left: 0px;}
      
      .h5, h5 {font-size: 14px; margin-top: 30px; margin-bottom: 0px; font-weight: bold; padding-left: 40px}

      .material-switch {float:right; margin-right: 25px;}

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
      
      .leaflet-control-layers-expanded{background-color: rgb(34,43,69)}

    "), # add tab margins, 'label' is labelcontrol on map, last one doesnt work
  # tags$li(class = "dropdown",
  #         tags$style(".main-header {max-height: 75px}"),
  #         tags$style(".main-header .logo {height: 80px}")
  # ),
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
        label = "Select Year",
        # options = list("actions-box" = TRUE),
        value = c(2017, 2019),
        min = 2017,
        max = 2019,
        # multiple = TRUE, selectize = FALSE,
        # step = 1,
        sep = ""
      )
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
      )
      # selectInput(
      #   "muni_names",
      #   "Municipality (not working)",
      #   choices = NULL,
      #   #muni_recode$MUNICIPALITY
      #   # options = list("actions-box" = TRUE),
      #   multiple = FALSE,
      #   selectize = FALSE
      # )
    ),
    menuItem(
      strong("Crash Type"),
      tabName = "crash_type",
      icon = icon("car-crash"),
      # startExpanded = TRUE, # start expanded
      # materialSwitch(inputId = "fatal", label = span("Fatal", style = "color:#D50032"), status = "danger", value = TRUE), 
      # set to each have 15 characters to fake left align: 1 space - &nbsp;  2 - &ensp; 4 - &emsp;
      materialSwitch(inputId = "fatal", label = tags$span(HTML('<i class="fa fa-heartbeat" style = "color:#D50032;"></i><p style="display:inline-block;">&ensp;Fatal&emsp;&emsp;&ensp;</p>')), status = "danger", value = TRUE),
      materialSwitch(inputId = "injury", label = tags$span(HTML('<i class="fa fa-first-aid" style = "color:#428BCA;"></i><p style="display:inline-block;">&ensp;Injury&emsp;&emsp;&nbsp;</p>')), status = "info", value = TRUE),
      materialSwitch(inputId = "propertydamage", label = tags$span(HTML('<i class="fa fa-car" style = "color:#4DB848;"></i><p style="display:inline-block;">&ensp;Property Damage</p>')), status = "success", value = TRUE),
      tags$h5("")
      ),
    menuItem(
      strong("Flags"),
      tabName = "crash_flags",
      icon = icon("flag"),
      startExpanded = FALSE, # start expanded
      # <br> add a break line
      tags$h5("Selection type:", style="display:inline-block; margin-top: 10px;"),
      tags$h5(switchInput(inputId = "any_or_all", value = TRUE, onLabel = "Any", offLabel = "All", onStatus = "primary", offStatus = "primary", size = "mini", inline = TRUE), style="display:inline-block; margin-top: 10px; padding: 0px;"),
      tags$h5("Driver behavior", style = "margin-top: 10px;"),
      # set to each have 15 characters to fake left align: 1 space - &nbsp;  2 - &ensp; 4 - &emsp;
      materialSwitch(inputId = "alc", label = tags$span(HTML('<i class="fa fa-glass-martini" style = "color:rgb(143,155,179);"></i> Alcohol-related&nbsp; ')), status = "info"),
      materialSwitch(inputId = "drug", label = tags$span(HTML('<i class="fa fa-pills" style = "color:rgb(143,155,179);"></i> Drug-related &ensp; &nbsp;')), status = "info"),
      materialSwitch(inputId = "speed", label = tags$span(HTML('<i class="fa fa-tachometer" style = "color:rgb(143,155,179);"></i> Speeding &emsp;&ensp; &nbsp;&nbsp; ')), status = "info"),
      materialSwitch(inputId = "lanedep", label = tags$span(HTML('<i class="fa fa-road" style = "color:rgb(143,155,179);"></i> Lane Departure')), status = "info"),
      
      tags$h5("Driver age"),
      materialSwitch(inputId = "teen", label = tags$span(HTML('<img src="icons8-driver-60.png" style="width:16px;height:16px;"></i> Teen driver &emsp;&nbsp; &nbsp; ')), status = "info"),
      materialSwitch(inputId = "older", label = tags$span(HTML('<img src="icons8-driver-60.png" style="width:16px;height:16px;"></i> Older driver &ensp;&nbsp;&nbsp; &nbsp; ')), status = "info"),
      tags$h5("Who's involved"),
      materialSwitch(inputId = "motorcycle", label = tags$span(HTML('<i class="fa fa-motorcycle" style = "color:rgb(143,155,179);"></i> Motorcycle &emsp;&nbsp;&nbsp;&nbsp;')), status = "info"),
      materialSwitch(inputId = "ped", label = tags$span(HTML('<i class="fa fa-walking" style = "color:rgb(143,155,179);"></i> Pedestrian &emsp;&ensp;&nbsp;&nbsp;')), status = "info"),
      materialSwitch(inputId = "bike", label = tags$span(HTML('<i class="fa fa-bicycle" style = "color:rgb(143,155,179);"></i> Bicycle &emsp;&emsp;&emsp;&nbsp;&nbsp;')), status = "info"),
      # tags$h5("Person behavior"),
      # materialSwitch(inputId = "seatbelt", label = tags$span(HTML('<i class="fa fa-car" style = "color:rgb(143,155,179);"></i> Seat belt (NO)')), status = "info"),
      tags$h5("Other"),
      materialSwitch(inputId = "singleveh", label = tags$span(HTML('<img src="icons8-traffic-accident-50.png" style="width:16px;height:16px;"></i> Single Vehicle &nbsp;')), status = "info"),
      materialSwitch(inputId = "deer", label = tags$span(HTML('<img src="icons8-deer-52.png" style="width:15px;height:15px;"></i> Deer &emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;')), status = "info")
      ), # <img src="icons8-deer-52.png"">
    menuItem(
      strong("Map Settings/Analysis"),
      tabName = "map_sett",
      icon = icon("map"),
      startExpanded = TRUE,  # start expanded
      materialSwitch(
        "hex", 
        label = tags$span(
          HTML(
            '</svg><image class="hexbin_svg" src="hexbin.svg" /> Hex Bins'
          )
        ),
        status = "info",
        value = FALSE #tags$span(HTML('<i class="fa fa-car" style = "color:#4DB848;"></i><p style="display:inline-block;">xx</p>'))
      ),
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
    #                 margin-left: auto; margin-right: auto;"
    #   ),
    #   align = "center",
    #   style = "
    #           position:absolute;
    #           bottom:0;
    #           width:100%;
    #           height:50px;   /* Height of the footer */
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
        
        tagList(tags$span(
          HTML(
            "<h3>Welcome to the WisDOT Interactive Crash Statistics Dashboard.</h3>
        <h6 style=font-size:14px;>Explore crash statistics by county from 2017 - 2019. Click on the above tabs
          to display different charts and click of the sidebar tabs to select parameters. Charts can be downloaded as a PNG and are also interactive; single click to remove a variable and double
          click to isolate a variable.</h6>
          
          <h6 style=font-size:14px;> For a data request email BOTSTS at <a href=mailto:CrashDataAnalysis@dot.wi.gov>CrashDataAnalysis@dot.wi.gov</a> </h6>
            <img src=tops-lab-logo.png style=height:100px;display:inline-block;background-color:white>
            <img src=zero-logo.png style=height:100px;display:inline-block;background-color:white;>"
          )
        ))
      ),
      tabPanel(
        # NOTE   br(), adds space between charts
        tagList(shiny::icon("car-crash"), strong("Crash trends")),
        plotlyOutput("crsh_svr_mth", height = "240px"),
        br(),
        plotlyOutput("timeofday_heat", height = "240px")
      ),
      tabPanel(
        tagList(tags$span(HTML('<img src="icons8-car-crash-50.png" style="width:16px;height:16px;"></i>')), strong("Crash types")),
        plotlyOutput("mnrcoll", height = "240px")
      ),
      tabPanel(
        tagList(shiny::icon("users"), strong("People involved")),
        plotlyOutput("person_role_treemap", height = "250px"),
        br(),
        plotlyOutput("person_age_gender", height = "240px"),
      ),
      tabPanel(
        tagList(tags$span(HTML('<img src="icons8-driver-60.png" style="width:16px;height:16px;"></i>')), strong("Driver Behavior")),
        plotlyOutput("drvrpc_chart", height = "240px")
        ),
      tabPanel(
        tagList(shiny::icon("bicycle"), shiny::icon("walking"), strong("Bike and Ped. Behavior")),
        plotlyOutput("nmtact_chart", height = "240px")
        # br(),
        # plotlyOutput("nmtloc_chart", height = "240px")
      ),
      tabPanel(
        tagList(shiny::icon("car"), strong("Vehicles involved")),
        plotlyOutput("vehicle_treemap", height = "240px")
      )
    )
  ),
  column(width = 6,
         leafglOutput("map1", height = "600px")
         # leafletOutput("map1", height = "600px") #from 680 # try this instead)
  ))

# Put them together into a dashboardPage
# adding icons on right side of header: https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
dashboardPage( title = "WisDOT Crash Dashboard", # browser tab name
  dashboardHeader(
    title = logo_mytheme,
    titleWidth = 230
  
  ), # text for browser tab and page title
  sidebar, 
              body)
