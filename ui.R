library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets) # need this??
library(dashboardthemes) # theme.R files
library(plotly)
library(leaflet)
library(shiny)
# library(bsplus)
# library(leafgl)
# library(shinythemes) # Not for dashboardSidebar

# layout is Bootstrap (i,e, row widths must add up to 12), helpful to know a little CSS, HTML
# source("www/theme_grey_dark.R")  # adds a cool theme

source("www/theme_grey_light.R")  # adds a cool theme
################### SIDEBAR #######################

sidebar <- dashboardSidebar( # .fa-car-band-aid {vertical-align: middle;}
  # Adjust the sidebar
  # tags$style(".left-side, .main-sidebar {padding-top: 20px}"),
  # tags$head( # Let's use a theme!
  #   tags$link(rel = "stylesheet", type = "text/css", href = "lux.css")
  # ),
  # changes sliderInput,  materialSwitch, fa is icon,   font size (right click and inspect element to find tag) # fa color: rgb(115,115,115) ??  .fa-car.fa-inverse::before {font-size: 6px;}
  # https://www.w3schools.com/css/ a resource for CSS
  # https://codepen.io/sosuke/pen/Pjoqqp to get FILTER colors for SVG (like hexbin) - plug in color and scroll down
  # https://icons8.com/ free icons - recolor (#8F9BB3) before download at 60pxPNG
  # body{color: black;} this is HEXES hover font color
  includeCSS("www/widgets.css"), # this applies css to certain shinyWidgets, I basically just changed the default aesthetic settings
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

      body{color: black;}
      
      .sidebar{color: rgb(100,100,100)}
      
      .material-switch {float:right; margin-right: 25px;}

      .btn.checkbtn.btn-primary {font-size: 12pt; text-align: left; padding-top:5px; padding-bottom:5px; float:right;}

      .small-box .icon-large{top: 0px;}
      .small-box.bg-red {height: 70px; text-align:center; background-color: rgb(248,248,248) !important; color: rgb(248,248,248) !important;}
      .small-box p {color: rgb(100,100,100)}
      
      .skin-blue .wrapper {background-color: #1d4f81;}

      .main-header .logo {font-size: 20px; font-weight: normal;}
      .skin-blue .main-header .navbar {background: #1d4f81;}
      .skin-blue .main-header .logo  {background: #1d4f81;}
       
      .skin-blue .main-header .navbar .sidebar-toggle {background: #1d4f81;}

      .leaflet-control-layers-expanded{background-color: rgb(120,120,120)}

    "), # add tab margins, 'label' is labelcontrol on map, last one doesnt work
  width = "250px",  # sidebar width
  sidebarMenu(
    # div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    shinydashboard::menuItem(
      strong("Year"),
      tabName = "year",
      icon = icon("calendar"),
      select_year("year") # Module
      # textOutput("out")
    ),
    shinydashboard::menuItem(
      strong("Location"), # strong makes it bold
      tabName = "location",
      icon = icon("map-marked-alt"),
      startExpanded = TRUE, # start expanded
      select_county_input("cntycode_input") # Module
      # select_municode_input("municode_input") # Module
    ),
    shinydashboard::menuItem(
      strong("Crash Severity"),
      tabName = "crash_type",
      icon = icon("car-crash"),
      # startExpanded = TRUE, # start expanded
      # materialSwitch(inputId = "fatal", label = span("Fatal", style = "color:#D50032"), status = "danger", value = TRUE), 
      # set to each have 15 characters to fake left align: 1 space - &nbsp;  2 - &ensp; 4 - &emsp;
      materialSwitch(inputId = "Fatal", label = tags$span(HTML('<i class="fa fa-heartbeat" style = "color:#D50032;"></i><p style="display:inline-block;">&ensp;Fatal&emsp;&emsp;&ensp;</p>')), status = "danger", value = TRUE),
      materialSwitch(inputId = "Injury", label = tags$span(HTML('<i class="fa fa-first-aid" style = "color:#428BCA;"></i><p style="display:inline-block;">&ensp;Injury&emsp;&emsp;&nbsp;</p>')), status = "info", value = TRUE),
      materialSwitch(inputId = "Property Damage", label = tags$span(HTML('<i class="fa fa-car" style = "color:#4DB848;"></i><p style="display:inline-block;">&ensp;Prop. Damage</p>')), status = "success", value = TRUE),
      tags$h5("")
      ),
    shinydashboard::menuItem(
      strong("Flags"),
      tabName = "crash_flags",
      icon = icon("flag"),
      startExpanded = FALSE, # start expanded
      # <br> add a break line
      tags$h5("Selection type:", style="display:inline-block; margin-top: 10px;"),
      tags$h5(switchInput(inputId = "any_or_all", value = TRUE, onLabel = "Any", offLabel = "All", onStatus = "primary", offStatus = "primary", size = "mini", inline = TRUE), style="display:inline-block; margin-top: 10px; padding: 0px;"),
      tags$h5("Driver behavior", style = "margin-top: 10px;"),
      # set to each have 15 characters to fake left align: 1 space - &nbsp;  2 - &ensp; 4 - &emsp;
      materialSwitch(inputId = "ALCFLAG", label = tags$span(HTML('<i class="fa fa-glass-martini" style = "color:rgb(115,115,115);"></i> Alcohol-related&nbsp; ')), status = "info"),
      materialSwitch(inputId = "DRUGFLAG", label = tags$span(HTML('<i class="fa fa-pills" style = "color:rgb(115,115,115);"></i> Drug-related &ensp; &nbsp;')), status = "info"),
      materialSwitch(inputId = "speedflag", label = tags$span(HTML('<i class="fa fa-tachometer" style = "color:rgb(115,115,115);"></i> Speeding &emsp;&ensp; &nbsp;&nbsp; ')), status = "info"),
      materialSwitch(inputId = "lanedepflag", label = tags$span(HTML('<i class="fa fa-road" style = "color:rgb(115,115,115);"></i> Lane Departure')), status = "info"),
      
      tags$h5("Driver age"),
      materialSwitch(inputId = "teenflag", label = tags$span(HTML('<img src="icons8-driver-60.png" style="width:16px;height:16px;"></i> Teen driver &emsp;&nbsp; &nbsp; ')), status = "info"),
      materialSwitch(inputId = "olderflag", label = tags$span(HTML('<img src="icons8-driver-60.png" style="width:16px;height:16px;"></i> Older driver &ensp;&nbsp;&nbsp; &nbsp; ')), status = "info"),
      tags$h5("Who's involved"),
      materialSwitch(inputId = "CYCLFLAG", label = tags$span(HTML('<i class="fa fa-motorcycle" style = "color:rgb(115,115,115);"></i> Motorcycle &emsp;&nbsp;&nbsp;&nbsp;')), status = "info"),
      materialSwitch(inputId = "PEDFLAG", label = tags$span(HTML('<i class="fa fa-walking" style = "color:rgb(115,115,115);"></i> Pedestrian &emsp;&ensp;&nbsp;&nbsp;')), status = "info"),
      materialSwitch(inputId = "BIKEFLAG", label = tags$span(HTML('<i class="fa fa-bicycle" style = "color:rgb(115,115,115);"></i> Bicycle &emsp;&emsp;&emsp;&nbsp;&nbsp;')), status = "info"),
      # tags$h5("Person behavior"),
      # materialSwitch(inputId = "seatbelt", label = tags$span(HTML('<i class="fa fa-car" style = "color:rgb(115,115,115);"></i> Seat belt (NO)')), status = "info"),
      tags$h5("Other"),
      materialSwitch(inputId = "singlevehflag", label = tags$span(HTML('<img src="icons8-traffic-accident-50.png" style="width:16px;height:16px;"></i> Single Vehicle &nbsp;')), status = "info"),
      materialSwitch(inputId = "deerflag", label = tags$span(HTML('<img src="icons8-deer-52.png" style="width:15px;height:15px;"></i> Deer &emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;')), status = "info")
      ), # <img src="icons8-deer-52.png"">
    shinydashboard::menuItem(
      strong("Map Settings/Analysis"),
      tabName = "map_sett",
      icon = icon("map"),
      # startExpanded = TRUE,  # start expanded
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
  )
)
################### BODY #######################

# The body is separated by tabs
body <- dashboardBody(
  ### changing theme
  theme_grey_light,  # the awesome theme
  
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
        tagList(shiny::icon("paper-plane"), strong("Welcome")),
        
        tagList(tags$span(
          HTML(
            "<h3>Welcome to the WisDOT Interactive Crash Statistics Dashboard.</h3>
        <h6 style=font-size:14px;>Explore crash statistics by county from 2017 - 2019. Click on the above tabs
          to display different charts and click of the sidebar tabs to select parameters. Charts can be downloaded as a PNG and are also interactive; single click to remove a variable and double
          click to isolate a variable.</h6>
          
          <h6 style=font-size:14px;> For a data request email BOTSTS at <a href=mailto:CrashDataAnalysis@dot.wi.gov>CrashDataAnalysis@dot.wi.gov</a> </h6>
            <img src=tops-lab-logo.png style=height:100px;display:inline-block;background-color:rgb(248,248,248)>
            <img src=zero-logo.png style=height:100px;display:inline-block;background-color:rgb(248,248,248);>"
          )
        ))
      ),
      tabPanel(
        # NOTE   br(), adds space between charts
        tagList(shiny::icon("car-crash"), strong("Crash trends")),
        crsh_svr_mth_ui("crsh_svr_mth"),
        br(),
        timeofday_heat_ui("timeofday_heat")
      ),
      tabPanel(
        tagList(tags$span(HTML('<img src="icons8-car-crash-50.png" style="width:16px;height:16px;"></i>')), strong("Crash types")),
        mnrcoll_ui("mnrcoll")
      ),
      tabPanel(
        tagList(shiny::icon("users"), strong("People involved")),
        person_role_treemap_ui("person_role_treemap"),
        br(),
        person_age_gender_ui("person_age_gender"),
      ),
      tabPanel(
        tagList(tags$span(HTML('<img src="icons8-driver-60.png" style="width:16px;height:16px;"></i>')), strong("Driver Behavior")),
        drvrpc_chart_ui("drvrpc_chart")
        ),
      tabPanel(
        tagList(shiny::icon("bicycle"), shiny::icon("walking"), strong("Bike and Ped. Behavior")),
        nmtact_chart_ui("nmtact_chart"),
        # br(),
        # nmtloc_chart_ui("nmtloc_chart")
      ),
      tabPanel(
        tagList(shiny::icon("car"), strong("Vehicles involved")),
        vehicle_treemap_ui("vehicle_treemap")
      )
    )
  ),
  column(width = 6,  # Text for 'xx crashes are not mapped'
         leafletOutput("map1", height = "600px"),
         br(),
         box(
           width = NULL,
             tagList(tags$span(
               HTML(paste0("<span style=color:rgb(100,100,100);>", textOutput("get_number_of_NA", inline = TRUE), " crashes are not mapped due to unknown coordinates.</span>")))))
         # leafletOutput("map1", height = "600px") #from 680 # try this instead)
  ))

# Put them together into a dashboardPage
# adding icons on right side of header: https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
dashboardPage(title = "WisDOT Crash Dashboard", # browser tab name
  dashboardHeader(
    title = logo_grey_light,  # text for page title
    titleWidth = 330,
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 50px}"),
            tags$style(".main-header .logo {height: 50px}")
    )
    
    # tags$li(class = "dropdown", style = "display:inline-block; horizontal-align: left", tags$h4(
    #   HTML(
    #     '<i class="fa fa-car-alt" style = "color:rgb(115,115,115);"></i><span style=color:white;> Crashes</span>',
    #     paste(
    #       "<b style=color:white;>",
    #       textOutput("crash_count", inline = TRUE),
    #       "</b>"))))
  ),
  sidebar, body
  )