# https://convertio.co/png-svg/  ### convert to svg

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @importFrom shinyWidgets materialSwitch
#' @noRd
#' 
#' 
# layout is Bootstrap (i,e, row widths must add up to 12), helpful to know a little CSS, HTML
# source("www/theme_grey_dark.R")  # adds a cool theme

# source("theme_grey_light.R")  # adds a cool theme

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    # adding icons on right side of header: https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
    dashboardPage(
      title = "WisDOT Crash Dashboard",
      # browser tab name
      dashboardHeader(
        title = logo_grey_light,
        # text for page title
        titleWidth = 330,
        # Set height of dashboardHeader
        tags$li(
          class = "dropdown",
          tags$style(".main-header {max-height: 50px}"),
          tags$style(".main-header .logo {height: 50px}")
        ),
        #horizontal-align: left;
        tags$li(class = "dropdown", style = "", tags$div(
          HTML(
            '<span><a href=mailto:CrashDataAnalysis@dot.wi.gov>Questions about this page: CrashDataAnalysis@dot.wi.gov</a></span>'
          )
        ))
      ),
      dashboardSidebar(
        # .fa-car-band-aid {vertical-align: middle;}
        # Adjust the sidebar
        # tags$style(".left-side, .main-sidebar {padding-top: 20px}"),
        width = "250px",    # sidebar width
        
        sidebarMenu(
          # Remove the sidebar toggle element
          tags$script(
            DT::JS(
              "document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"
            )
          ),
          # div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
          menuItem(
            strong(" Year"),
            tabName = "year",
            icon = icon("calendar"),
            mod_siderbar_select_year_ui("year") # Module
            # textOutput("out")
          ),
          menuItem(
            strong("Select Location"),
            # strong makes it bold
            tabName = "location",
            icon = icon("map-marked-alt"),
            startExpanded = TRUE,
            # start expanded
            selected = as.character("Adams"),
            mod_siderbar_select_county_ui("cntycode_input") # Module
            # mod_siderbar_select_muni_ui("municode_input") # Module
          ),
          menuItem(
            strong(" Crash Severity"),
            tabName = "crash_type",
            icon = icon("car-crash"),
            # startExpanded = TRUE, # start expanded
            # materialSwitch(inputId = "fatal", label = span("Fatal", style = "color:#D50032"), status = "danger", value = TRUE),
            # set to each have 15 characters to fake left align: 1 space - &nbsp;  2 - &ensp; 4 - &emsp;
            materialSwitch(
              inputId = "Fatal",
              label = tags$span(
                HTML(
                  '<i class="fa fa-heartbeat" style = "color:#D50032;"></i><p style="display:inline-block;">&ensp;Fatal&emsp;&emsp;&ensp;</p>'
                )
              ),
              status = "danger",
              value = TRUE
            ),
            materialSwitch(
              inputId = "Injury",
              label = tags$span(
                HTML(
                  '<i class="fa fa-first-aid" style = "color:#428BCA;"></i><p style="display:inline-block;">&ensp;Injury&emsp;&emsp;&nbsp;</p>'
                )
              ),
              status = "primary",
              value = TRUE
            ),
            materialSwitch(
              inputId = "Property Damage",
              label = tags$span(
                HTML(
                  '<i class="fa fa-car" style = "color:#4DB848;"></i><p style="display:inline-block;">&ensp;Prop. Damage</p>'
                )
              ),
              status = "success",
              value = TRUE
            ),
            tags$h5("")
          ),
          menuItem(
            strong(" Flags"),
            tabName = "crash_flags",
            icon = icon("flag"),
            startExpanded = FALSE,
            # start expanded
            # <br> add a break line
            tags$h5("Selection type:", style = "display:inline-block; margin-top: 10px;"),
            tags$h5(
              switchInput(
                inputId = "any_or_all",
                value = TRUE,
                onLabel = "Any",
                offLabel = "All",
                onStatus = "primary",
                offStatus = "primary",
                size = "mini",
                inline = TRUE
              ),
              style = "display:inline-block; margin-top: 10px; padding: 0px;"
            ),
            tags$h5(" Driver behavior", style = "margin-top: 10px;"),
            # set to each have 15 characters to fake left align: 1 space - &nbsp;  2 - &ensp; 4 - &emsp;
            materialSwitch(
              inputId = "ALCFLAG",
              label = tags$span(
                HTML(
                  '<i class="fa fa-glass-martini" style = "color:rgb(115,115,115);"></i> Alcohol-related&nbsp; '
                )
              ),
              status = "primary"
            ),
            materialSwitch(
              inputId = "DRUGFLAG",
              label = tags$span(
                HTML(
                  '<i class="fa fa-pills" style = "color:rgb(115,115,115);"></i> Drug-related &ensp; &nbsp;'
                )
              ),
              status = "primary"
            ),
            materialSwitch(
              inputId = "speedflag",
              label = tags$span(
                HTML(
                  '<i class="fa fa-tachometer" style = "color:rgb(115,115,115);"></i> Speeding &emsp;&ensp; &nbsp;&nbsp; '
                )
              ),
              status = "primary"
            ),
            materialSwitch(
              inputId = "lanedepflag",
              label = tags$span(
                HTML(
                  '<i class="fa fa-road" style = "color:rgb(115,115,115);"></i> Lane Departure'
                )
              ),
              status = "primary"
            ),
            
            tags$h5("Driver age"),
            materialSwitch(
              inputId = "teenflag",
              label = tags$span(
                HTML(
                  '<img src="www/icons8-driver-60.svg" style="width:16px;height:16px;"></i> Teen driver &emsp;&nbsp; &nbsp; '
                )
              ),
              status = "primary"
            ),
            materialSwitch(
              inputId = "olderflag",
              label = tags$span(
                HTML(
                  '<img src="www/icons8-driver-60.svg" style="width:16px;height:16px;"></i> Older driver &ensp;&nbsp;&nbsp; &nbsp; '
                )
              ),
              status = "primary"
            ),
            tags$h5("Who's involved"),
            materialSwitch(
              inputId = "CYCLFLAG",
              label = tags$span(
                HTML(
                  '<i class="fa fa-motorcycle" style = "color:rgb(115,115,115);"></i> Motorcycle &emsp;&nbsp;&nbsp;&nbsp;'
                )
              ),
              status = "primary"
            ),
            materialSwitch(
              inputId = "PEDFLAG",
              label = tags$span(
                HTML(
                  '<i class="fa fa-walking" style = "color:rgb(115,115,115);"></i> Pedestrian &emsp;&ensp;&nbsp;&nbsp;'
                )
              ),
              status = "primary"
            ),
            materialSwitch(
              inputId = "BIKEFLAG",
              label = tags$span(
                HTML(
                  '<i class="fa fa-bicycle" style = "color:rgb(115,115,115);"></i> Bicycle &emsp;&emsp;&emsp;&nbsp;&nbsp;'
                )
              ),
              status = "primary"
            ),
            # tags$h5("Person behavior"),
            # materialSwitch(inputId = "seatbelt", label = tags$span(HTML('<i class="fa fa-car" style = "color:rgb(115,115,115);"></i> Seat belt (NO)')), status = "primary"),
            tags$h5("Other"),
            materialSwitch(
              inputId = "singlevehflag",
              label = tags$span(
                HTML(
                  '<img src="www/icons8-traffic-accident-50.svg" style="width:16px;height:16px;"></i> Single Vehicle &nbsp;'
                )
              ),
              status = "primary"
            ),
            materialSwitch(
              inputId = "deerflag",
              label = tags$span(
                HTML(
                  '<img src="www/icons8-deer-52.svg" style="width:15px;height:15px;"></i> Deer &emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;'
                )
              ),
              status = "primary"
            )
          ),
          # <img src="www/icons8-deer-52.svg"">
          menuItem(
            strong(" Map Settings"),
            tabName = "map_sett",
            icon = icon("map"),
            # startExpanded = TRUE,  # start expanded
            materialSwitch(
              "hex",
              label = tags$span(
                HTML(
                  '</svg><image class="hexbin_svg" src="www/hexbin.svg"/> Hex Bins'
                )
              ),
              status = "primary",
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
      ),
      dashboardBody(
        # the awesome theme
        theme_grey_light,
        column(
          # # for column, width = NULL
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
            <img src=www/tops-lab-logo.png style=height:100px;display:inline-block;background-color:rgb(248,248,248)>
            <img src=www/zero-logo.png style=height:100px;display:inline-block;background-color:rgb(248,248,248);>"
                )
              ))
            ),
            tabPanel(
              # NOTE   br(), adds space between charts
              tagList(shiny::icon("car-crash"), strong("Crash Trends")),
              mod_chart_crsh_svr_mth_ui("crsh_svr_mth"),
              br(),
              mod_chart_timeofday_heat_ui("timeofday_heat"),
              br(),
              mod_chart_wisinj_by_year_ui("wisinj_by_year")
            ),
            tabPanel(tagList(
              tags$span(
                HTML(
                  '<img src="www/icons8-car-crash-50.svg" style="width:16px;height:16px;"></i>'
                )
              ), strong("Crash Types")
            ),
            mod_chart_mnrcoll_ui("mnrcoll")),
            tabPanel(
              tagList(shiny::icon("users"), strong("People Involved")),
              mod_chart_person_role_treemap_ui("person_role_treemap"),
              br(),
              mod_chart_person_age_gender_ui("person_age_gender"),
            ),
            tabPanel(
              tagList(tags$span(
                HTML(
                  '<img src="www/icons8-driver-60.svg" style="width:16px;height:16px;"></i>'
                )
              ), strong("Driver Behavior")),
              mod_chart_drvrpc_ui("drvrpc_chart")
            ),
            tabPanel(
              tagList(
                shiny::icon("bicycle"),
                shiny::icon("walking"),
                strong("Bike and Ped. Behavior")
              ),
              mod_chart_nmtact_ui("nmtact_chart"),
              # br(),
              # nmtloc_chart_ui("nmtloc_chart")
            ),
            tabPanel(
              tagList(shiny::icon("car"), strong("Vehicles Involved")),
              mod_chart_vehicle_treemap_ui("vehicle_treemap")
            )
          )
        ),
        column(
          width = 6,
          # Text for 'xx crashes are not mapped'
          leafgl::leafglOutput("map1", height = "600px"),
          br(),
          box(width = NULL,
              tagList(tags$span(HTML(
                paste0(
                  "<span style=color:rgb(100,100,100);>",
                  textOutput("get_number_of_NA", inline = TRUE),
                  " crashes are not mapped due to unknown coordinates.</span>"
                )
              ))))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  # tags$img(src = "www/icons8-driver-60.svg")
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'WisDOTcrashdashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

