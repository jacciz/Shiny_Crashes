## ui.R ##
#                                                   SIDEBAR
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
    badgeLabel = "new",
    badgeColor = "green"
  ),    pickerInput(
    "cntynum",
    "County",
    choices = sort(unique(all_crashes$CNTYCODE)),
    options = list("actions-box" = TRUE),
    multiple = FALSE,
    selected = "13"
  )
))
#                                                     BODY DASHBOARD
body <- dashboardBody(tabItems(
  tabItem(tabName = "dashboard",
          h2("Dashboard tab content"),
          fluidRow(
            box(
              title = "Bike Crashes",
              width = 4,
              solidHeader = TRUE,
              plotOutput("bikeflag")
            ),
            box(
              title = "Pedestrian Crashes",
              width = 4,
              solidHeader = TRUE,
              plotOutput("pedflag")
            ),
            box(
              title = "Alcohol Flag Crashes",
              width = 4,
              solidHeader = TRUE,
              plotOutput("alcflag")
            )
          )
          #                                                             BODY second row
          ),
  #                                               BODY TABLES
  tabItem(tabName = "tables",
          h2("Tables tab content"),
          fluidRow(box(
            width = 6, DTOutput("biketable", height = 600)
          ))
          )
))





# Put them together into a dashboardPage
dashboardPage(dashboardHeader(title = "Crash Data Dashboard"),
              sidebar,
              body)