# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "siderbar_material_switch" ) # Name of the module
golem::add_module( name = "waffle_chart" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "sql_queries" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("WisDOTcrashdashboard")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

# Shiny extensions
# https://awesomeopensource.com/project/nanxstats/awesome-shiny-extensions

# TESTING / DEBUGGING

# Run in viewer: rstudioapi::viewer("http://127.0.0.1:7583")
# shinytest::recordTest("C:/W_shortcut/Shiny_Crashes_Dashboard/") test for bugs
# https://rstudio.github.io/shinyloadtest/ # month.abb[month]

# For debugging, try shinyjs::runcodeUI() and shinyjs::runcodeServer()
# Also shiny::reactlog() for issues with reactivity
# options(shiny.reactlog = TRUE) then ctrl-f3 in browser



# library(shinyFeedback) # https://github.com/merlinoa/shinyFeedback and https://mastering-shiny.org/action-feedback.html, good for a single input, validate() is for output

# filter(.data[[input$var]] > .env$input$min)) or diamonds %>% filter(.data[[var]] > .env$min)

# create dynamic and multiple dropwdowns: https://mastering-shiny.org/action-dynamic.html
# ctrl shift enter
# jobs in C:\Users\dotjaz\Documents\RStudio\background-jobs view: rstudioapi::viewer("http://127.0.0.1:5101")
# traceback() to find error location, add browser() in code to launch debugger
# breakpoints - debugger tool - press n,c,q
# use message() glue::glue() str()
# selectVarServer("var", reactive(input$x)))
# ctrl .  to find function
# Testing: https://mastering-shiny.org/scaling-testing.html
# more testing https://speakerdeck.com/colinfay/workshop-building-successful-shiny-apps-with-golem?slide=130
# Performance: https://mastering-shiny.org/performance.html

# CSS AND ICONS
# https://www.w3schools.com/css/ a resource for CSS
# https://codepen.io/sosuke/pen/Pjoqqp to get FILTER colors for SVG (like hexbin) - plug in color and scroll down
# https://icons8.com/ free icons - recolor (#8F9BB3) before download at 60pxPNG


############ ADD MODULE ###########
# Put this in the actual app to reference these modules
# UI: crsh_svr_mth_ui("crsh_svr_mth")
# Server: crsh_svr_mth_server("crsh_svr_mth", filtered_crashes())

# To add a new chart - use this
# crsh_svr_mth_server <- function(id, crash_df()) {
#   moduleServer(id, function(input, output, session) {
#     plotly stuff goes here
#   })
#   }

# Example use
# select_county_input("cntycode_input") # UI
# county_input <- select_county_server("cntycode_input") # Server