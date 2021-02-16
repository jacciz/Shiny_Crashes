#' Charts module for ui-side processings
#'
#' This module produces all charts with the df based on variables selected by the user.
#' 
#' @param id standard \code{shiny} boilerplate
#' @param df data frame (non-reactive) with variables necessary for charts
#' 
# Put this in the actual app to reference these modules
# UI: crsh_svr_mth_ui("crsh_svr_mth")
# Server: crsh_svr_mth_server("crsh_svr_mth", filtered_crashes())

################### Crash Severity by Month #######################
crsh_svr_mth_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "crsh_svr_mth"), height = "240px"))
}

################### Persons Injured by Year Bar Chart #######################
wisinj_by_year_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "wisinj_by_year"), height = "240px"))
}

################### Time of Day Heatmap #######################
timeofday_heat_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "timeofday_heat"), height = "240px"))
}

################### Manner of Collision Bar Chart #######################
mnrcoll_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "mnrcoll"), height = "340px"))
}

################### Person by Role Tree Map #######################
person_role_treemap_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "person_role_treemap"), height = "250px"))
}

################### Age and Gender Bar Chart #######################
person_age_gender_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "person_age_gender"), height = "240px"))
}

################### DRVRPC Bar Chart #######################
drvrpc_chart_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "drvrpc_chart"), height = "340px"))
}
################### Bike/Ped Top Actions Bar Chart #######################
nmtact_chart_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "nmtact_chart"), height = "340px"))
}

################### Bike/Ped Location Bar Chart #######################
nmtloc_chart_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "nmtloc_chart", height = "340px")))
}
################### Vehicle Types Tree Map #######################
vehicle_treemap_ui <- function(id) {
  tagList(plotlyOutput(NS(id, "vehicle_treemap")))
}