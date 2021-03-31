################### Text Parameters for Charts #######################

# Text parameters for charts
chart_title = list(size = 16, color = "rgb(100,100,100)", family = "Verdana")
chart_axis = list(size = 14, color = "rgb(100,100,100)", family = "Verdana")
# chart_axis_bar = list(size = 14, color = "#38839C", family = "Verdana", face = "bold")
# New color - Text parameters for charts. Also replaced #428BCA with #4fb9db
chart_axis_bar = list(size = 14, color = "#4fb9db", family = "Verdana", face = "bold")

################### Colors and Factors #######################
# assigning colors for crash severity and gender for charts/map
# color_map_svr <- c("Fatal"="#D50032", "Injury"="#38839C", "Property Damage"="#4DB848")
# color_map_gender <- c("Female"="#D50032", "Male"="#38839C", "Unknown" = "#F9C218")
# color_map_wisinj <- c("Suspected Minor Injury" ="#4DB848", "Possible Injury" = "#38839C","Suspected Serious Injury" ="#7f42ca", "Fatal Injury" = "#D50032")

# New colors
color_map_svr <- c("Fatal"="#DB7E65", "Injury"="#4AAECF", "Property Damage"="#44DBAE")
color_map_gender <- c("Female"="#Db7e65", "Male"="#4fb9db", "Unknown" = "#dbb039")
color_map_wisinj <- c("Suspected Minor Injury" ="#4AAECF", "Possible Injury" = "#58CEF5","Suspected Serious Injury" ="#3D8DA8", "Fatal Injury" = "#265869")
color_map_waffle_inj <- c("BicyclistInjured" = "#3D8DA8", "BicyclistKilled" = "#Db7e65", "PedestrianInjured" = "#4AAECF", "PedestrianKilled" = "#Db7e65")
# Factor levels
wisinj_factor_levels <- c("Possible Injury", "Suspected Minor Injury", "Suspected Serious Injury", "Fatal Injury")

age_group_factor_levels <-c("0-4",
"5-9",
"10-14",
"15-19",
"20-24",
"25-29",
"30-34",
"35-39",
"40-44",
"45-49",
"50-54",
"55-59",
"60-64",
"65-69",
"70+")
# assigning colors for crash severity for map
# color_map_svr <- c("Fatal"="#D50032", "Injury"="#428BCA", "Property Damage"="#4DB848")
# New colors
crshsvr_factor_levels <- c("Property Damage", "Injury", "Fatal") # So Fatals will be on top in the map
# wisinj_factor_levels <- c("Possible Injury", "Suspected Minor Injury", "Suspected Serious Injury", "Fatal Injury")

