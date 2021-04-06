#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import data.table leaflet shiny
#' @importFrom shinyWidgets switchInput
#' @importFrom dplyr  all_of all_vars any_vars filter_at arrange select semi_join  summarise vars
#' @importFrom shinydashboard renderValueBox
#' @noRd
app_server <- function( input, output, session ) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
  # read shapefile for county.
  g = app_sys("inst/app/www/county.shp")
  county_geom <- sf::st_read(g)
  # List the first level callModules here
  ################### SIDEBAR Data Inputs #######################
  # Return inputs to filter data based on user selection. When using these variables, include ()
  # since these are reactive expressions
  year_input <- mod_siderbar_select_year_server("year")
  county_input <- mod_siderbar_select_county_server("cntycode_input")
  # municode_input <- mod_siderbar_select_muni_server("municode_input", county_input)
  
  # returns list for which crsh svr are selected. List must match Input IDs with field names of data
  crshsvr_selected_inputs <-c("Fatal", "Injury", "Property Damage")
  # Looks at status of crshsvr buttoms and returns only the ones that are selected (i.e. == TRUE)
  crshsvr_selected <- reactive({
    data <- sapply(crshsvr_selected_inputs, function(x) input[[x]] ) # fields contains all values we want to save, gather all the values based on input
    data <- Filter(function(x) !(all(x == FALSE)), data) # take out FALSE values
    names(data)
  })
  
  # List of all available crash flags #distracted_flag
  crshflag_selected_inputs <-
    c("ALCFLAG", "DRUGFLAG", "speedflag", "distracted_flag", "teenflag", "olderflag", "CYCLFLAG",
      "PEDFLAG", "BIKEFLAG", "singlevehflag", "lanedepflag","deerflag","intersection_flag")
  
  # Looks at status of crshsvr buttons and returns only the ones that are selected (i.e. == TRUE).
  get_crshflag_list <- reactive({
    data <- sapply(crshflag_selected_inputs, function(x) input[[x]] ) # fields contains all values we want to save, gather all the values based on input
    data <- Filter(function(x) !(all(x == FALSE)), data) # take out FALSE values
    names(data)
  })
  
  # this decides whether to return all OR any crash flags, returns only CRSHNMBR
  filtered_crsh_flags <-
    reactive({
      crshflag_list = get_crshflag_list()
      if (input$any_or_all) { # default for this button is 'any'
        # selects crash flags, filter each crshflag and finds any_vars == Y
        return(
          mod_filter_data_server("crsh_flags", "crsh_flags", county = county_input, crsh_svr = crshsvr_selected, year_input) %>% #module
            select(.data$CRSHNMBR, all_of(crshflag_list)) %>%
            filter_at(vars(all_of(crshflag_list)), any_vars(. == "Y")) %>%
            select(.data$CRSHNMBR)
        )
      }
      # Same, but find all_vars == Y
      return(
        mod_filter_data_server("crsh_flags", "crsh_flags", county = county_input, crsh_svr = crshsvr_selected, year_input) %>% #module
          select(.data$CRSHNMBR, all_of(crshflag_list)) %>%
          filter_at(vars(all_of(crshflag_list)), all_vars(. == "Y")) %>%
          select(.data$CRSHNMBR)
      )
    })
  
  # Takes the selected county, finds bbox so we can zoom to it
  selected_county <- reactive({
    sel_county <- county_geom %>%
      filter(.data$DNR_CNTY_C %in% county_input()) #COUNTY_NAM
    bbox <- sf::st_bbox(sel_county) %>%
      as.vector()
    bbox
  })
  # 
  ################### DATA OBSERVE EVENTS OF DATA #######################
  
  # returns FINAL crash data for charts, if a flag was selected it is joined with crsh_flag list via crshnmbr
  filtered_crashes <-
    reactive({
      if (length(get_crshflag_list()) == 0) {
        # if no flags selected, get data
        return (mod_filter_data_server("crash", "crash", county = county_input, crsh_svr = crshsvr_selected, year_input)) # module
      } else {
        # if at least 1 flag was selected, returns the join with filtered_crashes
        return(semi_join(
          mod_filter_data_server("crash", "crash", county = county_input, crsh_svr = crshsvr_selected, year_input), # module
          filtered_crsh_flags(),
          by = c("CRSHNMBR" = "CRSHNMBR")
        ))
      }
    })
  # returns FINAL person data, if a flag was selected it is joined with crsh_flag list
  filtered_persons <-
    reactive({
      if (length(get_crshflag_list()) == 0) {
        # if no flags selected, get data
        return (mod_filter_data_server("person", "person", county = county_input, crsh_svr = crshsvr_selected, year_input)) #module
      } else {
        # if at least 1 flag was selected
        # returns the join with filtered_crashes
        return(semi_join(
          mod_filter_data_server("person", "person", county = county_input, crsh_svr = crshsvr_selected, year_input), # module
          filtered_crsh_flags(),
          by = c("CRSHNMBR" = "CRSHNMBR")
        ))
      }
    })

  # read vehicles and join with filtered crashes that may have flags
  filtered_vehicles <-
    reactive({
      veh = mod_filter_data_server("vehicle", "vehicle", county = county_input, crsh_svr = crshsvr_selected, year_input)
      dplyr::inner_join(veh, filtered_crashes(), by = "CRSHNMBR") # inner join keeps crashes that match by CRSHNMBR
    })
  
  ############# IMPORT DATA FROM SQLITE #######################
  # Grabs the lat, longs, and crsh_svr for mapping
  filtered_crash_lat_long <- reactive({
    crash_lat_long_j <-
      filtered_crashes() %>% select(lng, lat, CRSHSVR) %>% # [, .(lng, lat, CRSHSVR)] %>%
      dplyr::filter(!is.na(lng), !is.na(lat)) %>%
      arrange(factor(CRSHSVR, levels  = crshsvr_factor_levels))# remove crashes with no lat/long
    if (dim(crash_lat_long_j)[1] != 0) {
      # convert to sf so we can map it!
        crash_lat_long_j <- sf::st_as_sf(
          x = crash_lat_long_j,
          coords = c("lng", "lat"),
          crs = 4326
        )
      return(sf::st_transform(crash_lat_long_j, "+proj=longlat +datum=WGS84"))
    } else { # Create fake df when nothing to map
      sf_obj = data.table::data.table(data.frame(
        lng = c(0, 0),
        lat = c(0, 0),
        CRSHSVR = c("Fatal", "Fatal")
      ))
      sf_obj <- sf::st_as_sf(x = sf_obj,
                         coords = c("lng", "lat"),
                         crs = 4326,
                         na.fail = FALSE)
      return(sf_obj)
    }
  })

  # Get number of crashes with no coordinates
  output$get_number_of_NA <- renderText({
    toString(format(sum(is.na(filtered_crashes()$lng)), big.mark = ","))
  })
  
 ######### Get Ped/Bike Count ##########
  # add factor_levels so 0 values will be kept (need .drop = FALSE)
  bike_ped_count <- reactive({
    filtered_persons() %>%
      dplyr::filter(.data$ROLE %in% c("Bicyclist", "Pedestrian"), .data$WISINJ != "No Apparent Injury") %>%
      mutate(inj = ifelse(.data$WISINJ == "Fatal Injury", "Killed","Injured"),
             inj = factor(.data$inj, levels =c("Injured","Killed")),
             ROLE = factor(.data$ROLE, levels =c("Bicyclist","Pedestrian"))) %>% 
      dplyr::count(.data$ROLE, .data$inj, .drop = FALSE) %>% mutate(for_colors = paste0(.data$ROLE, .data$inj)) %>%
      data.table::as.data.table()
  })

  ################### VALUE BOXES #######################
  output$tot_crash <- renderInfoBox({
    valueBox(tags$span(HTML(
      paste0(
        '<p style="font-size:1.5vmin; color:rgb(15,15,15)">',
        format(nrow(filtered_crashes()), big.mark = ","),
        '</p>')
    )),
    tags$li(
      HTML(
        '<i class="fa fa-car-crash" style = "color:grey;font-size: 2vmin;"></i><p style="font-size:1vmin;display:inline-block;padding-right:20px;color:rgb(15,15,15);">&ensp;Crashes</p>'
      )
    ),
    color = "red")
  })
  
  output$tot_inj <- renderInfoBox({
    valueBox(tags$span(HTML(
      paste0(
        '<p style="font-size:1.5vmin; color:rgb(15,15,15)">',
        filtered_crashes() %>% summarise(x = format(sum(.data$TOTINJ), big.mark = ",")),
        '</p>')
    )),
    tags$li(
      HTML(
        '<i class="fa fa-first-aid" style = "color:#428BCA;font-size: 2vmin;"></i><p style="font-size: 1vmin;color:rgb(15,15,15);text-align:center;display:inline-block;padding-right:20px;">&ensp;People injured</p>'
      )),
    color = "red")
  })
  output$tot_fatal <- renderInfoBox({
    valueBox(tags$span(HTML(
      paste0(
        '<p style="font-size:1.5vmin; color:rgb(15,15,15)">',
        filtered_crashes() %>% summarise(x = format(sum(.data$TOTFATL), big.mark = ",")),
        '</p>')
    )),
    tags$li(
      HTML(
        '<i class="fa fa-heartbeat" style = "color:#D50032;font-size: 2vmin;"></i><p style="font-size:1vmin;color:rgb(15,15,15);display:inline-block;padding-right:20px;">&ensp;People killed</p>'
      )),
    color = "red")
  })
  
  ################### BODY - CHARTS MODULES #######################
  #  Charts are in Shiny Modules in chart_modules_ui and in chart_modules_server
  mod_chart_crsh_svr_mth_server("crsh_svr_mth", filtered_crashes)
  # mod_chart_wisinj_by_year_server("wisinj_by_year", filtered_persons)
  mod_chart_timeofday_heat_server("timeofday_heat", filtered_crashes)
  mod_chart_mnrcoll_server("mnrcoll", filtered_crashes)
  mod_chart_person_role_treemap_server("person_role_treemap", filtered_persons)
  mod_chart_person_age_gender_server("person_age_gender", filtered_persons)
  mod_chart_drvrpc_server("drvrpc_chart", filtered_persons)
  mod_chart_nmtact_server("nmtact_chart", filtered_persons)
  mod_chart_nmtloc_server("nmtloc_chart", filtered_persons)
  mod_chart_vehicle_treemap_server("vehicle_treemap", filtered_vehicles)
  mod_waffle_chart_server("bike", bike_ped_count)
  
  ################### BODY - MAP #######################
  
  # odd issue with asynchronous data loading, could use renderUI so map gets updated based on user inputs
  # https://github.com/rstudio/leaflet/issues/151  https://github.com/rstudio/leaflet/issues/448
  # Leaflet Options: http://leaflet-extras.github.io/leaflet-providers/preview/
  
  # render basic map (CRS is 4326), i.e. items that do not need a reactive
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager, options = providerTileOptions(opacity = .5)) %>%
      # addTiles(options = providerTileOptions(opacity = .5)) %>%
      addPolygons(
        data = county_geom$geometry,
        group = "Counties",
        color = "#444444",
        fillOpacity = 0,
        weight = 1,
        smoothFactor = 0.5
        # options = pathOptions(clickable = FALSE)
      )
  })
  
  # change view based on county(ies) selected
  observeEvent(county_input(), {
    county_zoom <- selected_county()
    leafletProxy("map1") %>%
      fitBounds(county_zoom[1], county_zoom[2], county_zoom[3], county_zoom[4])  # zoom to selected county
  })
  
  # if (dim(filtered_crashes())[1] != 0) {
  observeEvent(filtered_crashes(), {  # same view, updates map data if selection crashes changes
    # if/else determines what to render (crash points or hex)
    if (input$hex == FALSE) { # when HEX is OFF
      #if/then to map if there's crashes
      # Clear map so we can add new stuff
      leafletProxy("map1") %>%
        # removeGlPoints(layerId = "Crashes") %>%
        leafgl::clearGlLayers() %>%
        leafgl::addGlPoints(  # when add points, ERROR: Uncaught TypeError: Cannot read property 'getSize' of null at s._redraw (VM123 glify-browser.js:48) MAY HAVE TO DO WITH BOUNDS??
          data = filtered_crash_lat_long(),
          fillColor = ~color_map_svr[CRSHSVR],
          fillOpacity = 1,
          radius = 5,
          layerId = "Crashes",
          group = "Crashes")
    } else {  # when HEX is ON
      leafletProxy("map1", data = filtered_crash_lat_long()) %>%
        # removeGlPoints(layerId = "Crashes") %>% # remove crashes
        leafgl::clearGlLayers() %>%
        # hideGroup("Crashes") %>% #uncheck crashes
        leaflet.extras2::clearHexbin() %>%
        leaflet.extras2::addHexbin(
          radius = input$hexsize,
          opacity = 1,
          options = leaflet.extras2::hexbinOptions(
            colorRange = c("#fee0d2", "#de2d26"), #c("#fee0d2", "#de2d26"), # red #c("#b0d0f2", "#05366b"), #blue    c("#99d899", "#005100") green
            resizetoCount = TRUE,
            radiusRange = c(input$hexsize, input$hexsize), # same size, must match radius
            tooltip = "Crashes: ")
        )
    }
  })
  
  observe({ # observe when hexsize changes or if hex is checked
    if (input$hex & input$hexsize) {
      leafletProxy("map1", data = filtered_crash_lat_long()) %>%
        # removeGlPoints(layerId = "Crashes") %>% # remove crashes
        leafgl::clearGlLayers() %>%
        # hideGroup("Crashes") %>% #uncheck crashes
        leaflet.extras2::clearHexbin() %>%
        leaflet.extras2::addHexbin(
          radius = input$hexsize,
          opacity = 1,
          options = leaflet.extras2::hexbinOptions(
            colorRange = c("#fee0d2", "#de2d26"),#c("#fee0d2", "#de2d26"), # red #c("#b0d0f2", "#05366b"), #blue    c("#99d899", "#005100") green
            resizetoCount = TRUE,
            radiusRange = c(input$hexsize, input$hexsize), # same size, must match radius
            tooltip = "Crashes: "
          )
        ) 
    }
    else { # remove hex if unchecked
      leafletProxy("map1") %>%
        leaflet.extras2::clearHexbin() %>%
        # removeGlPoints(layerId = "Crashes") %>% # remove crashes
        # showGroup("Crashes") %>% # check crashes
        leafgl::addGlPoints( # make sure this is same as above
          data = filtered_crash_lat_long(),
          fillColor = ~color_map_svr[CRSHSVR],
          fillOpacity = 1,
          radius = 5,
          layerId = "Crashes",
          group = "Crashes")
    }
  })

}
