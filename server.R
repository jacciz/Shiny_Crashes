library(dplyr) # select, filter functions
library(tidyr) # pivot_longer
# library(stringr) # str_wrap
# library(DT)    # create pretty tables
library(plotly) # interactive charts
library(lubridate) # for dates
library(leaflet) # the map
library(leaflet.extras2) # hexbin
library(data.table) # setnames function, data format for large data
library(tibble) # quick data frames
library(leafgl) # add points, much faster than leaflet's CircleMarkers

# assigning colors for crash severity and gender for charts/map
color_map_svr <- c("Fatal"="#D50032", "Injury"="#428BCA", "Property Damage"="#4DB848")

# shinytest::recordTest("C:/W_shortcut/Shiny_Crashes_Dashboard/") test for bugs

# https://rstudio.github.io/shinyloadtest/ # month.abb[month]

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
  ################### SIDEBAR OBSERVE EVENTS #######################
  updateSelectInput(session, # choose county
                    "cntynum",
                    choices = setNames(county_recode$CountyCode, county_recode$CountyName))

  observeEvent(input$cntynum, { # choose municipality

    muni_cnty_list <- muni_recode %>% filter(CntyCode %in% input$cntynum)

  updateSelectInput(session, "muni_names",
                      choices = setNames(muni_cnty_list$MuniCode, muni_cnty_list$Municipality_CTV))
  })

  min_date_selected <- reactive({ 
    # used to find date range
    if (length(input$year) > 1) {
      return (min(input$year))
    } else {
      return (input$year) # ERROR Warning: All formats failed to parse. No formats found.
    }
  })
  max_date_selected <- reactive({
    if (length(input$year) > 1) {
      return (max(input$year))
    } else {
      return (input$year)
    }
  })
  crshsvr_selected <- reactive({ # returns list of crsh svr selected
    crsh_list = list()
    if (input$fatal) {
      crsh_list <- c(crsh_list,"Fatal")
    }
    if (input$injury) {
      crsh_list <- c(crsh_list,"Injury")
    }
    if (input$propertydamage) {
      crsh_list <- c(crsh_list,"Property Damage")
    }
    return (crsh_list)
})
  
  get_crshflag_list <- reactive({
    # returns list of crshflags selected
    crshflag_list = as.character()
    if (input$alc) {
      crshflag_list <- c(crshflag_list, "ALCFLAG")
    }
    if (input$drug) {
      crshflag_list <- c(crshflag_list, "DRUGFLAG")
    }
    if (input$speed) {
      crshflag_list <- c(crshflag_list, "speedflag")
    }
    # if (input$distract) {
    #   crshflag_list <- c(crshflag_list,"distract_flag")
    # }
    if (input$teen) {
      crshflag_list <- c(crshflag_list, "teenflag")
    }
    if (input$older) {
      crshflag_list <- c(crshflag_list, "olderflag")
    }
    if (input$motorcycle) {
      crshflag_list <- c(crshflag_list, "CYCLFLAG")
    }
    if (input$ped) {
      crshflag_list <- c(crshflag_list, "PEDFLAG")
    }
    if (input$bike) {
      crshflag_list <- c(crshflag_list, "BIKEFLAG")
    }
    # if (input$seatbelt) {
    #   crshflag_list <- c(crshflag_list, "seatbeltflag")
    # }
    if (input$singleveh) {
      crshflag_list <- c(crshflag_list, "singlevehflag")
    }
    if (input$lanedep) {
      crshflag_list <- c(crshflag_list, "lanedepflag")
    }
    if (input$deer) {
      crshflag_list <- c(crshflag_list, "deerflag")
    }
    return (crshflag_list)
  })
  
  filtered_crsh_flags <-
    # this decides whether to return all or any crash flags, returns only CRSHNMBR
    reactive({
      crshflag_list = get_crshflag_list() # the if/else determines any or all selection
      if (input$any_or_all) { # default for this button is 'any'
        # selects crash flags, filter each crshflag and finds any_vars == Y
        return(
          all_crsh_flags %>% select(CRSHNMBR, all_of(crshflag_list)) %>% filter_at(vars(all_of(crshflag_list)), any_vars(. == "Y")) %>% dplyr::select(CRSHNMBR)
        )
      }
      # Same, but find all_vars == Y
      return(
        all_crsh_flags %>% select(CRSHNMBR, all_of(crshflag_list)) %>% filter_at(vars(all_of(crshflag_list)), all_vars(. == "Y")) %>% dplyr::select(CRSHNMBR)
      )
    })
  
# portage should be 49
  selected_county <- reactive({ # this takes the selected county and zooms to it
    sel_county <- county %>% filter(DNR_CNTY_C %in% input$cntynum) #COUNTY_NAM
    bbox <- st_bbox(sel_county) %>% as.vector()
    bbox
  })
  ################### DATA OBSERVE EVENTS OF DATA #######################

  filtered_crashes <-
    # returns crash data, depends if a flag was selected
    reactive({
      if (length(get_crshflag_list()) == 0) {
        # if no flags selected
        return (filtered_crashes_no_flags())
      } else {
        # if at least 1 flag was selected
        # returns the join with filtered_crashes
        return(semi_join(
          filtered_crashes_no_flags(),
          filtered_crsh_flags(),
          by = c("CRSHNMBR" = "CRSHNMBR")
        ))
      }
    })

  filtered_persons <-
    reactive({
      if (length(get_crshflag_list()) == 0) {
        # if no flags selected
        return (filtered_persons_no_flags())
      } else {
        # if at least 1 flag was selected
        # returns the join with filtered_crashes
        return(semi_join(
          filtered_persons_no_flags(),
          filtered_crsh_flags(),
          by = c("CRSHNMBR" = "CRSHNMBR")
        ))
      }
    })
  
  filtered_crashes_no_flags <- reactive({
    keycols = c("CNTYCODE", "CRSHDATE", "CRSHSVR") # sets keys for fast indexing, these are the fields we filter
    setkeyv(all_crashes, keycols) # this is also data.table
    yearrange <-
      interval(mdy(paste0("01-01-", min_date_selected())), mdy(paste0("12-31-", max_date_selected())))
    
    # filter data table
    filter_crashes <-
      all_crashes[CNTYCODE %in% input$cntynum &
                    CRSHSVR %in% crshsvr_selected() &
                    CRSHDATE %within% yearrange]
    filter_crashes
  })
  
  filtered_persons_no_flags <- reactive({
    keycols = c("CNTYCODE", "CRSHDATE", "CRSHSVR") # sets keys for fast indexing, these are the fields we filter
    setkeyv(all_persons, keycols) # this is also data.table
    yearrange <-
      interval(mdy(paste0("01-01-", min_date_selected())), mdy(paste0("12-31-", max_date_selected())))
    
    # filter data table
    filter_persons <-
      all_persons[CNTYCODE %in% input$cntynum &
                    CRSHSVR %in% crshsvr_selected() &
                    CRSHDATE %within% yearrange]
    filter_persons
  })
  
  filtered_vehicles <-
    reactive({
      # joins with the already filtered_crashes
      all_vehicles <-
        inner_join(all_vehicles, filtered_crashes(), by = "CRSHNMBR") # inner join keeps crashes that match by CRSHNMBR
    })

  filtered_crash_lat_long <- reactive({
    # get lat longs for map
    crash_lat_long_j <-
      filtered_crashes()[, .(lng, lat, CRSHSVR)] %>% na.omit() # remove crashes with no lat/long
    
    if (dim(crash_lat_long_j)[1] != 0) {
      # convert to sf so we can map it!
      return(st_as_sf(
        x = crash_lat_long_j,
        coords = c("lng", "lat"),
        crs = 4326
      ))
    } else { # Create fake df when nothing to map
   sf_obj = data.table(data.frame(
        lng = c(0, 0),
        lat = c(0, 0),
        CRSHSVR = c("Fatal", "Fatal")
      ))
      sf_obj <- st_as_sf(x = sf_obj,
                         coords = c("lng", "lat"),
                         crs = 4326,
                         na.fail = FALSE)
    return(sf_obj)
      }
  })

  output$get_number_of_NA <- renderText({ # Get number of crashes with no coordinates
    toString(format(sum(is.na(filtered_crashes()$lng)), big.mark = ","))
  })

################### VALUE BOXES #######################
  output$tot_crash <- renderInfoBox({
    valueBox(tags$span(HTML(
      paste0(
        '<p style="font-size: 22px">',
        format(nrow(filtered_crashes()), big.mark = ","),
        '</p>')
    )),
    tags$li(
      HTML(
        '<i class="fa fa-car-crash" style = "color:grey;"></i><p style="font-size:12px;display:inline-block;padding-right:20px;">&ensp;Crashes</p>'
      )
    ),
    color = "red")
  })
  
  # output$crash_count <- renderText({ # Try this for header?
  #   toString(format(nrow(filtered_crashes()), big.mark = ","))
  # })
  output$tot_inj <- renderInfoBox({
    valueBox(tags$span(HTML(
      paste0(
        '<p style="font-size: 22px">',
        filtered_crashes() %>% summarise(x = format(sum(TOTINJ), big.mark = ",")),
        '</p>')
    )),
    tags$li(
      HTML(
        '<i class="fa fa-first-aid" style = "color:#428BCA;"></i><p style="font-size:12px;text-align: center;display:inline-block;padding-right:20px;">&ensp;Injuries</p>'
      )),
    color = "red")
  })
  output$tot_fatal <- renderInfoBox({
    valueBox(tags$span(HTML(
      paste0(
        '<p style="font-size: 22px">',
        filtered_crashes() %>% summarise(x = format(sum(TOTFATL), big.mark = ",")),
        '</p>')
    )),
    tags$li(
      HTML(
        '<i class="fa fa-heartbeat" style = "color:#D50032;"></i><p style="font-size:12px;display:inline-block;padding-right:20px;">&ensp;Fatalities</p>'
      )),
    color = "red")
  })
  
  ################### BODY - CHARTS #######################
  #  Charts are in Shiny Modules in chart_modules_ui and in chart_modules_server
  # 
  crsh_svr_mth_server("crsh_svr_mth", filtered_crashes())
  timeofday_heat_server("timeofday_heat", filtered_crashes())
  mnrcoll_server("mnrcoll", filtered_crashes())
  person_role_treemap_server("person_role_treemap", filtered_persons())
  person_age_gender_server("person_age_gender", filtered_persons())
  drvrpc_chart_server("drvrpc_chart", filtered_persons())
  nmtact_chart_server("nmtact_chart", filtered_persons())
  nmtloc_chart_server("nmtloc_chart", filtered_persons())
  vehicle_treemap_server("vehicle_treemap", filtered_vehicles())
 
  ################### BODY - MAP #######################
  
  # odd issue with asynchronous data loading, could use renderUI so map gets updated based on user inputs
  # -> https://github.com/rstudio/leaflet/issues/151  https://github.com/rstudio/leaflet/issues/448
  
  # CRS is 4326
  output$map1 <- renderLeaflet({ #render basic map, pretty much items that do not need a reactive
      leaflet() %>%
      # Options: http://leaflet-extras.github.io/leaflet-providers/preview/
      addProviderTiles(providers$CartoDB.Voyager, options = providerTileOptions(opacity = .5)) %>% 
    # addTiles(options = providerTileOptions(opacity = .5)) %>%
      addPolygons(
        data = county$geometry,
        group = "Counties",
        color = "#444444",
        fillOpacity = 0,
        weight = 1,
        smoothFactor = 0.5
        # options = pathOptions(clickable = FALSE)
      )
  })

  observeEvent(input$cntynum, { # change view if location selected changes
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
        clearGlLayers() %>% 
        addGlPoints(  # when add points, ERROR: Uncaught TypeError: Cannot read property 'getSize' of null at s._redraw (VM123 glify-browser.js:48) MAY HAVE TO DO WITH BOUNDS??
          data = filtered_crash_lat_long(),
          fillColor = ~color_map_svr[CRSHSVR],
          fillOpacity = 1,
          radius = 5,
          layerId = "Crashes",
          group = "Crashes")
    } else {  # when HEX is ON
      leafletProxy("map1", data = filtered_crash_lat_long()) %>%
        # removeGlPoints(layerId = "Crashes") %>% # remove crashes
        clearGlLayers() %>% 
        # hideGroup("Crashes") %>% #uncheck crashes
        clearHexbin() %>%
        addHexbin(
          radius = input$hexsize,
          opacity = 1,
          options = hexbinOptions(
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
        clearGlLayers() %>% 
        # hideGroup("Crashes") %>% #uncheck crashes
        clearHexbin() %>%
        addHexbin(
          radius = input$hexsize,
          opacity = 1,
          options = hexbinOptions(
            colorRange = c("#fee0d2", "#de2d26"),#c("#fee0d2", "#de2d26"), # red #c("#b0d0f2", "#05366b"), #blue    c("#99d899", "#005100") green
            resizetoCount = TRUE,
            radiusRange = c(input$hexsize, input$hexsize), # same size, must match radius
            tooltip = "Crashes: "
          )
        )
    } else { # remove hex if unchecked
      leafletProxy("map1") %>% 
        clearHexbin() %>% 
        # removeGlPoints(layerId = "Crashes") %>% # remove crashes
        # showGroup("Crashes") %>% # check crashes
        addGlPoints( # make sure this is same as above
          data = filtered_crash_lat_long(),
          fillColor = ~color_map_svr[CRSHSVR],
          fillOpacity = 1,
          radius = 5,
          layerId = "Crashes",
          group = "Crashes")
        }
  })
}
