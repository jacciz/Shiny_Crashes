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
# library(VennDiagram)
library(ggVennDiagram)

# Run in viewer: rstudioapi::viewer("http://127.0.0.1:7583") 
# assigning colors for crash severity for map
# color_map_svr <- c("Fatal"="#D50032", "Injury"="#428BCA", "Property Damage"="#4DB848")
# New colors
color_map_svr <- c("Fatal"="#DB7E65", "Injury"="#4AAECF", "Property Damage"="#44DBAE")
crshsvr_factor_levels <- c("Property Damage", "Injury", "Fatal") # So Fatals will be on top in the map
# wisinj_factor_levels <- c("Possible Injury", "Suspected Minor Injury", "Suspected Serious Injury", "Fatal Injury")
# shinytest::recordTest("C:/W_shortcut/Shiny_Crashes_Dashboard/") test for bugs

# https://rstudio.github.io/shinyloadtest/ # month.abb[month]

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
  ################### SIDEBAR Data Inputs #######################
  # Modules are found in sidebar_userselection_modules_server.R
  # Return inputs to filter data based on user selection. When using these variables, include ()
  # since these are reactive expressions
  year_input <- select_year_server("year")
  min_year_input <- reactive(min(year_input()))
  max_year_input <- reactive(max(year_input()))
  
  county_input <- select_county_server("cntycode_input")
  municode_input <- select_municode_server("municode_input", county_input)
  output$out <- renderText(year_input()) # Print to test


  # find date range to select, returns min and max year
  min_date_selected <- reactive({
    if (min_year_input() != max_year_input()) {
      return (min_year_input())
    } else {
      return (year_input()[1]) # ERROR Warning: All formats failed to parse. No formats found.
    }
  })
  max_date_selected <- reactive({
    if (min_year_input() != max_year_input()) {
      return (max_year_input())
    } else {
      return (year_input()[1]) # ERROR Warning: All formats failed to parse. No formats found.
    }
  })
  
  # returns list for which crsh svr are selected. List must match Input IDs with field names of data
  crshsvr_selected_inputs <-  c("Fatal", "Injury", "Property Damage")
  # Looks at status of crshsvr buttoms and returns only the ones that are selected (i.e. == TRUE)
  crshsvr_selected <- reactive({
    data <- sapply(crshsvr_selected_inputs, function(x) input[[x]] ) # fields contains all values we want to save, gather all the values based on input
    data <- Filter(function(x) !(all(x == FALSE)), data) # take out FALSE values
    names(data)
  })
  
  crshflag_selected_inputs <- c("ALCFLAG", "DRUGFLAG", "speedflag", "teenflag", "olderflag", "CYCLFLAG", "PEDFLAG", "BIKEFLAG", "singlevehflag", "lanedepflag","deerflag")
  
  # Looks at status of crshsvr buttoms and returns only the ones that are selected (i.e. == TRUE)
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
          all_crsh_flags %>% select(CRSHNMBR, all_of(crshflag_list)) %>% filter_at(vars(all_of(crshflag_list)), any_vars(. == "Y")) %>% dplyr::select(CRSHNMBR)
        )
      }
      # Same, but find all_vars == Y
      return(
        all_crsh_flags %>% select(CRSHNMBR, all_of(crshflag_list)) %>% filter_at(vars(all_of(crshflag_list)), all_vars(. == "Y")) %>% dplyr::select(CRSHNMBR)
      )
    })
  
  # Takes the selected county, finds bbox so we can zoom to it
  selected_county <- reactive({ 
    sel_county <- county %>% filter(DNR_CNTY_C %in% county_input()) #COUNTY_NAM
    bbox <- st_bbox(sel_county) %>% as.vector()
    bbox
  })
  
  ################### DATA OBSERVE EVENTS OF DATA #######################
  
  # returns FINAL crash data for charts, if a flag was selected it is joined with crsh_flag list via crshnmbr
  filtered_crashes <-
    reactive({
      if (length(get_crshflag_list()) == 0) {
        # if no flags selected
        return (filtered_crashes_no_flags())
      } else {
        # if at least 1 flag was selected, returns the join with filtered_crashes
        return(semi_join(
          filtered_crashes_no_flags(),
          filtered_crsh_flags(),
          by = c("CRSHNMBR" = "CRSHNMBR")
        ))
      }
    })
  # returns FINAL person data, if a flag was selected it is joined with crsh_flag list
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
  
  # Module that filters data for each database
  # returns filtered data based of what user selected (county, year, crash severity) 
  filtered_crashes_no_flags <-
    filter_data(
      "crash",
      all_crashes,
      min_date_selected,
      max_date_selected,
      county_input,
      muni_input,
      crshsvr_selected
    )
  filtered_persons_no_flags <-
    filter_data(
      "person",
      all_persons,
      min_date_selected,
      max_date_selected,
      county_input,
      muni_input,
      crshsvr_selected
    )

  filtered_vehicles <-       # joins with the already filtered_crashes
    reactive({
      all_vehicles <-
        inner_join(all_vehicles, filtered_crashes(), by = "CRSHNMBR") # inner join keeps crashes that match by CRSHNMBR
    })
  
  # Grabs the lat, longs, and crsh_svr for mapping
  filtered_crash_lat_long <- reactive({
    crash_lat_long_j <-
      filtered_crashes()[, .(lng, lat, CRSHSVR)] %>% na.omit() %>% arrange(factor(CRSHSVR, levels  = crshsvr_factor_levels))# remove crashes with no lat/long
    
    if (dim(crash_lat_long_j)[1] != 0) {
      # convert to sf so we can map it!
      return(
        crash_lat_long_j <- st_as_sf(
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
  
  # Get number of crashes with no coordinates
  output$get_number_of_NA <- renderText({
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
        '<i class="fa fa-car-crash" style = "color:grey;"></i><p style="font-size:16px;display:inline-block;padding-right:20px;">&ensp;Crashes</p>'
      )
    ),
    color = "red")
  })

  output$tot_inj <- renderInfoBox({
    valueBox(tags$span(HTML(
      paste0(
        '<p style="font-size: 22px">',
        filtered_crashes() %>% summarise(x = format(sum(TOTINJ), big.mark = ",")),
        '</p>')
    )),
    tags$li(
      HTML(
        '<i class="fa fa-first-aid" style = "color:#428BCA;"></i><p style="font-size:16px;text-align: center;display:inline-block;padding-right:20px;">&ensp;People injured</p>'
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
        '<i class="fa fa-heartbeat" style = "color:#D50032;"></i><p style="font-size:16px;display:inline-block;padding-right:20px;">&ensp;People killed</p>'
      )),
    color = "red")
  })
  
  ################### BODY - CHARTS MODULES #######################
  #  Charts are in Shiny Modules in chart_modules_ui and in chart_modules_server
  crsh_svr_mth_server("crsh_svr_mth", filtered_crashes)
  wisinj_by_year_server("wisinj_by_year", filtered_persons)
  timeofday_heat_server("timeofday_heat", filtered_crashes)
  mnrcoll_server("mnrcoll", filtered_crashes)
  person_role_treemap_server("person_role_treemap", filtered_persons)
  person_age_gender_server("person_age_gender", filtered_persons)
  drvrpc_chart_server("drvrpc_chart", filtered_persons)
  nmtact_chart_server("nmtact_chart", filtered_persons)
  nmtloc_chart_server("nmtloc_chart", filtered_persons)
  vehicle_treemap_server("vehicle_treemap", filtered_vehicles)

  ################### BODY - MAP #######################
  
  # odd issue with asynchronous data loading, could use renderUI so map gets updated based on user inputs
  # -> https://github.com/rstudio/leaflet/issues/151  https://github.com/rstudio/leaflet/issues/448
  # Leaflet Options: http://leaflet-extras.github.io/leaflet-providers/preview/
  
  # render basic map (CRS is 4326), i.e. items that do not need a reactive
  output$map1 <- renderLeaflet({
    leaflet() %>%
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

  output$venn <- renderPlotly({
    if (length(get_crshflag_list()) < 2 | length(get_crshflag_list()) > 4 | dim(filtered_crashes())[1] == 0) {  # or no crashes with a time ??
      plotly_empty(type = "scatter", mode = 'markers') %>% layout(
        title = list(
          text = "\nVenn is for 2 - 4 crash flags",
          font = chart_title,
          x = 0
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
      
      # get df of all selected and filtered crash flags
    filtered_flags <- left_join(filtered_crsh_flags(), all_crsh_flags, by = "CRSHNMBR") #%>%  select(CRSHNMBR, list)

    # Function to select crashnmbr of a crash flag
    get_flag_crshnmbr_list <- function(flag_name){
      filtered_flags[get(flag_name) == "Y", CRSHNMBR] # works
    }
    
    # list <-  c("teenflag","olderflag", "lanedepflag") %>% unlist() %>% as.character()
    list <- get_crshflag_list() #%>%  as.list()
    # crshflag_selected_inputs <- c("ALCFLAG", "DRUGFLAG", "speedflag") # "teenflag", "olderflag", "CYCLFLAG", "PEDFLAG", "BIKEFLAG", "singlevehflag", "lanedepflag","deerflag")# %>% as.list()
    # Returns a list of a list of crash number by flag
    flag_list <- as.list(sapply(list, get_flag_crshnmbr_list, USE.NAMES = TRUE))
    
    gg <- # , category.names = crshflag_selected_inputs
      ggVennDiagram(x = flag_list) #+ scale_fill_gradient(low = "#fee0d2", high = "#de2d26")  #%>%
    gg %>%  
    ggplotly(type = 'scatter', mode = "lines+markers+text") %>% # %>% #Price: %{y:$.2f}<extra></extra>
      # ggrepel::GeomTextRepel(both)
      # geom_text_repel(aes(label = both))
      # add_annotations(text= ~both) %>% add_text(text = ~both)
      # add_annotations(text= ~count, textposition = "top right",arrowhead=.5, arrowwidth=1, font = list(size = 16, color = "#ffffff")) %>%
      # add_text(text= ~count) %>% 
      layout(
        title = list(
          text = "\nCrash Flags",
          font = chart_title,
          y = 1,
          x = 0
        ),
        margin = list(
          r = 0,
          l = 0,
          b = 0
          # t = 45
        ),
        xaxis = list(
          zeroline=F, showline=F, showgrid=F
        ),
        yaxis = list(
          zeroline=F, showline=F, showgrid=F
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%  config(
        toImageButtonOptions = list(
          width = 800,
          height = 800,
          filename = "Crash Flags Venn Diagram",
          scale = 2
        )
      )
    }
  })
  
}
 