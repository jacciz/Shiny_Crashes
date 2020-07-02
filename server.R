library(dplyr) # select, filter functions
library(ggplot2) # create pretty graphs
library(DT)    # create pretty tables
library(expss) # format freq tables, tab_cells
# library(forcats) # reorder freq in charts
library(plotly) # interactive charts
library(lubridate) # for dates
library(htmltools) # buttons and stuff
library(htmlwidgets) # buttons and stuff
library(leaflet) # the map
library(leaflet.extras2) # hexbin
library(tigris) # census tiger files
library(sf) # spatial analysis
library(data.table) # setnames function, data format for large data
library(tibble) # quick data frames

# shinytest::recordTest("C:/W_shortcut/Shiny_Crashes_Dashboard/") test for bugs

# run this code in the console to see performance (total time is 10230, app start 2880) now 9170 & 790
# profvis::profvis({ shiny::runApp('C:/W_shortcut/Shiny_Crashes_Dashboard/') })
# shinyloadtest::record_session(shiny::runApp('C:/W_shortcut/Shiny_Crashes_Dashboard/'), output_file = "C:/W_shortcut/recording.log")
# shinyloadtest::record_session("http://127.0.0.1:3184") # run with Docker

# https://rstudio.github.io/shinyloadtest/

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
  # Load county spatial data
  # wi_counties <- counties(state = '55', cb=TRUE, class = 'sf') # get counties data
  # wi_munis <- places(state = '55', cb=TRUE, class = 'sf')
  # wi_counties84 <- st_transform(wi_counties, crs = ('+proj=longlat +datum=WGS84')) # dont need to do
  # wi_counties_crs <- st_transform(wi_counties, 3071) # CRS, dont need to do
  
  updateSelectInput(session, # choose county
                    "cntynum", selected = 13, # default selection
                    choices = setNames(county_recode$CountyCode, county_recode$CountyName))

  observeEvent(input$cntynum, { # choose municipality

    muni_cnty_list <- muni_recode %>% filter(CntyCode %in% input$cntynum)

    updateSelectInput(session, "muni_names",
                      choices = setNames(muni_cnty_list$MuniCode, muni_cnty_list$Municipality_CTV) )
  })
  
  updateSelectInput(session, "year",
                    selected = 2019, # default selection
                    choices = c(2020, 2019, 2018, 2017)) #Set years of data
  
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
filtered_crsh_flags <- reactive({
  # what flags are selected - this is set to AND
  rename_crsh_flags <- # rename inputs so we can select flag columns
    c("Alcohol-related" = "ALCFLAG",
      "Drug-related" = "DRUGFLAG",
      # "Distracted driving",  # don't have this one, also CMV
      'Speeding' = 'speedflag',
      'Teen driver' = 'teenflag',
      'Older driver' = 'olderflag',
      "Bicyclist" = "BIKEFLAG",
      "Pedestrian" = "PEDFLAG",
      "Motorcycle" = "CYCLFLAG"
    )
  new_crsh_flags <-
    rename_crsh_flags[input$crsh_flags] # apply the rename to get a list
  # crsh_flags <- c("Alcohol-related", "Speeding")
  
  selected_crash_flags <-
    # selects crash flags, goes through each row and finds all Y
    all_crsh_flags[apply(all_crsh_flags [, ..new_crsh_flags], 1, function(x)
      all(x == "Y")),] %>% dplyr::filter(!is.na(CRSHNMBR)) # all() so ALL flags are selected
  selected_crash_flags # returns datatable with crshnmbr to match
})

filtered_crashes <-
  reactive({
    if (length(input$crsh_flags) == 0) {
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
    if (length(input$crsh_flags) == 0) {
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
                  CRSHSVR %in% input$crsh_svr & CRSHDATE %within% yearrange]
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
                  CRSHSVR %in% input$crsh_svr & CRSHDATE %within% yearrange]
  filter_persons
})

filtered_vehicles <- reactive({ # joins with the already filtered_crashes
  all_vehicles <-
    inner_join(all_vehicles, filtered_crashes(), by = "CRSHNMBR") # inner join keeps crashes that match my CRSHNMBR
})

filtered_crash_lat_long <- reactive({  # get lat longs for map
  crash_lat_long_j <-
    filtered_crashes()[, .(LONDECDG, LATDECDG)] %>% na.omit() # select lat long columns
  
  setnames(crash_lat_long_j, "LONDECDG", "lng") # rename so leaflet grabs correct columns
  setnames(crash_lat_long_j, "LATDECDG", "lat")
})

  # Value boxes change font size by tags$p("100", style = "font-size: 200%;")
  output$tot_crash <- renderInfoBox({
    valueBox(
      # tags$h6("11,888", style = "font-size: 100%; vertical-align: middle;"),
      format(nrow(filtered_crashes()), big.mark = ","),
      "Total Crashes",
      icon = icon("car-crash"),
      color = "red"
    )
  })
  output$tot_inj <- renderInfoBox({
    valueBox(
      filtered_crashes() %>% summarise(x = format(sum(TOTINJ), big.mark = ",")),
      "Total Injuries",
      icon = icon("band-aid"),
      color = "red"
    )
  })
  output$tot_fatal <- renderInfoBox({
    valueBox(
      filtered_crashes() %>% summarise(x = sum(TOTFATL)) %>% format(big.mark = ","),
      "Total Fatalities",
      icon = icon("skull"),
      color = "red"
    )
  })
  output$passveh_box <- renderInfoBox({
    valueBox(
      filtered_vehicles() %>% filter(
        VEHTYPE %in% c(
          "Passenger Car",
          "(Sport) Utility Vehicle",
          "Passenger Van",
          "Cargo Van (10,000 Lbs or Less)"
        )
      ) %>% nrow() %>% format(big.mark = ","),
      "Passenger Veh.",
      icon = icon("car"),
      color = "yellow"
    )
  })
  output$light_truck_box <- renderInfoBox({
    valueBox(
      filtered_vehicles() %>% filter(
        VEHTYPE %in% c("Utility Truck/Pickup Truck")
      ) %>% nrow() %>% format(big.mark = ","),
      "Light Trucks",
      icon = icon("truck"),
      color = "yellow"
    )
  })
  output$large_truck_box <- renderInfoBox({
    valueBox(
      filtered_vehicles() %>% filter(
        VEHTYPE %in% c(
          "Straight Truck",
          "Truck Tractor (Trailer Not Attached)",
          "Truck Tractor (Trailer Attached)",
          "Truck Tractor (More Than One Trailer)"
        )
      ) %>% nrow() %>% format(big.mark = ","),
      "Large Trucks",
      icon = icon("truck-moving"),
      color = "yellow"
    )
  })
  output$motorcycle_box <- renderInfoBox({
    valueBox(
      filtered_vehicles() %>% filter(
        VEHTYPE %in% c("Motorcycle")
      ) %>% nrow() %>% format(big.mark = ","),
      "Motorcycles",
      icon = icon("motorcycle"),
      color = "yellow"
    )
  })
  output$bike_box <- renderInfoBox({
    valueBox(
      filtered_vehicles() %>% filter(
        VEHTYPE %in% c("Bicycle")
      ) %>% nrow() %>% format(big.mark = ","),
      "Bicycles",
      icon = icon("bicycle"),
      color = "yellow"
    )
  })
  output$ped_box <- renderInfoBox({  # should I use vehicle??
    valueBox(
      filtered_persons() %>% filter(
        ROLE %in% c("Pedestrian")
      ) %>% nrow() %>% format(big.mark = ","),
      "Pedestrians",
      icon = icon("walking"),
      color = "yellow"
    )
  })

  ################### BODY CHARTS #######################
  chart_title = list(size = 16, color = "rgb(205,205,205)", family = "Cambria")
  chart_axis = list(size = 14, color = "rgb(205,205,205)", family = "Cambria")
  chart_axis_bar = list(size = 14, color = "#428BCA", family = "Cambria", face = "bold")

  output$crsh_svr_mth <- renderPlotly({
    
    if (dim(filtered_crashes())[1] == 0) { # or no crashes with a time ??
      plotly_empty(type = "bar") %>% layout(
        title = list(text ="Crash Severity by Month", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
      crshsvr_table <-
        table(month = filtered_crashes()$CRSHMTH, svr = filtered_crashes()$CRSHSVR) %>% as_tibble() # get counts, put in a tibble
      crshsvr_table$month <-
        factor(crshsvr_table$month, levels = month.name) # this orders months
    
    # crshsvr_table$month <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec") # rename months
    # month_factor = month.name
      
    plot_ly(
      crshsvr_table,
      type = 'bar',
      x = ~ month,
      y = ~ n,
      color = ~ svr,
      colors = c("#D50032", "#428BCA", "#4DB848"),  #colors for female, male, unknown in this order
      hovertemplate = paste('%{x}<br>',
                            '<b>%{y: .0f} Crashes')
    ) %>% #Price: %{y:$.2f}<extra></extra>
      layout(
        title = list(text ="Crash Severity by Month", font = chart_title, y = 1, x = 0),
        legend = list(
          x = .5,
          y = 1.2,
          orientation = 'h',
          font = chart_axis
        ),
        margin = list(r = 0, l = 0, b = 0, t = 45
        ),
        xaxis = list(
          title = "",
          tickfont = chart_axis,
          tickangle = 0,
          # automargin = TRUE,
          dtick = 5 # every 5 months are labeled
        ),
        yaxis = list(
          showgrid = FALSE,
          tickfont = chart_axis
        ),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)',
        barmode = 'stack'
      )
    }
    # scale_x_discrete(limits = month.name, name = "", labels = function(labels) {  # scatter labels
    # sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}
  })

  output$timeofday_heat <- renderPlotly({
    
    if (dim(filtered_crashes())[1] == 0) { # or no crashes with a time ??
      plotly_empty(type = "heatmap") %>% layout(
        title = list(text ="Time of Day Crashes", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
      day_time_data <- filtered_crashes()[ , .(.N), by = .(newtime, DAYNMBR)]
      day_time_data[DAYNMBR == "", DAYNMBR := NA] # if DAYNMBR not exist, make it NA
      day_time_data <- day_time_data %>% na.omit() # remove all NA values
      day_time_data <- dcast(day_time_data, newtime ~ DAYNMBR, # reshape to long table
                             value.var = "N", fill = 0)
      
    # Used to create the empty tibble
    x <- c("Sunday", "Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday") #newtime
    y = c("12am","1am","2am","3am", "4am","5am", "6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm",
          "5pm","6pm","7pm","8pm","9pm","10pm","11pm")    
    
    # create an empty tibble so we get a full matrix for heat map
    empty_tibble <- tibble(newtime = y)

    # Combine empty tibble with data, use mutate to ensure levels match
    time_tibble <- left_join(mutate(empty_tibble, newtime=factor(newtime, levels=y)), day_time_data, by = c("newtime" = "newtime"))
    
    # function to find if column exists, if not, adds column with NA values
    fncols <- function(data, cname) {
      add <-cname[!cname%in%names(data)]
      if(length(add)!=0) data[add] <- 0
      data
    }
    
    day_time <- fncols(time_tibble, x) # apply function to get all columns
    day_time[is.na(day_time)] = 0 # NA will be 0
    
    day_time <-
      day_time[, c( # reorder columns
        "newtime","Sunday", "Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday"
      )]
    names(day_time) <-
      c("newtime", "Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat") # rename columns
    
    m <- day_time[, 2:8] %>% as.matrix()
    
    # get blue color gradient
    vals <- unique(scales::rescale(m))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Blues", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    
    plot_ly(
      x = colnames(day_time[2:8]),
      y = day_time$newtime,
      z = m, # crash count
      type = "heatmap",
      colorscale = colz,
      showscale = FALSE, # No legend
      hovertemplate = paste('%{x} %{y}<br>',
                            '<b>%{z:.0f} Crashes')
      ) %>% 
      layout(
        title = list(text ="Time of Day Crashes", font = chart_title, x = 0),
        margin = list(r = 0,l = 0, b = 0
        ),
        xaxis = list(tickfont = chart_axis, tickangle = 0),
        yaxis = list(tickfont = chart_axis),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    }
  })

    output$mnrcoll <- renderPlotly({
      if (dim(filtered_crashes())[1] == 0){ # if no crashes, show empty plot, else make plot
        # hide("mnrcoll")
        plotly_empty(type = "bar") %>% layout(
          title = list(text ="Manner of Collision", font = chart_title, x = 0),
          plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        
      mnr_crashes <- filtered_crashes() %>%
        filter(MNRCOLL != "Unknown")
      
      mnr_crashes_table <- table(mnrcoll = mnr_crashes$MNRCOLL) %>% as_tibble()
      
      plot_ly(
        mnr_crashes_table,
        type = 'bar',
        orientation = 'h',
        x = ~ n,
        y = ~ reorder(mnrcoll, n), # reorder from big to small values
        marker = list(color = "#428BCA"), # blue!
        hovertemplate = paste('%{text}', '<br>%{x: .0f} Crashes<br>'),
        text = ~ format(n, big.mark=","), # bar end number
        textfont = chart_axis_bar,
        textposition = 'outside',
        cliponaxis = FALSE
      ) %>%
        layout(
          title = list(text ="Manner of Collision", font = chart_title, x = 0),
          margin = list(
            r = 30, # set to 30 so labels don't get cut off
            l = 0,
            # t = 0, # this will cut off title
            b = 0
          ),
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE # remove axis labels
          ),
          yaxis = list(title = "", tickfont = chart_axis),
          plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      }
  })

  output$person_role <- renderPlotly({  # have a symbol for each role
    
    if (dim(filtered_persons())[1] == 0) { # or no crashes with a time ??
      plotly_empty(type = "bar") %>% layout(
        title = list(text ="Role of Persons Involved", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
    
    role_table <- table(role = filtered_persons()$ROLE) %>% as_tibble() # get counts of ROLE, put in a tibble

    plot_ly(
      role_table,
      type = 'bar',
      orientation = 'h',
      x = ~ n,
      y = ~ reorder(role, n), # reorder from big to small values
      marker = list(color = "#428BCA"), # blue!
      # hovertemplate = paste('%{x}', '<br>Count: %{text:.2s}<br>'),
      text = ~ format(n, big.mark=","),
      textfont = chart_axis_bar,
      textposition = 'outside',
      cliponaxis = FALSE
    ) %>%
      layout(
        title = list(text ="Role of Persons Involved", font = chart_title, x = 0),
        margin = list(
          r = 30, # set to 30 so labels don't get cut off
          l = 0,
          b = 0
        ),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          showticklabels = FALSE # remove axis labels
        ),
        yaxis = list(title = "", tickfont = chart_axis),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    }
  })
  
  output$person_role_treemap <- renderPlotly({
    
    if (dim(filtered_persons())[1] == 0) { # or no crashes with a time ??
      plotly_empty(type = "treemap") %>% layout(
        title = list(text ="Role of Persons Involved", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
    
    role_table <- table(role = filtered_persons()$ROLE) %>% as_tibble() %>% mutate(parent = "role")
    plot_ly(
      role_table,
      type = 'treemap',
      textfont = list(size = 14, family = "Cambria"),
      labels = ~role,
      parents = ~parent,
      values = ~n
    ) %>% 
      layout(
        title = list(text ="Role of Persons Involved", font = chart_title, y = 1, x = 0),
        margin = list(r = 0, l = 0, b = 10, t = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    }
  })

  output$person_age_gender <- renderPlotly({
    
    if (dim(filtered_persons())[1] == 0) { # or no crashes with a time ??
      plotly_empty(type = "bar") %>% layout(
        title = list(text ="Age and Gender of Persons Involved", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
    
    person <-
      filtered_persons()[, .(age_group, SEX)]
    
    age_sex_table <- table(age = person$age_group, sex = person$SEX) %>% as_tibble() # get counts, put in a tibble

    plot_ly(
      age_sex_table,
      type = 'bar',
      x = ~ age,
      y = ~ n,
      color = ~ sex,
      colors = c("#D50032","#428BCA", "#F9C218"), #colors for female, male, unknown in this order
      hovertemplate = paste('<br>%{x}<br>',
                            '<b>%{y: .0f} people<b>')
    ) %>%
      layout(
        title = list(text ="Age and Gender of Persons Involved", font = chart_title, y = 1, x = 0),
        legend = list(x = .5, y = 1.2, orientation = 'h', font = chart_axis),
        margin = list(
          r = 0,
          l = 0,
          b = 0,
          t = 45
        ),
        xaxis = list(title = "", tickfont = chart_axis, tickangle = 0, categoryarray = ~age, categoryorder = "array", dtick = 2),
        yaxis = list(title = "", showgrid = FALSE, tickfont = chart_axis),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)',
        barmode = 'stack'
      )
        # labels = function(labels) { # this scatters labels so they fit on two lines
        #   sapply(seq_along(labels), function(i)
        #     paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        # }
    }
   })
  output$vehicle_treemap <- renderPlotly({
    
    if (dim(filtered_vehicles())[1] == 0) {
      plotly_empty(type = "treemap") %>% layout(
        title = list(text ="Vehicles Involved", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
    newcar <- filtered_vehicles() %>% # put this is data import
      mutate(
        cate = case_when(
          VEHTYPE == "Passenger Car" ~ "Passenger Veh.",
          VEHTYPE == "(Sport) Utility Vehicle" ~ "Passenger Veh.",
          VEHTYPE == "Cargo Van (10,000 Lbs or Less)" ~ "Passenger Veh.",
          VEHTYPE == "Passenger Van" ~ "Passenger Veh.",
          VEHTYPE == "Utility Truck/Pickup Truck" ~ "Light Trucks",
          VEHTYPE == "Straight Truck" ~ "Large Trucks",
          VEHTYPE == "Truck Tractor (Trailer Not Attached)" ~ "Large Trucks",
          VEHTYPE == "Truck Tractor (Trailer Attached)" ~ "Large Trucks",
          VEHTYPE == "Truck Tractor (More Than One Trailer)" ~ "Large Trucks",
          VEHTYPE == VEHTYPE ~ "Other"
        )
      )
    car_tib <- # then change newcar to filtered_vehicles()
      table(vehtype = newcar$VEHTYPE, parent = newcar$cate) %>%
      as_tibble() %>% filter(n != 0)
    
    parent_tib <- xtabs(car_tib$n~car_tib$parent) %>% as_tibble()# then rbind
    names(parent_tib)[names(parent_tib)=="car_tib$parent"] <- "vehtype"
    parent_tib <- parent_tib %>% mutate(parent = "Vehicle Type")
    
    car_tib <- rbind(car_tib, parent_tib)
    
    plot_ly(car_tib,
            type='treemap',
            branchvalues="total",
            textfont = list(size = 14, family = "Cambria"),
            # insidetextfont = list(size = 14, family = "Cambria"),
            tiling = list(packing = "ratio"),
            ids = ~vehtype,
            labels= ~vehtype,
            parents= ~parent,
            values= ~n,
            textinfo="label+value+percent parent+percent"
    ) %>% 
      layout(
        title = list(text ="Vehicles Involved", font = chart_title, y = 1, x = 0),
        margin = list(r = 0, l = 0, b = 0, t = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    }
  })
# this is the map - using Leaflet, can also do clusterOptions = markerClusterOptions()

    # odd issue with asynchronous data loading, need to renderUI so map gets updated based on user inputs
  # -> https://github.com/rstudio/leaflet/issues/151

  output$map <- renderUI({ # HEXBIN WHEN ZOOMED IN??
  # this takes the selected county and zooms to it
    # county <- wi_counties %>% filter(NAME %in% input$counties)
    #   bbox <- st_bbox(county) %>% as.vector()
    #   fitBounds(county[1], county[2], county[3], county[4]) %>% # zooms to county

    tomap <- filtered_crash_lat_long()
    id = "map_TRUE" # not sure what this means but it should be set to TRUE
    output[[id]] = renderLeaflet ({ # this initializes the map, need observeEvent when layers change
      input$print
      mymap <- filtered_crash_lat_long() %>%
        leaflet() %>% addTiles() %>%
        # addPolygons(
        #   data = wi_counties$geometry,
        #   group = "Counties",
        #   color = "#444444",
        #   fillOpacity = 0,
        #   weight = 1,
        #   smoothFactor = 0.5
        # ) %>%
        addCircleMarkers(
          group = "Crashes",
          fillColor = "red",
          radius = 4,
          # fillOpacity = 0.2,
          stroke = FALSE
        ) %>%
      addLayersControl(
        overlayGroups = c(
          "Crashes"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
      mymap
    })
    observe({ # observe when hexsize changes
      if (input$hex) {
      leafletProxy("map_TRUE", data = tomap) %>%
        clearHexbin() %>%
        addHexbin( # both addHexbin functions must match
          lng = tomap$lng,
          lat = tomap$lat,
          radius = input$hexsize,
          opacity = 0.8,
          options = hexbinOptions(
            colorRange = c("#b0d0f2", "#05366b"), #blue  c("#99d899", "#005100") green
            resizetoCount = TRUE,
            radiusRange = c(input$hexsize, input$hexsize) # same size, must match radius
          )
        )
      }
    })
    
    observeEvent(input$hex, { # observe if hex has been checked
      proxy <- leafletProxy("map_TRUE", data = tomap)
      # proxy %>% clearControls() # what does this do?
      if (input$hex) {  # both addHexbin functions must match
        proxy %>% clearHexbin() %>% 
            addHexbin(
                lng = tomap$lng,
                lat = tomap$lat,
                radius = input$hexsize,
                opacity = 0.8,
                options = hexbinOptions(
                  colorRange = c("#b0d0f2", "#05366b"),
                  resizetoCount = TRUE,
                  radiusRange = c(input$hexsize, input$hexsize) # same size, must match radius
                )
              )
      } else {
        proxy %>% hideHexbin()
      }
    })
    leafletOutput(id, height = "600px") #from 680
  })
}
