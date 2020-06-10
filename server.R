library(dplyr) # select, filter functions
library(ggplot2) # create pretty graphs
library(DT)    # create pretty tables
# library(expss) # format freq tables
# library(forcats) # reorder freq in charts
library(plotly) # interactive charts
# library(d3heatmap) # makes time of day / week heat chart
library(lubridate) # for dates
library(htmltools)
library(htmlwidgets)
# library(ggrepel)  # adjusts labels for ggplots, not for axis
library(leaflet)
# library(leaflethex)
library(leaflet.extras2) # hexbin, newer than leafthehe
library(tidyverse)
# library(r2d3)
# library(rbokeh)
# library(tmap)
# library(geogrid)
# library(hexbin)
# library(shinyjs)
# library(RColorBrewer)

# download.file('https://rawgit.com/Asymmetrik/leaflet-d3/master/src/js/hexbin/HexbinLayer.js', 'C:/CSV/hex.js', mode="wb")
# helloLocalFile <- htmlDependency("hex", "1.0",
                                 # src = c(file = normalizePath('C:/CSV')),  script = "hex.js")
# jsfile <- "https://rawgit.com/Asymmetrik/leaflet-d3/master/src/js/hexbin/HexbinLayer.js" # Hex map file
#
server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
    attachDependencies( # import file for hex
      tags$script("
        alert('goodbye')
      "),
       helloLocalFile
    )

  })

  updateSelectInput(session, # choose county
                    "cntynum", selected = 13, # default selection
                    choices = setNames(county_recode$CountyCode, county_recode$CountyName))

  observeEvent(input$cntynum, { # choose municipality

    muni_cnty_list <- muni_recode %>% filter(CntyCode %in% input$cntynum)

    updateSelectInput(session,
                      "muni_names",
                      choices = setNames(muni_cnty_list$MuniCode, muni_cnty_list$Municipality_CTV) )
  })

  output$crsh_svr_out <- renderPrint(input$crsh_svr) # delete?

  updateSelectInput(session,
                    "year",
                    selected = 2019,
                    # default selection
                    choices = c(2020, 2019, 2018, 2017)) #Set years of data

    # Filtered data based on input by the user
  crash_flags_selected <-
    reactive({
      # what flags are selected - this is set to OR (not AND)
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
        # seleced_crash_flag_crshes = c('Speeding','Teen driver', 'Older driver')
        seleced_crash_flag_crshes <-
          crsh_flags[apply(crsh_flags [new_crsh_flags], 1, function(x)
            any(x == "Y")), ] %>% dplyr::filter(!is.na(CRSHNMBR)) # any, so OR flags are selected
        return (seleced_crash_flag_crshes$CRSHNMBR) # output is df of CRSHNMBRS
    })

  filtered_crashes <- reactive({
    all_crashes %>%
      filter(
        CNTYCODE %in% input$cntynum,
        # CNTYCODE %in% 13,
        # year(CRSHDATE) %in% 2018
        year(CRSHDATE) %in% input$year,
        if (length(input$crsh_flags) > 0) CRSHNMBR %in% crash_flags_selected() else CRSHNMBR

        # CRSHSVR %in% crsh_svr_out - wrong)
        # INJSVR %in% input$inj_svr_out
      )
  })

  filtered_persons <- reactive({
    all_persons %>%
      filter(
        CNTYCODE %in% input$cntynum,
        year(CRSHDATE) %in% input$year,
        if (length(input$crsh_flags) > 0) CRSHNMBR %in% crash_flags_selected() else CRSHNMBR,
        WISINJ %in% input$inj_svr
      )
  })

  filtered_vehicles <- reactive({
    all_vehicles <-
      inner_join(all_vehicles, filtered_crashes(), by = "CRSHNMBR") # inner join keeps crashes that match my CRSHNMBR
  })

  filtered_crash_lat_long <- reactive({ # get lat longs for map

    crash_lat_long_j = filtered_crashes() %>% dplyr::filter(!is.na(LATDECDG)) %>% select(LONDECDG, LATDECDG) # get rid on NA values, i.e. crashes not mapped
    # crashes_to_map = crash_lat_long[1:20,] %>% dplyr::filter(!is.na(LATDECDG)) %>% select(LONDECDG, LATDECDG)
    setnames(crash_lat_long_j, "LONDECDG", "lng") # could move this to data import part
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
  # output$tot_some <- renderInfoBox({
  #   valueBox(
  #     3400, "Total Vehicles", icon = icon("car"),
  #     color = "red"
  #   )
  # })
  #
  # X row charts
  output$passveh_box <- renderInfoBox({
    valueBox(
      filtered_vehicles() %>% filter(
        VEHTYPE %in% c(
          "Passenger Car",
          "(Sport) Utility Vehicle",
          "Passenger Car",
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

# SECOND row charts

  output$crsh_svr_mth <- renderPlotly({
    
    crshsvr_table <- table(month = filtered_crashes()$CRSHMTH, svr = filtered_crashes()$CRSHSVR) %>% as_tibble() # get counts, put in a tibble
    
    crshsvr_table$month <- factor(crshsvr_table$month, levels = month.name) # this orders months
    
    # crshsvr_table$month <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec") # rename months

    plot_ly(
      crshsvr_table,
      type = 'bar',
      x = ~ month,
      y = ~ n,
      color = ~ svr,
      colors = c("#D50032", "#428BCA", "#4DB848"), #colors for female, male, unknown in this order
      hovertemplate = paste('<br>%{x}<br>',
                            '<b>%{y: .0f} Crashes<b>')
    ) %>%
      layout(
        legend = list(
          x = 0,
          y = 100,
          orientation = 'h',
          font = list(size = 10, color = "white")
        ),
        margin = list(
          r = 0,
          l = 0,
          t = 0,
          b = 0
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 10, color = "white"),
          tickangle = 0,
          # automargin = TRUE
          dtick = 5 # every 5 months are labeled
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          tickfont = list(size = 10, color = "white")
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)',
        barmode = 'stack'
      )
      # scale_x_discrete(limits = month.name, name = "", labels = function(labels) {  # scatter labels
        # sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}
  
  })

  output$timeofday_heat <- renderPlotly({
    day_time_data <- filtered_crashes() %>% # make a df for chart
      na.omit(newtime) %>%
      mutate(newtime = forcats::fct_explicit_na(newtime)) %>% # keep NA value, do this
      group_by(newtime, DAYNMBR) %>%
      summarise(n = n()) %>%
      filter(newtime != '') %>%
      tidyr::spread(DAYNMBR, n, fill = 0) 
    
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
      if(length(add)!=0) data[add] <- NA
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
      hovertemplate = paste('<br>%{x} %{y}<br>',
                            '<b>%{z:.0f} Crashes<b>')
      ) %>% 
      layout(
        margin = list(r = 0,l = 0,t = 0,b = 0
        ),
        xaxis = list(tickfont = list(size = 10, color = "white"), tickangle = 0),
        yaxis = list(tickfont = list(size = 10, color = "white")),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })

    output$mnrcoll <- renderPlotly({
    
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
      textfont = list(
        family = 'Cambria',
        size = 10,
        color = 'white'
      ),
      textposition = 'outside',
      cliponaxis = FALSE
    ) %>%
      layout(
        margin = list(
          r = 30, # set to 30 so labels don't get cut off
          l = 0,
          t = 0,
          b = 0
        ),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          showticklabels = FALSE # remove axis labels
        ),
        yaxis = list(title = "", tickfont = list(size = 10, color = "white")),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })

  output$person_role <- renderPlotly({  # have a symbol for each role
    
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
      textfont = list(
        family = 'Cambria',
        size = 10,
        color = 'white'
      ),
      textposition = 'outside',
      cliponaxis = FALSE
    ) %>%
      layout(
        margin = list(
          r = 30, # set to 30 so labels don't get cut off
          l = 0,
          t = 0,
          b = 0
        ),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          showticklabels = FALSE # remove axis labels
        ),
        yaxis = list(title = "", tickfont = list(size = 10, color = "white")),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })

  output$person_age_gender <- renderPlotly({
    
    person <-
      filtered_persons() %>% select(age_group, SEX)
    
    age_sex_table <- table(age = person$age_group, sex = person$SEX) %>% as_tibble() # get counts, put in a tibble

    plot_ly(
      age_sex_table,
      type = 'bar',
      x = ~ age,
      y = ~ n,
      color = ~ sex,
      colors = c("#D50032","#428BCA", "#F9C218"), #colors for female, male, unknown in this order
      hovertemplate = paste('<br>%{x}<br>',
                            '<b>%{y: .0f}<b>')
    ) %>%
      layout(
        legend = list(x = 0, y = 100, orientation = 'h', font = list(size = 10, color = "white")),
        margin = list(
          r = 0,
          l = 0,
          t = 0,
          b = 0
        ),
        xaxis = list(title = "", tickfont = list(size = 10, color = "white"), tickangle = 0),
        yaxis = list(title = "", showgrid = FALSE, tickfont = list(size = 10, color = "white")),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)',
        barmode = 'stack'
      )
        # labels = function(labels) { # this scatters labels so they fit on two lines
        #   sapply(seq_along(labels), function(i)
        #     paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        # }
   })

# this is the map - using Leaflet, can also do clusterOptions = markerClusterOptions()

    # odd issue with asynchronous data loading, need to renderUI so map gets updated based on user inputs
  # -> https://github.com/rstudio/leaflet/issues/418

  output$map <- renderUI({
    # works!!
    tomap <- filtered_crash_lat_long()
    id = "map_TRUE" # not sure what this means but it should be set to TRUE
    output[[id]] = renderLeaflet ({
      input$print
      mymap <- tomap %>%
        leaflet() %>% addTiles() %>% addCircleMarkers(
          group = "Crashes",
          fillColor = "red",
          radius = 1,
          fillOpacity = 0.2,
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
    observeEvent(input$hexize, { # observe if hexsize changed
      proxy <- leafletProxy("map_TRUE", data = tomap)
      # proxy %>% clearControls()
      if (input$hexsize) { # both addHexbin functions must match
        proxy %>%
          clearHexbin() %>%
          addHexbin(
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
      proxy %>% clearControls() # what does this do?
      if (input$hex) {  # both addHexbin functions must match
        proxy %>% addHexbin(
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

    leafletOutput(id)
  })
  # TABLES
  table_crsh <- all_crashes %>%
    tab_cells(CNTYCODE) %>%                           # stuff to put in the rows
    tab_subgroup(ALCFLAG == "Yes") %>%                # only select certain elements
    tab_cols(CRSHSVR %nest% ALCFLAG, total()) %>%     # columns with nesting
    tab_stat_cases(total_label = "Total Crashes") %>% # frequency count, can also do percent
    tab_pivot() %>%
    drop_empty_columns() %>%
    datatable(rownames = FALSE)
  output$biketable <- renderDT({table_crsh})
}
