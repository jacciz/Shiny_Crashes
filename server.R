library(dplyr) # select, filter functions
library(ggplot2) # create pretty graphs
library(DT)    # create pretty tables
library(expss) # format freq tables
library(forcats) # reorder freq in charts
library(plotly) # interactive charts
library(d3heatmap) # makes time of day / week heat chart
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

  # copied from https://gist.github.com/helgasoft/799fac40f6fa2561c61cd1404521573a
  # hexBin <- htmltools::htmlDependency(
  #   name = 'hexBin',
  #   version = "1.0",
  #   # (1) works in R but not in Shiny due to async loading, see https://github.com/rstudio/shiny/issues/1389
  #   # src = c(href = 'https://cdn.statically.io/gh/dayjournal/Leaflet.Control.Opacity/master/dist/'),
  #   # (2) works in R and Shiny - download js/css files, then use this:
  #   src = 'C:/W_shortcut/Shiny_Crashes_Dashboard/js',
  #   script = c("hexbin.js", "deps.js"),
  #   stylesheet = "hexbin.css"
  # )
  # registerPlugin <- function(map, plugin) {
  #   map$dependencies <- c(map$dependencies, list(plugin))
  #   map
  # }

# Sidebar Choices. What the user inputs.
#
# hex_plugin <-  # add hex from leafthehex
#   pluginFactory( #in Chrome, disable JS source maps, enable CSS maps
#     "Hex", # name
#     "./js", # path #W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/
#     "hexbin.js",
#     "deps.js",
#     "hexbin.css")

  # hex_plugin <-  # add hex from leafthehex
  #   pluginFactory( #in Chrome, disable JS source maps, enable CSS maps
  #     "Hex", # name
  #     "C:/Users/dotjaz/Documents/R/win-library/4.0/leaflethex/js", # path #W:/HSSA/Keep/Jaclyn Ziebert/R/Shiny_Crashes_Dashboard/
  #     "hexbin.js",
  #     "deps.js",
  #     "hexbin.css")

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
            # "Distracted driving",  # don't have this one
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

    # labels = c("Jan.", "Feb.") #could write a list of abbreviations for chart

    crsh_svr_mth_chart <- filtered_crashes() %>% ggplot(mapping = aes(CRSHMTH)) +
      theme_classic() +
      geom_bar(aes(fill=CRSHSVR)) +
      # ggtitle("Crash Severity by Month") +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        # plot.title = element_text(size = 6, family = "Cambria", face = "plain", color = "white"),
        plot.title.position = "panel",
        legend.text = element_text(size = 6, family = "Cambria", face = "plain", color = "white"),
        legend.title = element_text(size = 6, family = "Cambria", face = "plain", color = "white"),
        axis.text.x = element_text(size = 6, family = "Cambria", face = "plain", color = "white"),
        axis.text.y = element_text(size = 6, family = "Cambria", face = "plain", color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background =element_rect(fill = "transparent", color = NA)
      ) +
      scale_x_discrete(limits = month.name, name = "", labels = function(labels) {  # scatter labels
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}
                         ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
    scale_fill_manual(
      name = "", #Crash Severity no legend title
      values = c("#D50032", "#428BCA", "#4DB848"))

  crsh_svr_mth_chart %>% ggplotly() %>% layout(legend = list(x = 0.5, y = 100, orientation = 'h'))%>%
    layout(margin=list(r=0, l=0, t=0, b=0)) # hoverinfo, can use event_data to update ui data
  })
  # , bg="transparent"

  output$timeofday_heat <- renderPlotly({
    day_time <- filtered_crashes() %>%
      group_by(newtime, DAYNMBR) %>%
      summarise(n = n()) %>%
      filter(newtime != '') %>%
      tidyr::spread(DAYNMBR, n, fill = 0)
    
    day_time <-
      day_time[, c(
        "newtime",
        "Sunday",
        # reorder columns
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )]
    names(day_time) <-
      c("newtime", "Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat") # rename columns
    m <- day_time[, 2:8] %>% as.matrix()
    
    
    # get blue colors
    vals <- unique(scales::rescale(m))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Blues", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    
    
    plot_ly(
      x = colnames(day_time[2:8]),
      y = day_time$newtime,
      z = m,
      type = "heatmap",
      colorscale = colz
    )

    # day_time <- filtered_crashes() %>%
    #   # apply_labels(CNTYCODE = "County") %>%
    #   tab_cells(newtime) %>%       # stuff to put in the rows
    #   # tab_subgroup(ALCFLAG == "Yes") %>%                # only select certain elements
    #   tab_cols(DAYNMBR) %>%     # columns
    #   tab_stat_cases() %>% # frequency count, can also do percent
    #   tab_pivot()
    # 
    # row.names(day_time) <-
    #   day_time$row_labels # change row names to match row_labels
    # 
    # for (col in 1:ncol(day_time)) { #relabel
    #   colnames(day_time)[col] <-
    #     sub("DAYNMBR|", "", colnames(day_time)[col])
    # }
    # for (row in 1:nrow(day_time)) { #relabel
    #   rownames(day_time)[row] <-
    #     sub("newtime|", "", rownames(day_time)[row])
    # }
    # 
    # day_time[is.na(day_time)] = 0 #NA will be 0
    # 
    # day_time <-
    #   day_time[, c("|Sunday",  # reorder columns
    #                "|Monday",
    #                "|Tuesday",
    #                "|Wednesday",
    #                "|Thursday",
    #                "|Friday",
    #                "|Saturday")]
    # 
    # d3heatmap( # output map
    #   day_time[1:24, 1:7],
    #   Rowv = FALSE,
    #   Colv = FALSE,
    #   colors = "Reds",  # colors
    #   theme = "dark",
    #   na.rm = FALSE,
    #   xaxis_font_size = "10px",
    #   yaxis_font_size = "10px",
    #   labCol = c(
    #     "Sun.",
    #     "Mon.",
    #     "Tuesday",
    #     "Wednesday",
    #     "Thursday",
    #     "Friday",
    #     "Saturday"
    #   ),
    #   labRow = c(
    #     "12am",
    #     "1am",
    #     "2am",
    #     "3am",
    #     "4am",
    #     "5am",
    #     "6am",
    #     "7am",
    #     "8am",
    #     "9am",
    #     "10am",
    #     "11am",
    #     "12am",
    #     "1pm",
    #     "2pm",
    #     "3pm",
    #     "4pm",
    #     "5pm",
    #     "6pm",
    #     "7pm",
    #     "8pm",
    #     "9pm",
    #     "10pm",
    #     "11pm"
    #   )
    # )
  })
  
  # THIRD row charts

  output$mnrcoll <- renderPlotly({

    mnr_crashes <- filtered_crashes() %>%
      filter(MNRCOLL != "Unknown")

    mnr_crashes$MNRCOLL <-
      fct_infreq(mnr_crashes$MNRCOLL) %>% fct_rev()

    # max_count = max(table(mnr_crashes$MNRCOLL))

    mnrcoll_chart <-
      mnr_crashes %>%
      ggplot(mapping = aes(x = MNRCOLL)) +
      theme_classic() +
      geom_bar(fill = "#428BCA", position = 'dodge', stat = 'count') +
      # ggtitle("Manner of Collision") +
      theme(
        axis.line = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 6, color = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      ylim(c(0, max(table(mnr_crashes$MNRCOLL))*1.05)) +  # finds max count so labels don't get cut off
      geom_text(
        stat = 'count',
        color = "white", #428BCA
        size = 3,
        aes(label = format(..count.., big.mark=",")),
        fontface = "bold",
        hjust = 0
        # nudge_y = max_count / 20
      ) +
      coord_flip()
    mnrcoll_chart %>% ggplotly()%>%
      layout(margin=list(r=0, l=0, t=0, b=0)) # hoverinfo, can use event_data to update ui data
  })

  output$person_role <- renderPlotly({  # have a symbol for each role
    person <- filtered_persons()

    
    role_table <- table(role = person$ROLE) %>% as_tibble()
    # role_table$role <- fct_infreq(role_table$role) %>% fct_rev() # sorts data
    # max_count = max(table(person$ROLE))
    
    
    plot_ly(
      role_table,
      type = 'bar',
      orientation = 'h',
      x = ~ n,
      y = ~ reorder(role, n), # reorder from big to small values
      name = "",
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
      # add_annotations(text = ~n, textposition = "outside", font = list(family = 'Cambria', size = 12, color = 'red')) %>%
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
        yaxis = list(title = "", tickfont = list(size = 10, color = "white") ),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )

    # p_role_chart <-
    #   role_table %>%
    #   ggplot(mapping = aes(reorder(role, desc(-n)), n)) + # values to display, reorder so highest value is on top
    #   theme_classic() +
    #   geom_bar(aes(fill = "#428BCA"), stat = 'identity') +
    #   theme(axis.line=element_blank(),
    #         legend.position = "none",
    #         axis.ticks=element_blank(),
    #         axis.text.x = element_blank(),
    #         axis.text.y = element_text(size = 6, color = "white"),
    #         axis.title.y = element_blank(),
    #         axis.title.x = element_blank(),
    #         plot.title = element_text(),
    #         plot.background = element_rect(fill = "transparent", colour = NA),
    #         panel.background = element_rect(fill = "transparent")
    #   ) +
    #   # scale_x_discrete( name = "", labels = function(labels) {  # scatter labels
    #     # sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}) +
    #   scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
    #   # ylim(c(0, max(table(person$ROLE))*1.05)) +  # finds max count so labels don't get cut off
    #   # geom_text(
    #     # stat = 'count',
    #     # color = "white", ##428BCA
    #     # size = 3,
    #     # aes(label = format(..count.., big.mark=",")),
    #     # fontface = "bold",
    #     # hjust = -.5 #-0.6
    #     # nudge_y = max_count / 14
    #   # ) +
    #   coord_flip()
    # 
    # p_role_chart %>% ggplotly() %>%
    #   layout(margin=list(r=0, l=0, t=0, b=0)) %>% # hoverinfo, can use event_data to update ui data text
    #     style(text =  n, textposition = "outside", textfont = list(size= 10, color = "white")) # bar percents style

  })

  output$person_age_gender <- renderPlotly({
    person <-
      filtered_persons() %>% select(age_group, SEX) %>% na.omit()

    label_age <- c( "0-4","5-9","10-14","15-19","20-24","25-29","30-34", # this labels x-axis
      "35-39", "40-44","45-49","50-54","55-59","60-64","65-69","70+")
    cols <- c("F" = "#D50032", "M" = "#428BCA", "U" = "#F9C218") # colors for gender

    p_age_gender_chart <-
      person %>%
      ggplot(mapping = aes(x = age_group, fill = SEX)) +
      theme_classic() +
      geom_bar() +
      theme(
        axis.line = element_blank(),
        legend.justification=c(1,0),
        legend.position = "top",
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 6, color = "white"),
        axis.text.y = element_text(size = 6, color = "white"),
        legend.text = element_text(size = 6, color = "white"),
        legend.title = element_text(size = 6, color = "white"),
        axis.title.y = element_blank(),
        plot.title = element_text(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      scale_x_discrete(
        name = "", limits = label_age,
        labels = function(labels) {
          # scatter labels
          sapply(seq_along(labels), function(i)
            paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        }
      ) +
      scale_fill_manual(
        # name = "Gender",
        # values = c("#D50032", "#428BCA", "#F9C218"),
        values = c("Female" = "#D50032", "Male" = "#428BCA", "Unknown" = "#F9C218")
        # aesthetics = c("colour", "fill")
        # na.translate = FALSE
      )
    # coord_flip()

    p_age_gender_chart %>% ggplotly() %>% layout(
      legend = list(x = 0, y = 100, orientation = 'h'), # horizontal legend, on top of chart
      margin = list( # no margins
        r = 0,
        l = 0,
        t = 0,
        b = 0
      )
    )
  })

  # output$map_crash <-  # this is the map - using Leaflet, can also do clusterOptions = markerClusterOptions()
  #   renderLeaflet({
  #
  #   filtered_crash_lat_long() %>%
  #    leaflet() %>% addTiles() %>% addCircles() %>% addHexbin(lowEndColor='green', highEndColor='red',  uniformSize = TRUE, radius = 25)
  #     # registerPlugin(hexBin) %>%
  #     # onRender("function(el, x) { L.HexbinLayer({}).addTo(this);}")
  #       #newHex()
  #   })


  # odd issue with asynchronous data loading, need to renderUI so map gets updated based on user inputs
  # -> https://github.com/rstudio/leaflet/issues/418

  # observeEvent(input$map_btn, {
  #   id <- paste0("map_", input$map_btn)
  #   output[[id]] <- renderLeaflet({
  #     l <- filtered_crash_lat_long() %>%
  #          leaflet() %>% addTiles() %>% addCircles() %>% addHexbin()
  #     l
  #   })
  #   output$map <- renderUI({
  #     leafletOutput(id)
  #   })
  # })

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
        # addHexbin(
        #   lng = tomap$lng,
        #   lat = tomap$lat,
        #   radius = 5,
        #   group = "Hex analysis",
        #   layerId = "Hex",
        #   opacity = 0.8,
        #   options = hexbinOptions(
        #     colorRange = c("#b0d0f2", "#05366b"),
        #     resizetoCount = TRUE,
        #     radiusRange = c(5, 5) # same size, must match radius
        #   )
        # ) %>% 
      addLayersControl(
        overlayGroups = c(
          "Crashes"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) 
      mymap
    })
    observeEvent(input$hexize, { # observe if hexsize has been changed
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
  # observeEvent(input$addhex, { # reactive to when hex check box changes by user
  #   cmv_latlong <- cmv_latlong_data()
  #   if (input$addhex) {
  #     leafletProxy("map") %>% 
  #       # clearHexbin() %>%
  #       addHexbin(
  #         lng = cmv_latlong$LONDECDG,
  #         lat = cmv_latlong$LATDECDG,
  #         radius = input$hexsize,
  #         opacity = 0.8,
  #         options = hexbinOptions(
  #           colorRange = c("#99d899", "#005100"),
  #           # blue c("#b0d0f2", "#05366b"),
  #           resizetoCount = TRUE,
  #           radiusRange = c(input$hexsize, input$hexsize) # same size, must match radius
  #         )) }
  #   else {
  #     leafletProxy("map") %>%
  #       hideHexbin()
  #   }
  # })
  #                                                                               TABLES
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
