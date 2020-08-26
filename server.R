library(dplyr) # select, filter functions
library(tidyr) # pivot_longer
library(stringr) # str_wrap
# library(ggplot2) # create pretty graphs
library(DT)    # create pretty tables
# library(expss) # format freq tables, tab_cells
# library(forcats) # reorder freq in charts
library(plotly) # interactive charts
library(lubridate) # for dates
library(leaflet) # the map
library(leaflet.extras2) # hexbin
library(data.table) # setnames function, data format for large data
library(tibble) # quick data frames

# src("https://unpkg.com/ionicons@5.0.0/dist/ionicons.js") # for icons ?? didnt work
# shinytest::recordTest("C:/W_shortcut/Shiny_Crashes_Dashboard/") test for bugs

# run this code in the console to see performance (total time is 10230, app start 2880) now 9170 & 790
# profvis::profvis({ shiny::runApp('C:/W_shortcut/Shiny_Crashes_Dashboard/') })
# shinyloadtest::record_session(shiny::runApp('C:/W_shortcut/Shiny_Crashes_Dashboard/'), output_file = "C:/W_shortcut/recording.log")
# shinyloadtest::record_session("http://127.0.0.1:3184") # run with Docker

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
                    "cntynum", # selected = 13, # default selection
                    choices = setNames(county_recode$CountyCode, county_recode$CountyName))

  observeEvent(input$cntynum, { # choose municipality

    muni_cnty_list <- muni_recode %>% filter(CntyCode %in% input$cntynum)

  updateSelectInput(session, "muni_names",
                      choices = setNames(muni_cnty_list$MuniCode, muni_cnty_list$Municipality_CTV) )
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
  
  filtered_crsh_flags <- # this decides whether to return all or any crash flags, returns only CRSHNMBR
    reactive({
      crshflag_list = get_crshflag_list() # the if/else determines any or all selection
      if (input$any_or_all) {
        # default for this button is 'any'
        # selects crash flags, goes through each row and finds all Y
        return(all_crsh_flags[apply(all_crsh_flags [, ..crshflag_list], 1, function(x)
          any(x == "Y")), ] %>% dplyr::filter(!is.na(CRSHNMBR))) # all() so ALL flags are selected
      }
      # selects crash flags, goes through each row and finds all Y
      return(all_crsh_flags[apply(all_crsh_flags [, ..crshflag_list], 1, function(x)
        all(x == "Y")), ] %>% dplyr::filter(!is.na(CRSHNMBR))) # all() so ALL flags are selected
    })
  
# portage should be 49
  selected_county <- reactive({ # this takes the selected county and zooms to it
    sel_county <- county %>% filter(DNR_CNTY_C %in% input$cntynum) #COUNTY_NAM
    bbox <- st_bbox(sel_county) %>% as.vector()
    bbox
    # print(sel_county)
    # print(bbox)
  })
  ################### DATA OBSERVE EVENTS OF DATA #######################
  
# filtered_crsh_flags <- reactive({ # returns crash numbers of crash flags selected
#   # what flags are selected - this is set to AND
#   rename_crsh_flags <- # rename inputs so we can select flag columns
#     c("Alcohol-related" = "ALCFLAG",
#       "Drug-related" = "DRUGFLAG",
#       # "Distracted driving",  # don't have this one, also CMV
#       'Speeding' = 'speedflag',
#       'Teen driver (Age 16-19)' = 'teenflag',
#       'Older driver (Age 65+)' = 'olderflag',
#       "Bicyclist" = "BIKEFLAG",
#       "Pedestrian" = "PEDFLAG",
#       "Motorcycle" = "CYCLFLAG"
#     )
#   new_crsh_flags <-
#     rename_crsh_flags[input$crsh_flags] # apply the rename to get a list
#   # crsh_flags <- c("Alcohol-related", "Speeding")
#   
#   selected_crash_flags <-
#     # selects crash flags, goes through each row and finds all Y
#     all_crsh_flags[apply(all_crsh_flags [, ..new_crsh_flags], 1, function(x)
#       all(x == "Y")),] %>% dplyr::filter(!is.na(CRSHNMBR)) # all() so ALL flags are selected
#   selected_crash_flags # returns datatable with crshnmbr to match
# })

filtered_crashes <- # returns crash data, depends if a flag was selected
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
                  CRSHSVR %in% crshsvr_selected() & CRSHDATE %within% yearrange]
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
                  CRSHSVR %in% crshsvr_selected() & CRSHDATE %within% yearrange]
  filter_persons
})

filtered_vehicles <- reactive({ # joins with the already filtered_crashes
  all_vehicles <-
    inner_join(all_vehicles, filtered_crashes(), by = "CRSHNMBR") # inner join keeps crashes that match my CRSHNMBR
})

filtered_crash_lat_long <- reactive({  # get lat longs for map
  crash_lat_long_j <-
    filtered_crashes()[, .(lng, lat, CRSHSVR)] %>% na.omit() # remove crashes with no lat/long
})

################### VALUE BOXES #######################
  output$tot_crash <- renderInfoBox({
    valueBox(
      format(nrow(filtered_crashes()), big.mark = ","),
      "Total Crashes",
      icon = icon("car-alt"),
      color = "red"
    )
  })
  output$tot_inj <- renderInfoBox({
    valueBox(
      filtered_crashes() %>% summarise(x = format(sum(TOTINJ), big.mark = ",")),
      "Total Injuries",
      icon = icon("first-aid"),
      color = "red"
    )
  })
  output$tot_fatal <- renderInfoBox({
    valueBox(
      filtered_crashes() %>% summarise(x = sum(TOTFATL)) %>% format(big.mark = ","),
      "Total Fatalities",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  ################### BODY - CHARTS #######################
  chart_title = list(size = 16, color = "rgb(243,243,245)", family = "Arial")
  chart_axis = list(size = 14, color = "rgb(205,205,205)", family = "Arial")
  chart_axis_bar = list(size = 14, color = "#428BCA", family = "Arial", face = "bold")
  
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
        factor(crshsvr_table$month, levels = month.name) # factors month names, in month.name order
    
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") # rename months
    crshsvr_table$month <- month.abb[crshsvr_table$month] # abbreviate months
      # month_factor = month.name
      
    # assigning colors
    color_map <- c("Fatal"="#D50032", "Injury"="#428BCA", "Property Damage"="#4DB848")
      
    plot_ly(
      crshsvr_table, 
      type = 'bar',
      x = ~ month,
      y = ~ n,
      color = ~svr,
      colors = ~color_map[svr], # assign colors, this will give a warning 'Duplicate levels detected'
      hovertemplate = paste('%{x}<br>',
                            '<b>%{y: .0f} Crashes')
    ) %>% #Price: %{y:$.2f}<extra></extra>
      layout(
        title = list(text ="Crash Severity by Month", font = chart_title, y = 1, x = 0),
        legend = list(
          x = .5,
          y = 1.2,
          orientation = 'h',
          font = chart_axis,
          traceorder = "normal" # alphabetical legend order
        ),
        margin = list(r = 0, l = 0, b = 0, t = 45
        ),
        xaxis = list(
          title = "",
          tickfont = chart_axis,
          tickangle = -45,
          categoryarray = ~month_order, categoryorder = "array" # sets order
          # ticktext = ~month.abb[crshsvr_table$month],
          # automargin = TRUE,
          # dtick = 5 # every 5 months are labeled
        ),
        yaxis = list(
          showgrid = FALSE,
          tickfont = chart_axis,
          title = ""
        ),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)',
        barmode = 'stack'
      ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Crash Severity by Month", scale = 2)
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
      ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Time of Day Crashes", scale = 2)
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
        hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
        text = ~ format(n, big.mark=","), # bar end number
        textfont = chart_axis_bar,
        textposition = 'outside',
        cliponaxis = FALSE
      ) %>%
        layout(
          title = list(text ="Manner of Collision", font = chart_title, x = 0),
          margin = list(
            r = 40, # set to 40 so labels don't get cut off
            l = 200, # so axis label don't get cut off
            # t = 0, # this will cut off title
            b = 0
          ),
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE # remove axis labels
          ),
          yaxis = list(title = "", tickfont = chart_axis),
          plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Manner of Collision", scale = 2)
        )
      }
  })

  output$person_role_treemap <- renderPlotly({
    
    if (dim(filtered_persons())[1] == 0) { # or no crashes with a time ??
      plotly_empty(type = "treemap") %>% layout(
        title = list(text ="Role of All Persons", font = chart_title, x = 0),
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
      values = ~n,
      textinfo="label+value+percent parent+percent"
    ) %>% 
      layout(
        title = list(text ="Role of All Persons", font = chart_title, y = 1, x = 0),
        margin = list(r = 0, l = 0, b = 10, t = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Role of All Persons", scale = 2)
      )
    }
  })

  output$person_age_gender <- renderPlotly({
    
    if (dim(filtered_persons())[1] == 0) { # or no crashes with a time ??
      plotly_empty(type = "bar") %>% layout(
        title = list(text ="Age and Gender of All Persons", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
    
    person <-
      filtered_persons()[, .(age_group, SEX)]
    
    age_sex_table <- table(age = person$age_group, sex = person$SEX) %>% as_tibble() # get counts, put in a tibble
    
    color_map <- c("Female"="#D50032", "Male"="#428BCA", "Unknown" = "#F9C218")
    
    plot_ly(
      age_sex_table,
      type = 'bar',
      x = ~ age,
      y = ~ n,
      color = ~ sex,
      colors = ~color_map[sex],
      hovertemplate = paste('<br>Age %{x}<br>',
                            '<b>%{y: .0f} people<b>')
    ) %>%
      layout(
        title = list(text ="Age and Gender of All Persons", font = chart_title, y = 1, x = 0),
        legend = list(x = .5, y = 1.2, orientation = 'h', font = chart_axis),
        margin = list(
          r = 0,
          l = 0,
          b = 0,
          t = 45
        ),
        xaxis = list(title = "", tickfont = chart_axis, tickangle = -45, categoryarray = ~age, categoryorder = "array"),
        yaxis = list(title = "", showgrid = FALSE, tickfont = chart_axis),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)',
        barmode = 'stack'
      ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Age and Gender of All Persons", scale = 2)
      )
        # labels = function(labels) { # this scatters labels so they fit on two lines
        #   sapply(seq_along(labels), function(i)
        #     paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        # }
    }
   })
  
  output$drvrpc_chart <- renderPlotly({
    
    if (dim(filtered_persons())[1] == 0) {
      plotly_empty(type = "bar") %>% layout(
        title = list(text ="No Driver Contributing Circumstances Found", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
      drvrpc <- filtered_persons() %>%
        select(DRVRPC01:DRVRPC24) %>% pivot_longer(DRVRPC01:DRVRPC24) %>% filter(value !='')
      # make freq table, remove variables, arrange and take top 8
      drvrpc_table <- table(drvrpc_count = drvrpc$value) %>% as_tibble() %>%
        filter(drvrpc_count != "No Contributing Action",
               drvrpc_count != "Unknown") %>% arrange(desc(n)) %>% head(., 8)
    #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
      drvrpc_table$drvrpc_count <- reorder(drvrpc_table$drvrpc_count, drvrpc_table$n)  # reorder from big to small values
      plot_ly(
        drvrpc_table,
        type = 'bar',
        orientation = 'h',
        x = ~ n,
        y = ~ reorder(drvrpc_count, n), # reorder from big to small values, also wrap text
        marker = list(color = "#428BCA"), # blue!
        hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
        text = ~ format(n, big.mark=","), # bar end number
        textfont = chart_axis_bar,
        textposition = 'outside',
        cliponaxis = FALSE
      ) %>% #labels = function(x) str_wrap(drvrpc_count, width = 15)
        layout(
          title = list(text ="Top Driver Contributing Circumstance", font = chart_title, x = 0),
          margin = list(
            r = 35, # set to 30 so labels don't get cut off
            l = 200, # so axis label don't get cut off
            # t = 0, # this will cut off title
            b = 0
          ),
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE # remove axis labels
          ),
          yaxis = list(title = "", tickfont = chart_axis),
          plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Driver Contributing Circumstance", scale = 2)
        )
    }
    })
  output$nmtact_chart <- renderPlotly({
    
    if (dim(filtered_persons() %>% select(NMTACT01:NMTACT12) %>% pivot_longer(NMTACT01:NMTACT12)
            %>% filter(value != "Unknown", value !=''))[1] == 0) {
      plotly_empty(type = "bar") %>% layout(
        title = list(text ="No Pedestrians or Cyclists", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
      nmtact <- filtered_persons() %>%
        select(NMTACT01:NMTACT12) %>% pivot_longer(NMTACT01:NMTACT12) %>% filter(value !='')
      # make freq table, remove variables, arrange and take top 8
      nmtact_table <- table(nmtact_count = nmtact$value) %>% as_tibble() %>%
        filter(
              # nmtact_count != "No Improper Action",
               nmtact_count != "Unknown") %>%
        arrange(desc(n)) %>% head(., 8)
      #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
      plot_ly(
        nmtact_table,
        type = 'bar',
        orientation = 'h',
        x = ~ n,
        y = ~ reorder(nmtact_count, n), # reorder from big to small values, also wrap text
        marker = list(color = "#428BCA"), # blue!
        hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
        text = ~ format(n, big.mark=","), # bar end number
        textfont = chart_axis_bar,
        textposition = 'outside',
        cliponaxis = FALSE
      ) %>% #labels = function(x) str_wrap(drvrpc_count, width = 15)
        layout(
          title = list(text ="Top Actions of Pedestrians and Cyclists", font = chart_title, x = 0),
          margin = list(
            r = 40, # set to 30 so labels don't get cut off
            l = 200, # so axis label don't get cut off
            # t = 0, # this will cut off title
            b = 0
          ),
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE # remove axis labels
          ),
          yaxis = list(title = "", tickfont = chart_axis),
          plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Actions of Pedestrians and Cyclists", scale = 2)
        )
    }
  })
  output$nmtloc_chart <- renderPlotly({
    
    if (dim(filtered_persons() %>% filter(NMTLOC!=''))[1] == 0) {
      plotly_empty(type = "bar") %>% layout(
        title = list(text ="No Pedestrians or Cyclists", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
      # nmtloc <- filtered_persons() %>%
        # select(NMTACT01:NMTACT12) %>% pivot_longer(NMTACT01:NMTACT12) %>% filter(value !='')
      # make freq table, remove variables, arrange and take top 8
      nmtloc_table <- table(nmtloc_count = filtered_persons()$NMTLOC) %>% as_tibble() %>%
        filter(nmtloc_count !='') %>% 
        # filter(nmtact_count != "No Improper Action",
               # nmtact_count != "Unknown") %>%
        arrange(desc(n)) %>% head(., 8) 
      #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
      plot_ly(
        nmtloc_table,
        type = 'bar',
        orientation = 'h',
        x = ~ n,
        y = ~ reorder(nmtloc_count, n), # reorder from big to small values, also wrap text
        marker = list(color = "#428BCA"), # blue!
        hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
        text = ~ format(n, big.mark=","), # bar end number
        textfont = chart_axis_bar,
        textposition = 'outside',
        cliponaxis = FALSE
      ) %>% #labels = function(x) str_wrap(drvrpc_count, width = 15)
        layout(
          title = list(text ="Top Locations of Pedestrians and Cyclists", font = chart_title, x = 0),
          margin = list(
            r = 40, # set to 30 so labels don't get cut off
            l = 200, # so axis label don't get cut off
            # t = 0, # this will cut off title
            b = 0
          ),
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE # remove axis labels
          ),
          yaxis = list(title = "", tickfont = chart_axis),
          plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Locations of Pedestrians and Cyclists", scale = 2)
        )
    }
  })
  output$vehicle_treemap <- renderPlotly({
    
    if (dim(filtered_vehicles())[1] == 0) {
      plotly_empty(type = "treemap") %>% layout(
        title = list(text ="All Vehicles Involved", font = chart_title, x = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    } else {
    newcar <- filtered_vehicles() %>% # put this is data import  # vehcate
      mutate(
        cate = case_when(
          VEHTYPE == "Passenger Car" ~ "Passenger Vehicle",
          VEHTYPE == "(Sport) Utility Vehicle" ~ "Passenger Vehicle",
          VEHTYPE == "Cargo Van (10,000 Lbs or Less)" ~ "Passenger Vehicle",
          VEHTYPE == "Passenger Van" ~ "Passenger Vehicle",
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
        title = list(text ="All Vehicles Involved", font = chart_title, y = 1, x = 0),
        margin = list(r = 0, l = 0, b = 0, t = 0),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      )  %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "All Vehicles Involved", scale = 2)
      )
    }
  })
  
  ################### BODY - MAP #######################
  
  # odd issue with asynchronous data loading, could use renderUI so map gets updated based on user inputs
  # -> https://github.com/rstudio/leaflet/issues/151  https://github.com/rstudio/leaflet/issues/448
  
  # observeEvent(input$my_easy_button, { #print status
  #   if (input$my_easy_button == 'no cluster'){ # this line works
  #     shinyjs::enable("my_easy_button", shinyjs::toggle("no cluster"))
  #   }
  # })
    # str(input$my_easy_button)
  # if ((input$my_easy_button) == 'no cluster') {
  #   str("YAY")
  #   leafletProxy("map1") %>% clearMarkerClusters()
  # htmlwidgets::onRender("function(el, x) {
  #         var clusterManager =
  #           map.layerManager.getLayer('cluster', 'crashCluster');
  #         clusterManager.disableClustering();}")
  
  # shinyjs::toggleState("decluster-markers", JS("
  #       function(btn, map1) {
  #         var clusterManager =
  #           map.layerManager.getLayer('cluster', 'crashCluster');
  #         clusterManager.disableClustering();"))
  # }

  output$map1 <- renderLeaflet({ #render basic map, pretty much items that do not need a reactive
      leaflet() %>% addTiles() %>%
      addPolygons(
        data = county$geometry,
        group = "Counties",
        color = "#444444",
        fillOpacity = 0,
        weight = 1,
        smoothFactor = 0.5
        # options = pathOptions(clickable = FALSE)
      ) %>%
      # change in easybutton state https://stackoverflow.com/questions/60120184/shiny-leaflet-easybutton-only-fires-once
      addEasyButton(easyButton(
        states = list(
          easyButtonState( 
            stateName="decluster-markers",
            icon = tags$span(HTML('</svg><image class="cluster_on_svg" src="icons8-connect-filled.svg" />')),  #"ion-toggle-filled", # icon("artificial-intelligence", lib = "glyphicon"),
            title="Disable Clustering",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'crashCluster');
            clusterManager.disableClustering();
            btn.state('cluster-markers');
            Shiny.onInputChange('my_easy_button', 'no cluster');
          }")
          ),
          easyButtonState(
            stateName="cluster-markers",
            icon=tags$span(HTML('</svg><image class="cluster_off_svg" src="icons8-connect.svg" />')),
            title="Enable Clustering",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'crashCluster');
            clusterManager.enableClustering();
            btn.state('decluster-markers');
            Shiny.onInputChange('my_easy_button', 'cluster');
          }")
          )
        )
      ))
  })

  ## MUST RESET TOGGLE BUTTON, observe if btn is toggled, if-else
  observeEvent(input$cntynum, { # change view if location selected changes
    county_zoom <- selected_county()
    # print(selected_county())
    leafletProxy("map1") %>%
     fitBounds(county_zoom[1], county_zoom[2], county_zoom[3], county_zoom[4]) # zoom to selected county
    # update btn / toggle ??
    })
 
  observeEvent(filtered_crashes(), { # same view, updates map data if selection changes
    filtered_crash_with_icons <- filtered_crash_lat_long() %>% # create a dataframe with a column to specify icon names
      mutate(crash_icon = case_when( # For the crash icons
        CRSHSVR == 'Fatal' ~ "fatal",
        CRSHSVR == 'Injury' ~ "injury",
        CRSHSVR == 'Property Damage' ~ "property"))
 
    crshIcons <- awesomeIconList( # icon list
      fatal = makeAwesomeIcon(icon = "close-circle", library = "ion",
                              markerColor = "red"),
      injury = makeAwesomeIcon(icon = "person", library = "ion",
                              markerColor = "blue"),
      property = makeAwesomeIcon(icon = "car", library = "ion",
                               markerColor = "green")
    )
    # crshIcons2 <- iconList( # icon list, dont like these
    #   fatal = makeIcon("icons/skull.png", iconWidth = 20, iconHeight = 20),
    #   injury = makeIcon("icons/user-injured-person.png", iconWidth = 20, iconHeight = 20),
    #   property = makeIcon("icons/car-crash-solid.png", iconWidth = 20, iconHeight = 20)
    # )
    
    # "button-state state-cluster-markers cluster-markers-active"
    # if ("button-state state-cluster-markers cluster-markers-active")
    
    # Clear map so we can add new stuff
    leafletProxy("map1") %>%
      clearMarkers() %>% clearMarkerClusters() %>%

    # issue with fa icons breaking, use different icon library https://github.com/rstudio/shinydashboard/issues/339
    # to add legend https://stackoverflow.com/questions/47064921/leaflet-legend-for-addawesomemarkers-function-with-icons

    addAwesomeMarkers(
      clusterOptions = markerClusterOptions(maxClusterRadius = 30), clusterId = "crashCluster",
      group = "Crashes",
      lng = filtered_crash_with_icons$lng,
      lat = filtered_crash_with_icons$lat,
      icon = crshIcons[filtered_crash_with_icons$crash_icon]
    ) %>%
      addLayersControl(
        overlayGroups = c(
          "Crashes"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({ # observe when hexsize changes or if hex is checked
    if (input$hex & input$hexsize) {
      leafletProxy("map1", data = filtered_crash_lat_long()) %>%
        clearHexbin() %>%
        addHexbin(
          lng = filtered_crash_lat_long()$lng,
          lat = filtered_crash_lat_long()$lat,
          radius = input$hexsize,
          opacity = 0.8,
          options = hexbinOptions(
            colorRange = c("#b0d0f2", "#05366b"),#c("#fee0d2", "#de2d26"), # red #c("#b0d0f2", "#05366b"), #blue    c("#99d899", "#005100") green
            resizetoCount = TRUE,
            radiusRange = c(input$hexsize, input$hexsize), # same size, must match radius
            tooltip = "Crashes: "
          )
        )
    } else { # remove hex if unchecked
      leafletProxy("map1") %>% clearHexbin() }
  })
}
