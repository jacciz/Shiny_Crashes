library(dplyr) # select, filter functions
library(ggplot2) # create pretty graphs
library(DT)    # create pretty tables
library(expss) # format freq tables
library(forcats) # reorder freq in charts
library(plotly) # interactive charts
library(d3heatmap) # makes time of day / week heat chart
library(lubridate) # for dates
# library(ggrepel)  # adjusts labels for ggplots, not for axis

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
# Sidebar Choices. What the user inputs.

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
                    "year", selected = 2019, # default selection
                    choices = c(2020, 2019, 2018, 2017)) #Set years of data

  # Filtered data based on input by the user
  crash_flags_selected <- reactive({# what flags are selected
    flags_selected <- input$crsh_flags # get list of flag
    # flags_selected <- c("Alcohol-related",
    #                     "Drug-related")
    flags_selected <- as.factor(flags_selected)
    flags_selected <-
      relabel(
        flags_selected,
        "Alcohol-related" = "ALCFLAG",
        "Drug-related" = "DRUGFLAG"
        # "Distracted driving" = "DISTFLAG"
      )
  })

  filtered_crashes <- reactive({
    all_crashes %>%
      filter(
        CNTYCODE %in% input$cntynum,
        year(CRSHDATE) %in% input$year
        # CRSHSVR %in% crsh_svr_out - wrong
        # crash_flags_selected()
      )
    # INJSVR %in% input$inj_svr_out
  })
  
  filtered_persons <- reactive({
    all_persons %>%
      filter(
        CNTYCODE %in% input$cntynum,
        year(CRSHDATE) %in% input$year,
        WISINJ %in% input$inj_svr
      )
  })
  
  filtered_vehicles <- reactive({
    all_vehicles <-
      inner_join(all_vehicles, filtered_crashes(), by = "CRSHNMBR") # inner join keeps crashes that match my CRSHNMBR
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
        plot.title = element_text(size = 10, family = "Cambria", face = "plain", color = "white"),
        plot.title.position = "panel",
        legend.text = element_text(size = 8, family = "Cambria", face = "plain", color = "white"),
        legend.title = element_text(size = 8, family = "Cambria", face = "plain", color = "white"),
        axis.text.x = element_text(size = 8, family = "Cambria", face = "plain", color = "white"),
        axis.text.y = element_text(size = 8, family = "Cambria", face = "plain", color = "white"),
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
  
  output$timeofday_heat <- renderD3heatmap({
   
    day_time <- filtered_crashes() %>%
      # apply_labels(CNTYCODE = "County") %>%
      tab_cells(newtime) %>%       # stuff to put in the rows
      # tab_subgroup(ALCFLAG == "Yes") %>%                # only select certain elements
      tab_cols(DAYNMBR) %>%     # columns
      tab_stat_cases() %>% # frequency count, can also do percent
      tab_pivot()
    
    row.names(day_time) <-
      day_time$row_labels # change row names to match row_labels
    
    for (col in 1:ncol(day_time)) { #relabel
      colnames(day_time)[col] <-
        sub("DAYNMBR|", "", colnames(day_time)[col])
    }
    for (row in 1:nrow(day_time)) { #relabel
      rownames(day_time)[row] <-
        sub("newtime|", "", rownames(day_time)[row])
    }
    
    day_time[is.na(day_time)] = 0 #NA will be 0
    
    day_time <-
      day_time[, c("|Sunday",  # reorder columns
                   "|Monday",
                   "|Tuesday",
                   "|Wednesday",
                   "|Thursday",
                   "|Friday",
                   "|Saturday")]
    
    d3heatmap( # output map
      day_time[1:24, 1:7],
      Rowv = FALSE,
      Colv = FALSE,
      colors = "Reds",  # colors
      theme = "dark",
      na.rm = FALSE,
      xaxis_font_size = "10px",
      yaxis_font_size = "10px",
      labCol = c(
        "Sun.",
        "Mon.",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      ),
      labRow = c(
        "12am",
        "1am",
        "2am",
        "3am",
        "4am",
        "5am",
        "6am",
        "7am",
        "8am",
        "9am",
        "10am",
        "11am",
        "12am",
        "1pm",
        "2pm",
        "3pm",
        "4pm",
        "5pm",
        "6pm",
        "7pm",
        "8pm",
        "9pm",
        "10pm",
        "11pm"
      )
    )
  })

  # THIRD row charts
  
  output$mnrcoll <- renderPlotly({
    
    mnr_crashes <- filtered_crashes() %>%
      filter(MNRCOLL != "Unknown")
    
    mnr_crashes$MNRCOLL <-
      fct_infreq(mnr_crashes$MNRCOLL) %>% fct_rev()
    
    max_count = max(table(mnr_crashes$MNRCOLL))
    
    mnrcoll_chart <-
      mnr_crashes %>%
      ggplot(mapping = aes(x = MNRCOLL, y = ..count..)) +
      theme_classic() +
      geom_bar(fill = "#428BCA") +
      # ggtitle("Manner of Collision") +
      theme(
        axis.line = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8, color = "white"),
        axis.title.y = element_blank(),
        plot.title = element_text(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      geom_text(
        stat = 'count',
        color = "#428BCA",
        size = 3,
        aes(label = format(..count.., big.mark=",")),
        fontface = "bold",
        hjust = 0,
        nudge_y = max_count / 20
      ) +
      coord_flip()
    mnrcoll_chart %>% ggplotly()%>%
      layout(margin=list(r=0, l=0, t=0, b=0)) # hoverinfo, can use event_data to update ui data
  })
  
  output$person_role <- renderPlotly({  # have a symbol for each role
    person <- filtered_persons()
    
    person$ROLE <- fct_infreq(person$ROLE) %>% fct_rev() # sorts data
    
    max_count = max(table(person$ROLE))
    
    p_role_chart <- 
      person %>%
      ggplot(mapping = aes(x = ROLE, y = ..count..)) +
      theme_classic() +
      geom_bar(fill = "#428BCA") +
      theme(axis.line=element_blank(),
            legend.position = "none",
            axis.ticks=element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 8, color = "white"),
            axis.title.y = element_blank(),
            plot.title = element_text(),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent")
      ) +
      scale_x_discrete( name = "", labels = function(labels) {  # scatter labels
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      geom_text(
        stat = 'count',
        color = "#428BCA",
        size = 3,
        aes(label = format(..count.., big.mark=",")),
        fontface = "bold",
        hjust = 0,
        nudge_y = max_count / 14
      ) +
      coord_flip()
    
    p_role_chart %>% ggplotly() %>%
      layout(margin=list(r=0, l=0, t=0, b=0)) # hoverinfo, can use event_data to update ui data text
    
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
        axis.text.x = element_text(size = 8, color = "white"),
        axis.text.y = element_text(size = 8, color = "white"),
        legend.text = element_text(size = 8, color = "white"),
        legend.title = element_text(size = 8, color = "white"),
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
        # na.translate = FALSE,
        # limits = c("Female", "Male", "Unknown") # do not work
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