library(dplyr) # select, filter functions
library(ggplot2) # make pretty graphs
library(DT)    # create pretty tables
library(expss) # format freq tables
library(forcats) # reorder freq in charts
library(plotly) # interactive charts
library(d3heatmap)

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
#                                                       INPUTS
  
  # accidentData%>%filter(
  #   Local_Authority_.District. %in% input$district &
  #     Year %in% input$year &
  #     Day_of_Week %in% input$day)

  tot_crash_num <- reactive({
    all_crashes %>%
      filter(CNTYCODE == input$cntynum)
  })
  # all_crashes %>% rename(m_names = MUNICODE)
  
  updatePickerInput(session,
                    "cntynum",
                    choices = setNames(county_recode$CountyCode, county_recode$CountyName))
  
  observeEvent(input$cntynum, {

    muni_cnty_list <- muni_recode %>% filter(CntyCode == input$cntynum)  #input$cntynum
    
    updatePickerInput(session,
                      "muni_names",
                      choices = setNames(muni_cnty_list$MuniCode, muni_cnty_list$Municipality_CTV) )
  }) 
  # setNames(muni_recode$MuniCode, muni_recode$Municipality) # set above to see names
#                                                                               TABLES
  output$biketable <- renderDT({
    all_crashes %>% 
      tab_cells(CNTYCODE) %>%                           # stuff to put in the rows
      tab_subgroup(ALCFLAG == "Yes") %>%                # only select certain elements
      tab_cols(CRSHSVR %nest% ALCFLAG, total()) %>%     # columns with nesting
      tab_stat_cases(total_label = "Total Crashes") %>% # frequency count, can also do percent
      tab_pivot() %>%
      drop_empty_columns() %>% 
      datatable(rownames = FALSE)
  })
  
  updatePickerInput(session,
                    "year",
                    choices = c(2019, 2018)) #Set years of data

  #                                                          First row charts  
  output$tot_crash <- renderInfoBox({
    valueBox(
      nrow(tot_crash_num()), "Total Crashes", icon = icon("car-crash"),
      color = "red"
    )
  })
  output$tot_inj <- renderInfoBox({
    valueBox(
      tot_crash_num() %>% summarise(x = sum(TOTINJ)),
      "Total Injuries", icon = icon("band-aid"),
      color = "red"
    )
  })
  output$tot_fatal <- renderInfoBox({
    valueBox(
      tot_crash_num() %>% summarise(x = sum(TOTFATL)),
      "Total Fatalities", icon = icon("skull"),
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
  #                                                          X row charts 
  output$passveh_box <- renderInfoBox({
    valueBox(
      3400, "Passenger Veh.", icon = icon("car"),
      color = "red"
    )
  })
  output$light_truck_box <- renderInfoBox({
    valueBox(
      32, "Light Trucks", icon = icon("truck"),
      color = "red"
    )
  })
  output$large_truck_box <- renderInfoBox({
    valueBox(
      6, "Large Trucks", icon = icon("truck-moving"),
      color = "green"
    )
  })
  output$motorcycle_box <- renderInfoBox({
    valueBox(
      12, "Motorcycles", icon = icon("motorcycle"),
      color = "blue"
    )
  })
  output$bike_box <- renderInfoBox({
    valueBox(
      4, "Bicycles", icon = icon("bicycle"),
      color = "orange"
    )
  })
  output$ped_box <- renderInfoBox({
    valueBox(
      23, "Pedestrians", icon = icon("walking"),
      color = "yellow"
    )
  })
  
#                                                          SECOND row charts                       
  output$bike_ped_flag <- renderPlot({
    # all_crashes <- rbind(crash_month())  #take variable of what was inputted
    # all_crashes$group <- c()
    
    # bike_crashes <- all_crashes %>% 
    #   # group_by(CRSHMTH) %>%
    #   filter(BIKEFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
    # 
    # ped_crashes <- all_crashes %>% 
    #   # group_by(CRSHMTH) %>%
    #   filter(PEDFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart 
    # 
    # all_crashes <- all_crashes %>% 
    #   # group_by(CRSHMTH) %>%
    #   filter(CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart 
    # 
    # ggplot(mapping = aes(x=all_crashes$CRSHMTH, y=..count..)) +
    #   theme_classic() +
    #   geom_bar(fill = "orange") +
    #   geom_bar(width = .5, mapping = aes(x=ped_crashes$CRSHMTH, y=..count..), fill = "blue") +
    #   geom_bar(width = .3, mapping = aes(x=bike_crashes$CRSHMTH, y=..count..), fill = "red") +
    #   theme(axis.line=element_blank(),
    #         legend.position = "none",
    #         axis.text.y=element_blank(),axis.ticks=element_blank(),
    #         axis.text.x = element_text(size = 12)
    #   ) +
    #   scale_x_discrete(limits = month.name, name = "") +
    #   scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "")

  })
  output$crsh_svr_mth <- renderPlot({
    crsh_svr_mth <- all_crashes %>%
      # group_by(CRSHMTH) %>%
      filter(CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart

    ggplot(crsh_svr_mth, mapping = aes(CRSHMTH)) +
      theme_classic() +
      geom_bar(aes(fill=CRSHSVR)) +
      theme(
        axis.line = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 12
        )
      ) +
      scale_x_discrete(limits = month.name, name = "") +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "")
    
  })
  
  
  output$timeofday_heat <- renderD3heatmap({
   
    day_time <- all_crashes %>%
      filter(CNTYCODE == input$cntynum) %>%
      # apply_labels(CNTYCODE = "County") %>%
      tab_cells(newtime) %>%       # stuff to put in the rows
      # tab_subgroup(ALCFLAG == "Yes") %>%                # only select certain elements
      tab_cols(DAYNMBR) %>%     # columns
      tab_stat_cases() %>% # frequency count, can also do percent
      tab_pivot()
      # drop_empty_columns()
    
    row.names(day_time) <-
      day_time$row_labels # - change row names to match row_labels
    
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
      day_time[, c("|Sunday",
                   "|Monday",
                   "|Tuesday",
                   "|Wednesday",
                   "|Thursday",
                   "|Friday",
                   "|Saturday")] # reorder columns
    
    d3heatmap(
      day_time[1:24, 1:7],
      Rowv = FALSE,
      Colv = FALSE,
      colors = "Blues",
      xaxis_font_size = "12px",
      yaxis_font_size = "12px",
      labCol = c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )
    )
  })
  
  # output$alcflag <- renderPlot({
  #   # all_crashes <- rbind(crash_month())  #take variable of what was inputted
  #   # all_crashes$group <- c()
  #   
  #   all_crashes <- all_crashes %>% 
  #     # group_by(CRSHMTH) %>%
  #     filter(ALCFLAG == "Yes", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
  #   
  #   all_crashes %>% 
  #     ggplot(mapping = aes(x=CRSHMTH, y=..count..)) +
  #     theme_classic() +
  #     geom_bar()
  # })
  #                                                          THIRD row charts
  
  output$mnrcoll <- renderPlotly({
    
    all_crashes <- all_crashes %>%
      filter(MNRCOLL != "Unknown", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
    
    all_crashes$MNRCOLL <- fct_infreq(all_crashes$MNRCOLL) %>% fct_rev()
    
  mnrcoll_chart <- 
    all_crashes %>%
      ggplot(mapping = aes(x = MNRCOLL, y = ..count..)) +
      theme_classic() +
      geom_bar(fill = "#428BCA") +
      ggtitle("Manner of Collision") +
      theme(axis.line=element_blank(),
            legend.position = "none",
            axis.ticks=element_blank(),
            axis.text.x = element_text(size = 12),
            plot.title = element_text()
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      coord_flip()
  mnrcoll_chart %>% ggplotly() # hoverinfo, can use event_data to update ui data
    # 
    # all_crashes <- all_crashes %>%  # can delete this
    #   filter(MNRCOLL != "Unknown", CNTYCODE == input$cntynum) %>%
    #   group_by(MNRCOLL, CRSHMTH) %>%
    #   ggplotly() %>%
    #   add_trace(x = ~MNRCOLL, y = ~ CRSHMTH)    # 
    # sum <- all_crashes %>% count(MNRCOLL)
    # sum %>% plot_ly() %>% add_trace(x = ~MNRCOLL, y = ~n, type = 'bar')
  })
  # mapping = aes(x = reorder(MNRCOLL, -count), y = count) not using this
}

# input - from widgets, controls, never include variables likes input$var
# output - stuff to send to ui, always quite variables, plotOutput("plot")
# plot charts uses renderPlot and plotOutput
# text is renderText and textOutput