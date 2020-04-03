library(dplyr) # select, filter functions
library(ggplot2) # make pretty graphs
library(DT)    # create pretty tables
library(expss) # format freq tables
library(forcats) # reorder freq in charts
library(plotly)

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
#                                                       INPUTS
  # all_crashes %>% rename(m_names = MUNICODE)
  
  updatePickerInput(session,
                    "cntynum",
                    choices = setNames(county_recode$CountyCode, county_recode$CountyName))
  
  observeEvent(input$cntynum, {
    muni_codes <- all_crashes %>% 
      filter(CNTYCODE == input$cntynum) %>% 
      pull(MUNICODE) #correct
    
    # muni_codes <- setNames(muni_codes, muni_recode$MUNICIPALITY)
    
    updatePickerInput(session,
                      "muni_names",
                      choices = sort(unique(muni_codes)) )
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
  
  # output$passvehicles <- renderValueBox({   # to get vehicle type count
  #   all_vehicles <- all_vehicles %>%
  #     # group_by(CRSHMTH) %>%
  #     filter(BIKEFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
  #   
  # 
  # })
  #                                                          First row charts  
  output$tot_crash <- renderInfoBox({
    valueBox(
      3400, "Total Crashes", icon = icon("car-crash"),
      color = "purple", width = NULL
    )
  })
  output$tot_inj <- renderInfoBox({
    valueBox(
      340, "Total Injuries", icon = icon("user-injured"),
      color = "purple", width = NULL
    )
  })
  output$tot_fatal <- renderInfoBox({
    valueBox(
      30, "Total Fatalities", icon = icon("skull-crossbones"),
      color = "purple", width = NULL
    )
  })
  output$tot_some <- renderInfoBox({
    valueBox(
      3400, "Total Something", icon = icon("car"),
      color = "purple", width = NULL
    )
  })
  
  #                                                          X row charts 
  output$passveh_box <- renderInfoBox({
    valueBox(
      3400, "Passenger Vehicles", icon = icon("car"),
      color = "purple"
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
    
    bike_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(BIKEFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
    
    ped_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(PEDFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart 
    
    all_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart 
    
    ggplot(mapping = aes(x=all_crashes$CRSHMTH, y=..count..)) +
      theme_classic() +
      geom_bar(fill = "orange") +
      geom_bar(width = .5, mapping = aes(x=ped_crashes$CRSHMTH, y=..count..), fill = "blue") +
      geom_bar(width = .3, mapping = aes(x=bike_crashes$CRSHMTH, y=..count..), fill = "red") +
      theme(axis.line=element_blank(),
            legend.position = "none",
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.text.x = element_text(size = 12)
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
      tab_cols(DAYNMBR) %>%     # columns with nesting
      tab_stat_cases() %>% # frequency count, can also do percent
      tab_pivot() %>%
      drop_empty_columns()
    
    row.names(day_time) <-
      day_time$row_labels # - change row names to match row_labels
    
    # for (col in 1:ncol(day_time)) { #relabel
    #   colnames(day_time)[col] <-
    #     sub("DAYNMBR|", "", colnames(day_time)[col])
    # }
    
    day_time[is.na(day_time)] = 0 #NA will be 0
    
    d3heatmap(day_time[1:24, 2:8], Rowv = FALSE, Colv = FALSE, colors = "Blues")
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
      theme(axis.line=element_blank(),
            legend.position = "none",
            axis.ticks=element_blank(),
            axis.text.x = element_text(size = 12)
      ) +
      geom_bar() +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      coord_flip()
  mnrcoll_chart %>% ggplotly() # hoverinfo, event_data to update ui data
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