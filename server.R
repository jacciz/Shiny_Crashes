library(dplyr) # select, filter functions
library(ggplot2) # make pretty graphs
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
# INPUTS

  tot_crash_num <- reactive({
    filtered_crashes()
  })
 
  updateSelectInput(session,
                    "cntynum", selected = 13,
                    choices = setNames(county_recode$CountyCode, county_recode$CountyName))
  
  observeEvent(input$cntynum, {

    muni_cnty_list <- muni_recode %>% filter(CntyCode == input$cntynum)
    
    updateSelectInput(session,
                      "muni_names",
                      choices = setNames(muni_cnty_list$MuniCode, muni_cnty_list$Municipality_CTV) )
  }) 
  
  output$crsh_svr_out <- renderPrint(input$crsh_svr)
  
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
  
  updateSelectInput(session,
                    "year", selected = 2019,
                    choices = c(2020, 2019, 2018, 2017)) #Set years of data
  
  # Selected and filtered data
  filtered_crashes <- reactive({
    all_crashes %>%
      filter(CNTYCODE == input$cntynum, year(CRSHDATE) %in% input$year)
    # crsh_svr_out
  })
  
  filtered_persons <- reactive({
    all_persons %>%
      filter(CNTYCODE == input$cntynum, year(CRSHDATE) %in% input$year, WISINJ %in% input$crsh_svr)
  })
  

  # First row charts  
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
  # X row charts 
  output$passveh_box <- renderInfoBox({
    valueBox(
      3400, "Passenger Veh.", icon = icon("car"),
      color = "yellow"
    )
  })
  output$light_truck_box <- renderInfoBox({
    valueBox(
      32, "Light Trucks", icon = icon("truck"),
      color = "yellow"
    )
  })
  output$large_truck_box <- renderInfoBox({
    valueBox(
      6, "Large Trucks", icon = icon("truck-moving"),
      color = "yellow"
    )
  })
  output$motorcycle_box <- renderInfoBox({
    valueBox(
      12, "Motorcycles", icon = icon("motorcycle"),
      color = "yellow"
    )
  })
  output$bike_box <- renderInfoBox({
    valueBox(
      4, "Bicycles", icon = icon("bicycle"),
      color = "yellow"
    )
  })
  output$ped_box <- renderInfoBox({
    valueBox(
      23, "Pedestrians", icon = icon("walking"),
      color = "yellow"
    )
  })
  
# SECOND row charts                       
  
  output$crsh_svr_mth <- renderPlotly({
    
    # labels = c("Jan.", "Feb.") #could write a list of abbreviations for chart

    crsh_svr_mth_chart <- filtered_crashes() %>% ggplot(mapping = aes(CRSHMTH)) +
      theme_classic() +
      geom_bar(aes(fill=CRSHSVR)) +
      theme(
        axis.line = element_blank(),
        legend.position = "top",   # not working
        legend.box = "horizontal",
        axis.ticks = element_blank(),
        legend.text = element_text(size = 10, family = "Cambria", face = "plain", color = "white"),
        legend.title = element_text(size = 10, family = "Cambria", face = "plain", color = "white"),
        axis.text.x = element_text(size = 10, family = "Cambria", face = "plain", color = "white"),
        axis.text.y = element_text(size = 10, family = "Cambria", face = "plain", color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background =element_rect(fill = "transparent", color = NA)
      ) +
      scale_x_discrete(limits = month.name, name = "", labels = function(labels) {  # scatter labels
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}
                         ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
    scale_fill_manual(
      name = "Crash Severity",
      values = c("#D50032", "#428BCA", "#4DB848"))
    
  crsh_svr_mth_chart %>% ggplotly() # hoverinfo, can use event_data to update ui data
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
      theme = "dark",
      na.rm = FALSE,
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
        axis.text.x = element_text(size = 10, color = "white"),
        axis.text.y = element_text(size = 8, color = "white"),
        axis.title.y = element_blank(),
        plot.title = element_text(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      coord_flip()
    mnrcoll_chart %>% ggplotly() # hoverinfo, can use event_data to update ui data
  })
  
  output$person_role <- renderPlotly({
    person <- filtered_persons()
    
    person$ROLE <- fct_infreq(person$ROLE) %>% fct_rev() # sorts data
    
    p_role_chart <- 
      person %>%
      ggplot(mapping = aes(x = ROLE, y = ..count.., fill = WISINJ)) +
      theme_classic() +
      geom_bar(fill = "#428BCA") +
      theme(axis.line=element_blank(),
            legend.position = "none",
            axis.ticks=element_blank(),
            axis.text.x = element_text(size = 10, color = "white"),
            axis.text.y = element_text(size = 8, color = "white"),
            axis.title.y = element_blank(),
            plot.title = element_text(),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +
      scale_x_discrete( name = "", labels = function(labels) {  # scatter labels
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}) +
      coord_flip()
    
    p_role_chart %>% ggplotly() # hoverinfo, can use event_data to update ui data
    
  })
  
  output$person_age_gender <- renderPlotly({
    person <-
      filtered_persons() %>% select(age_group, SEX) %>% na.omit()
    
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
        axis.text.x = element_text(size = 10, color = "white"),
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
        name = "",
        labels = function(labels) {
          # scatter labels
          sapply(seq_along(labels), function(i)
            paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        }
      ) +
      scale_fill_manual(
        name = "Gender",
        values = c("#D50032", "#428BCA", "#F9C218"),
        labels = c("Female", "Male", "Unknown") # do not work
      )
    # coord_flip()
    
    p_age_gender_chart %>% ggplotly() # hoverinfo, can use event_data to update ui data
    
  })
}