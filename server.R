library(dplyr) # select, filter functions
library(ggplot2) # make pretty graphs
library(DT)    # create pretty tables
library(expss) # format freq tables

setwd("W:/HSSA/Keep/Jaclyn Ziebert/R_WD_Data/Shiny_Crashes")
# all_crashes <- read.csv("data/2020_crash.csv")    # pull either 2019 or 2020
all_crashes <- read.csv("data/2019_crash.csv")

county_recode <- read.csv("data/county_recode.csv")
# setNames(county_recode$CountyCode, county_recode$CountyName)  # don't need this, in updatePickerInput
windowsFonts("Cambria" = windowsFont("Cambria"))

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
  
  # crash_month <- reactive({              # INPUTS: user selects from county list
  #   all_crashes%>%filter(
  #     CNTYCODE %in% input$cntynum)
  #     })
  # 
  updatePickerInput(session, "cntynum", choices = setNames(county_recode$CountyCode, county_recode$CountyName))
  
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
  
#                                                        First row charts                       
  output$bikeflag <- renderPlot({
    # all_crashes <- rbind(crash_month())  #take variable of what was inputted
    # all_crashes$group <- c()
    
    all_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(BIKEFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
    
    all_crashes %>% 
    ggplot() +
      theme_classic() +
      geom_bar(mapping = aes(x=CRSHMTH, y=..count..))
  })
  
  output$pedflag <- renderPlot({
    # all_crashes <- rbind(crash_month())  #take variable of what was inputted
    # all_crashes$group <- c()
    
    all_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(PEDFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
    
    all_crashes %>% 
      ggplot() +
      theme_classic() +
      geom_bar(mapping = aes(x=CRSHMTH, y=..count..)) +
      theme(axis.line=element_blank(),
            legend.position = "none",
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.text.x = element_text(size = 12, family = "Cambria")
      ) +
      scale_x_discrete(limits = month.name, name = "") +
      scale_y_continuous(expand = expansion(mult = c(0, .05)), name = "") +  # y starts at 0, adds 5% gap on top
      geom_text(stat = "count", family = "Cambria", size = 6, aes(x = CRSHMTH, y = ..count.. + ..count../6, label = ..count..)) 
  })
  
  output$alcflag <- renderPlot({
    # all_crashes <- rbind(crash_month())  #take variable of what was inputted
    # all_crashes$group <- c()
    
    all_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(ALCFLAG == "Yes", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
    
    all_crashes %>% 
      ggplot() +
      theme_classic() +
      geom_bar(mapping = aes(x=CRSHMTH, y=..count..))
  })
  
}

# input - from widgets, controls, never include variables likes input$var
# output - stuff to send to ui, always quite variables, plotOutput("plot")
# plot charts uses renderPlot and plotOutput
# text is renderText and textOutput