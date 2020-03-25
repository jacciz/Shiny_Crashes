library(dplyr)
library(ggplot2)
library(DT)
library(expss)

setwd("W:/HSSA/Keep/Jaclyn Ziebert/R_WD_Data/Shiny_Crashes")
# all_crashes <- read.csv("data/2020_crash.csv")
county_recode <- read.csv("data/county_recode.csv")
# setNames(county_recode$CountyCode, county_recode$CountyName)

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
      tab_cells(CNTYCODE) %>%       # stuff to put in the rows
      tab_subgroup(ALCFLAG == "Yes") %>%                # only select certain elements
      tab_cols(CRSHSVR %nest% ALCFLAG, total()) %>%     # columns with nesting
      tab_stat_cases(total_label = "Total Crashes") %>% # frequency count, can also do percent
      tab_pivot() %>%
      drop_empty_columns() %>% 
      datatable(rownames = FALSE)
  })
  
  
  output$bikeflag <- renderPlot({
    # all_crashes <- rbind(crash_month())  #take variable of what was inputted
    # all_crashes$group <- c()
    
    all_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(BIKEFLAG == "Y", CNTYCODE == input$cntynum)  #CNTYCODE is what changes chart
    
    all_crashes %>% 
    ggplot() +
      geom_bar(mapping = aes(x=CRSHMTH, y=..count..))
  })
  
}

# input - from widgets, controls, never include variables likes input$var
# output - stuff to send to ui, always quite variables, plotOutput("plot")
# plot charts uses renderPlot and plotOutput
# text is renderText and textOutput