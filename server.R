library(dplyr)
library(ggplot2)

setwd("W:/HSSA/Keep/Jaclyn Ziebert/R_WD_Data/R_Shiny")
# all_crashes <- read.csv("data/2020_crash.csv")

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel()              # sidebar panel stuff ?
    }
  })
  
  crash_jan <- reactive({              # INPUTS: user selects from county list
    all_crashes%>%filter(
      CNTYCODE %in% input$cntynum)
      })
  
  output$curve_plot <- renderPlot({    # OUTPUTS: test to show plot
    curve(x ^ input$exponent, from = -5, to = 5)
  })
  
  output$bikeflag <- renderPlot({
    # all_crashes <- rbind(crash_jan())  #take variable of what was inputted
    # all_crashes$group <- c()
    
    all_crashes <- all_crashes %>% 
      # group_by(CRSHMTH) %>%
      filter(BIKEFLAG == "Y")
    
    all_crashes %>% 
    ggplot() +
      geom_bar(mapping = aes(x=CRSHMTH, y=..count..))
  })
  
}

# shinyApp(ui, server)


# function(input, output, session) {
#   
  # output$curve_plot <- renderPlot({
  #   curve(x ^ input$exponent, from = -5, to = 5)
  # })
#   
# }
# input - from widgetse, controls, never include variables likes input$var
# output - stuff to send to ui, always quite variables, plotOutput("plot")
# plot charts uses renderPlot and plotOutput
# text is renderText and textOutput