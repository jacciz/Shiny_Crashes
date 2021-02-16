#' Charts modules for server-side processings
#'
#' This module produces all charts with the crash_df/people_df/Vehicles_df based on variables selected by the user.
#' 
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df data frame (non-reactive) with variables necessary for charts

# Put this in the actual app to reference these modules
# UI: crsh_svr_mth_ui("crsh_svr_mth")
# Server: crsh_svr_mth_server("crsh_svr_mth", filtered_crashes())

# To add a new chart - use this
# crsh_svr_mth_server <- function(id, crash_df()) {
#   moduleServer(id, function(input, output, session) {
#     plotly stuff goes here
#   })
#   }

################### Text Parameters for Charts #######################

# Text parameters for charts
chart_title = list(size = 16, color = "rgb(100,100,100)", family = "Verdana")
chart_axis = list(size = 14, color = "rgb(100,100,100)", family = "Verdana")
# chart_axis_bar = list(size = 14, color = "#38839C", family = "Verdana", face = "bold")
# New color - Text parameters for charts. Also replaced #428BCA with #4fb9db
chart_axis_bar = list(size = 14, color = "#4fb9db", family = "Verdana", face = "bold")

################### Colors and Factors #######################
# assigning colors for crash severity and gender for charts/map
# color_map_svr <- c("Fatal"="#D50032", "Injury"="#38839C", "Property Damage"="#4DB848")
# color_map_gender <- c("Female"="#D50032", "Male"="#38839C", "Unknown" = "#F9C218")
# color_map_wisinj <- c("Suspected Minor Injury" ="#4DB848", "Possible Injury" = "#38839C","Suspected Serious Injury" ="#7f42ca", "Fatal Injury" = "#D50032")

# New colors
color_map_svr <- c("Fatal"="#DB7E65", "Injury"="#4AAECF", "Property Damage"="#44DBAE")
color_map_gender <- c("Female"="#Db7e65", "Male"="#4fb9db", "Unknown" = "#dbb039")
color_map_wisinj <- c("Suspected Minor Injury" ="#4AAECF", "Possible Injury" = "#58CEF5","Suspected Serious Injury" ="#3D8DA8", "Fatal Injury" = "#265869")

# Factor levels
wisinj_factor_levels <- c("Possible Injury", "Suspected Minor Injury", "Suspected Serious Injury", "Fatal Injury")

# TRY THIS: ~ stringr::str_wrap(reorder(drvrpc_count, n), width = 20),

################### Crash Severity by Month Bar Chart #######################

crsh_svr_mth_server <- function(id, crash_df) {
  moduleServer(id, function(input, output, session) {
   output$crsh_svr_mth <- renderPlotly({
      if (dim(crash_df())[1] == 0) {  # or no crashes with a time ??
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nCrash Severity by Month",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        crshsvr_table <-
          table(month = crash_df()$CRSHMTH, svr = crash_df()$CRSHSVR) %>% as_tibble() # get counts, put in a tibble
        crshsvr_table$month <-
          factor(crshsvr_table$month, levels = month.name) # factors month names, in month.name order
        
        month_order <-
          c(
            "Jan",
            "Feb",
            "Mar",
            "Apr",
            "May",
            "Jun",
            "Jul",
            "Aug",
            "Sep",
            "Oct",
            "Nov",
            "Dec"
          ) # rename months
        crshsvr_table$month <-
          month.abb[crshsvr_table$month] # abbreviate months
        # month_factor = month.name
        
        # Finds months not in df and adds those rows then replaces NA with 0
        add <- month_order[!month_order %in% crshsvr_table$month]
        crshsvr_table <-
          if (length(add) != 0) {
            crshsvr_table %>% add_row(month = add) %>% replace_na(list(n = 0, svr = "Fatal"))
          } else{
            crshsvr_table
          }
        
        plot_ly(
          crshsvr_table,
          type = 'bar',
          x = ~ month,
          y = ~ n,
          color = ~ svr,
          colors = ~ color_map_svr,
          # assign colors, this will give a warning 'Duplicate levels detected'
          hovertemplate = paste('%{x}<br>',
                                '<b>%{y: .0f} Crashes')
        ) %>% #Price: %{y:$.2f}<extra></extra>
          layout(
            title = list(
              text = "\nCrash Severity by Month",
              font = chart_title,
              y = 1,
              x = 0
            ),
            showlegend = TRUE,
            legend = list(
              x = .5,
              y = 1.2,
              orientation = 'h',
              font = chart_axis,
              traceorder = "normal" # alphabetical legend order
            ),
            margin = list(
              r = 0,
              l = 0,
              b = 0,
              t = 45
            ),
            xaxis = list(
              title = "",
              tickfont = chart_axis,
              tickangle = -0,
              categoryarray = ~ month_order,
              categoryorder = "array" # sets order
              # ticktext = ~month.abb[crshsvr_table$month],
              # automargin = TRUE,
              # dtick = 5 # every 5 months are labeled
            ),
            yaxis = list(
              showgrid = FALSE,
              zerolinecolor = "white",
              tickfont = chart_axis,
              title = ""
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)',
            barmode = 'stack'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Crash Severity by Month",
              scale = 2
            )
          )
      }
      # scale_x_discrete(limits = month.name, name = "", labels = function(labels) {  # scatter labels
      # sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))}
    })
  })
}
################### Persons Injured by Year Bar Chart #######################

wisinj_by_year_server <- function(id, person_df) {
  moduleServer(id, function(input, output, session) {
    output$wisinj_by_year <- renderPlotly({
      if (dim(person_df())[1] == 0) {  # or no crashes with a time ??
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nPersons Injured each Year",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        wisinj_table <-  
          table(year = year(person_df()$CRSHDATE), inj = factor(person_df()$WISINJ, levels = wisinj_factor_levels)) %>% as_tibble() %>% filter(inj != "No Apparent Injury")# get counts, put in a tibble
        # crshsvr_table$month <-
          # factor(crshsvr_table$month, levels = month.name) # factors month names, in month.name order

        plot_ly(
          wisinj_table,
          type = 'bar',
          x = ~ year,
          y = ~ n,
          color = ~ factor(inj, levels = wisinj_factor_levels),
          colors = ~ color_map_wisinj,
          text = ~sprintf("<b>%s</b>", format(n, big.mark = ",")),
          # bar end number
          textfont = list(size = 14, color = color_map_wisinj[wisinj_table$inj], family = "Verdana", face = "bold"),
          textposition = 'outside',
          cliponaxis = FALSE,
          # assign colors, this will give a warning 'Duplicate levels detected'
          hovertemplate = paste('%{x}<br>',
                                '<b>%{y: .0f} Persons')
        ) %>% #Price: %{y:$.2f}<extra></extra>
          layout(
            title = list(
              text = "\nPersons Injured each Year",
              font = chart_title,
              y = 1,
              x = 0
            ),
            showlegend = TRUE,
            legend = list(
              x = .5,
              y = 1.2,
              orientation = 'h',
              font = chart_axis,
              traceorder = "normal" # alphabetical legend order
            ),
            margin = list(
              r = 0,
              l = 0,
              b = 0,
              t = 45
            ),
            xaxis = list(
              title = "",
              tickfont = chart_axis,
              tickangle = -0,
              # categoryarray = ~ month_order,
              categoryorder = "array" # sets order
              # ticktext = ~month.abb[crshsvr_table$month],
              # automargin = TRUE,
              # dtick = 5 # every 5 months are labeled
            ),
            yaxis = list(
              showgrid = FALSE,
              zerolinecolor = "white",
              tickfont = chart_axis,
              title = ""
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)',
            barmode = 'group'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Persons Injured each Year",
              scale = 2
            )
          )
      }
    })
  })
}
################### Time of Day Heatmap #######################

timeofday_heat_server <- function(id, crash_df) 
  moduleServer(id, function(input, output, session) {

output$timeofday_heat <- renderPlotly({
  
  if (dim(crash_df())[1] == 0) { # or no crashes with a time ??
    plotly_empty(type = "heatmap") %>% layout(
      title = list(text ="Time of Day", font = chart_title, x = 0),
      plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
      paper_bgcolor = 'rgba(0,0,0,0)'
    )
  } else {
    day_time_data <- crash_df()[ , .(.N), by = .(newtime, DAYNMBR)]
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
        title = list(text ="Time of Day", font = chart_title, x = 0),
        margin = list(r = 0,l = 0, b = 0
        ),
        xaxis = list(tickfont = chart_axis, tickangle = 0, tickcolor = "white"),
        yaxis = list(tickfont = chart_axis, tickcolor = "white"),
        plot_bgcolor = 'rgba(0,0,0,0)', # make transparent background
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%  config(toImageButtonOptions = list(width = 800, height = 800, filename = "Time of Day Crashes", scale = 2)
      )
  }
})
  })

################### Manner of Collision Bar Chart #######################
mnrcoll_server <- function(id, crash_df) {
  moduleServer(id, function(input, output, session) {
    output$mnrcoll <- renderPlotly({
      if (dim(crash_df())[1] == 0) { # if no crashes, show empty plot, else make plot
        # hide("mnrcoll")
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nManner of Collision",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        mnr_crashes <- crash_df() %>%
          filter(MNRCOLL != "Unknown")
        
        mnr_crashes_table <-
          table(mnrcoll = mnr_crashes$MNRCOLL) %>% as_tibble()
        
        plot_ly(
          mnr_crashes_table,
          type = 'bar',
          orientation = 'h',
          x = ~ n,
          # y = ~ reorder(mnrcoll, n),
          y = ~ reorder(stringr::str_wrap(mnrcoll, width = 30), n), 
          # reorder from big to small values
          marker = list(color = "#4fb9db"),
          # blue!
          hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
          text = ~sprintf("<b>%s</b>", format(n, big.mark = ",")),
          # bar end number
          textfont = chart_axis_bar,
          textposition = 'outside',
          cliponaxis = FALSE
        ) %>%
          layout(
            title = list(
              text = "\nManner of Collision",
              font = chart_title,
              x = 0,
              y = .99
            ),
            margin = list(r = 40, # set to 40 so labels don't get cut off
                          l = 200, # so axis label don't get cut off
                          t = 40,
                          pad = 5, # axis label to bar padding
                          b = 0),
            xaxis = list(
              title = "",
              zerolinecolor = "white",
              showgrid = FALSE,
              showticklabels = FALSE # remove axis labels
            ),
            yaxis = list(title = "", tickfont = chart_axis),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>% config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Manner of Collision",
              scale = 2
            )
          )
      }
    })
  })
}
################### Person by Role Tree Map #######################
person_role_treemap_server <- function(id, persons_df) {
  # stopifnot(is.reactie(x))
  moduleServer(id, function(input, output, session) {
    output$person_role_treemap <- renderPlotly({
      if (dim(persons_df())[1] == 0) {  # or no crashes with a time ??
        plotly_empty(type = "treemap") %>% layout(
          title = list(
            text = "\nRole of All Persons",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        role_table <-
          table(role = persons_df()$ROLE) %>% as_tibble() %>% mutate(parent = "role")
        plot_ly(
          role_table,
          type = 'treemap',
          textfont = list(size = 14, family = "Verdana"),
          outsidetextfont = list(color = "rgba(0,0,0,0)"), # transparent title
          labels = ~ role,
          parents = ~ parent,
          values = ~ n,
          hoverlabel = list(font=list(size = 16, family = "Verdana")),
          hoverinfo = "label+value+percent root",
          textinfo = "label+value+percent root"
        ) %>%
          layout(colorway=c("#5a77db", "#F9C218", "#4DB848","#D50032","#4fb9db"),
                 uniformtext=list(minsize=14),
            title = list(
              text = "\nRole of All Persons",
              font = chart_title,
              y = 1,
              x = 0
            ),
            margin = list(
              r = 0,
              l = 0,
              b = 10,
              t = 0
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Role of All Persons",
              scale = 2
            )
          )
      }
    })
  })
}
################### Age and Gender Bar Chart #######################
person_age_gender_server <- function(id, persons_df) {
  moduleServer(id, function(input, output, session) {
    output$person_age_gender <- renderPlotly({
      if (dim(persons_df())[1] == 0) {
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nAge and Gender of All Persons",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        person <-
          persons_df()[, .(age_group, SEX)]
        
        age_sex_table <-
          table(age = person$age_group, sex = person$SEX) %>% as_tibble() # get counts, put in a tibble
        
        plot_ly(
          age_sex_table,
          type = 'bar',
          x = ~ age,
          y = ~ n,
          color = ~ sex,
          colors = ~ color_map_gender,
          hovertemplate = paste('<br>Age %{x}<br>',
                                '<b>%{y: .0f} people<b>')
        ) %>%
          layout(
            title = list(
              text = "\nAge and Gender of All Persons",
              font = chart_title,
              y = 1,
              x = 0
            ),
            showlegend = TRUE,
            legend = list(
              x = .5,
              y = 1.2,
              orientation = 'h',
              font = chart_axis
            ),
            margin = list(
              r = 0,
              l = 0,
              b = 0,
              t = 45
            ),
            xaxis = list(
              title = "",
              tickfont = chart_axis,
              tickangle = -45,
              categoryarray = ~ age,
              categoryorder = "array"
            ),
            yaxis = list(
              title = "",
              showgrid = FALSE,
              tickfont = chart_axis,
              zerolinecolor = "white"
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)',
            barmode = 'stack'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Age and Gender of All Persons",
              scale = 2
            )
          )
        # labels = function(labels) { # this scatters labels so they fit on two lines
        #   sapply(seq_along(labels), function(i)
        #     paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        # }
      }
    })
  })
}
################### DRVRPC Bar Chart #######################
drvrpc_chart_server <- function(id, persons_df) {
  moduleServer(id, function(input, output, session) {
    output$drvrpc_chart <- renderPlotly({
      if (dim(persons_df())[1] == 0) {
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nNo Driver Contributing Circumstances Found",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        drvrpc <- persons_df() %>%
          select(DRVRPC01:DRVRPC24) %>% pivot_longer(DRVRPC01:DRVRPC24) %>% filter(value !=
                                                                                     '')
        # make freq table, remove variables, arrange and take top 8
        drvrpc_table <-
          table(drvrpc_count = drvrpc$value) %>% as_tibble() %>%
          filter(drvrpc_count != "No Contributing Action",
                 drvrpc_count != "Unknown") %>% arrange(desc(n)) %>% head(., 8)
        #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
        drvrpc_table$drvrpc_count <-
          reorder(drvrpc_table$drvrpc_count, drvrpc_table$n)  # reorder from big to small values
        plot_ly(
          drvrpc_table,
          type = 'bar',
          orientation = 'h',
          x = ~ n,
          # y = ~ reorder(drvrpc_count, n),
          y = ~ reorder(stringr::str_wrap(drvrpc_count, width = 30), n), # Break line after every 20 characters
          # reorder from big to small values, also wrap text
          marker = list(color = "#4fb9db"),
          # blue!
          hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
          text = ~sprintf("<b>%s</b>", format(n, big.mark = ",")),
          # bar end number
          textfont = chart_axis_bar,
          textposition = 'outside',
          cliponaxis = FALSE
        ) %>% #labels = function(x) str_wrap(drvrpc_count, width = 15)
          layout(
            title = list(
              text = "\nTop Driver Contributing Circumstance",
              font = chart_title,
              x = 0,
              y = .99
            ),
            margin = list(r = 35, # set to 30 so labels don't get cut off
                          l = 200, # so axis label don't get cut off
                          t = 40,
                          pad = 5, # axis label to bar padding
                          b = 0),
            xaxis = list(
              title = "",
              showgrid = FALSE,
              zerolinecolor = "white",
              showticklabels = FALSE # remove axis labels
            ),
            yaxis = list(title = "", tickfont = chart_axis, tickson = "labels"), # make sure labels are in the center of the bar
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Driver Contributing Circumstance",
              scale = 2
            )
          )
      }
    })
  })
}
################### Bike/Ped Top Actions Bar Chart #######################
nmtact_chart_server <- function(id, persons_df) {
  moduleServer(id, function(input, output, session) {
    output$nmtact_chart <- renderPlotly({
      if (dim(
        persons_df() %>% select(NMTACT01:NMTACT12) %>% pivot_longer(NMTACT01:NMTACT12)
        %>% filter(value != "Unknown", value != '')
      )[1] == 0) {
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nNo Pedestrians or Cyclists",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        nmtact <- persons_df() %>%
          select(NMTACT01:NMTACT12) %>% pivot_longer(NMTACT01:NMTACT12) %>% filter(value !=
                                                                                     '')
        # make freq table, remove variables, arrange and take top 8
        nmtact_table <-
          table(nmtact_count = nmtact$value) %>% as_tibble() %>%
          filter(# nmtact_count != "No Improper Action",
            nmtact_count != "Unknown") %>%
          arrange(desc(n)) %>% head(., 8)
        #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
        plot_ly(
          nmtact_table,
          type = 'bar',
          orientation = 'h',
          x = ~ n,
          # y = ~ reorder(nmtact_count, n),
          y = ~ reorder(stringr::str_wrap(nmtact_count, width = 30), n), 
          # reorder from big to small values, also wrap text
          marker = list(color = "#4fb9db"),
          # blue!
          hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
          text = ~sprintf("<b>%s</b>", format(n, big.mark = ",")),
          # bar end number
          textfont = chart_axis_bar,
          textposition = 'outside',
          cliponaxis = FALSE
        ) %>% #labels = function(x) str_wrap(drvrpc_count, width = 15)
          layout(
            title = list(
              text = "\nTop Actions of Pedestrians and Cyclists",
              font = chart_title,
              x = 0,
              y = .99
            ),
            margin = list(r = 30, # set to 30 so labels don't get cut off
                          l = 200, # so axis label don't get cut off
                          t = 40,
                          pad = 5, # axis label to bar padding
                          b = 0),
            xaxis = list(
              title = "",
              showgrid = FALSE,
              zerolinecolor = "white",
              showticklabels = FALSE # remove axis labels
            ),
            yaxis = list(title = "", tickfont = chart_axis),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Actions of Pedestrians and Cyclists",
              scale = 2
            )
          )
      }
    })
  })
}
################### Bike/Ped Location Bar Chart #######################
nmtloc_chart_server <- function(id, persons_df) {
  moduleServer(id, function(input, output, session) {
    output$nmtloc_chart <- renderPlotly({
      if (dim(persons_df() %>% filter(NMTLOC != ''))[1] == 0) {
        plotly_empty(type = "bar") %>% layout(
          title = list(
            text = "\nNo Pedestrians or Cyclists",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        # nmtloc <- persons_df() %>%
        # select(NMTACT01:NMTACT12) %>% pivot_longer(NMTACT01:NMTACT12) %>% filter(value !='')
        # make freq table, remove variables, arrange and take top 8
        nmtloc_table <-
          table(nmtloc_count = persons_df()$NMTLOC) %>% as_tibble() %>%
          filter(nmtloc_count != '') %>%
          # filter(nmtact_count != "No Improper Action",
          # nmtact_count != "Unknown") %>%
          arrange(desc(n)) %>% head(., 8)
        #  reorder(drvrpc_count, n)   str_wrap(drvrpc_count, width = 15)
        plot_ly(
          nmtloc_table,
          type = 'bar',
          orientation = 'h',
          x = ~ n,
          # y = ~ reorder(nmtloc_count, n),
          y = ~ reorder(stringr::str_wrap(nmtloc_count, width = 30), n), 
          # reorder from big to small values, also wrap text
          marker = list(color = "#4fb9db"),
          # blue!
          hovertemplate = paste('%{y}', '<br>%{x: .0f} Crashes<br>'),
          text = ~sprintf("<b>%s</b>", format(n, big.mark = ",")),
          # bar end number
          textfont = chart_axis_bar,
          textposition = 'outside',
          cliponaxis = FALSE
        ) %>% #labels = function(x) str_wrap(drvrpc_count, width = 15)
          layout(
            title = list(
              text = "\nTop Locations of Pedestrians and Cyclists",
              font = chart_title,
              x = 0
            ),
            margin = list(r = 40, # set to 30 so labels don't get cut off
                          l = 200, # so axis label don't get cut off
                          # t = 0, # this will cut off title
                          b = 0),
            xaxis = list(
              title = "",
              showgrid = FALSE,
              showticklabels = FALSE # remove axis labels
            ),
            yaxis = list(title = "", tickfont = chart_axis),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "Locations of Pedestrians and Cyclists",
              scale = 2
            )
          )
      }
    })
  })
}

################### Vehicle Types Tree Map #######################
vehicle_treemap_server <- function(id, vehicles_df) {
  moduleServer(id, function(input, output, session) {
    output$vehicle_treemap <- renderPlotly({
      if (dim(vehicles_df())[1] == 0) {
        plotly_empty(type = "treemap") %>% layout(
          title = list(
            text = "\nAll Vehicles Involved",
            font = chart_title,
            x = 0
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          # make transparent background
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
      } else {
        newcar <- vehicles_df() %>% # put this is data import  # vehcate
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
        car_tib <- # then change newcar to vehicles_df()
          table(vehtype = newcar$VEHTYPE, parent = newcar$cate) %>%
          as_tibble() %>% filter(n != 0)
        
        parent_tib <-
          xtabs(car_tib$n ~ car_tib$parent) %>% as_tibble()# then rbind
        names(parent_tib)[names(parent_tib) == "car_tib$parent"] <-
          "vehtype"
        parent_tib <- parent_tib %>% mutate(parent = "Vehicle Type")
        
        car_tib <- rbind(car_tib, parent_tib)
        
        plot_ly(
          car_tib,
          type = 'treemap',
          branchvalues = "total",
          textfont = list(size = 14, family = "Verdana"),
          outsidetextfont = list(color = "rgba(0,0,0,0)"), # transparent title
          tiling = list(packing = "ratio"),
          ids = ~ vehtype,
          labels = ~ vehtype,
          parents = ~ parent,
          values = ~ n,
          hoverlabel = list(font=list(size = 16, family = "Verdana")), # NEW
          hoverinfo = "label+value+percent root", # NEW
          textinfo = "label+value+percent root" # NEW
        ) %>%
          layout(colorway=c("#5a77db", "#F9C218", "#4DB848","#D50032"),
                 uniformtext=list(minsize=14),
            title = list(
              text = "\nAll Vehicles Involved",
              font = chart_title,
              y = 1,
              x = 0
            ),
            margin = list(
              r = 0,
              l = 0,
              b = 0,
              t = 0
            ),
            plot_bgcolor = 'rgba(0,0,0,0)',
            # make transparent background
            paper_bgcolor = 'rgba(0,0,0,0)'
          )  %>%  config(
            toImageButtonOptions = list(
              width = 800,
              height = 800,
              filename = "All Vehicles Involved",
              scale = 2
            )
          )
      }
    })
  })
}
