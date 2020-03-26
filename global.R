# library(grDevices)  # for fonts

# all_crashes <- read.csv("data/2020_crash.csv")    # pull either 2019 or 2020
all_crashes <- read.csv("data/2019_crash.csv")

county_recode <- read.csv("data/county_recode.csv")

# windowsFonts("Cambria" = windowsFont("Cambria"))

theme_traffic <- function(base_size = 11, base_family = ""){
  
  
}