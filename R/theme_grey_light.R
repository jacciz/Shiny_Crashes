# Theme: Grey Light -------------------------------------------------------------------------------------
#' @title theme_grey_light
#' @description Grey Light theme for a shinydashboard application
#'
#' @return Object produced by shinyDashboardThemeDIY
#' @seealso \code{\link{shinyDashboardThemeDIY}}
#' @export
theme_grey_light <- dashboardthemes::shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Verdana"
  ,appFontColor = "rgb(45,45,45)"
  ,primaryFontColor = "rgb(15,15,15)"
  ,infoFontColor = "rgb(15,15,15)"
  ,successFontColor = "rgb(15,15,15)"
  ,warningFontColor = "rgb(15,15,15)"
  ,dangerFontColor = "rgb(15,15,15)"
  ,bodyBackColor = "rgb(240,240,240)"
  
  ### header
  ,logoBackColor = "#1d4f81" # "rgb(120,120,120)"
  
  ,headerButtonBackColor = "#1d4f81" # "rgb(120,120,120)"
  ,headerButtonIconColor = "rgb(220,220,220)"
  ,headerButtonBackColorHover = "#1d4f81" #"rgb(100,100,100)"
  ,headerButtonIconColorHover = "white"
  
  ,headerBackColor = "#1d4f81" # "rgb(120,120,120)"
  ,headerBoxShadowColor = "#dfdfdf"
  ,headerBoxShadowSize = "3px 5px 5px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(255,255,255)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#dfdfdf"
  
  ,sidebarUserTextColor = "rgb(115,115,115)"
  
  ,sidebarSearchBackColor = "rgb(240,240,240)"
  ,sidebarSearchIconColor = "rgb(100,100,100)"
  ,sidebarSearchBorderColor = "rgb(220,220,220)"
  
  ,sidebarTabTextColor = "rgb(100,100,100)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected ="rgb(255,255,255)"# "rgb(230,230,230)"
  ,sidebarTabTextColorSelected ="#4fb9db"# "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(255,255,255)"# "rgb(230,230,230)" #"rgb(245,245,245)"
  ,sidebarTabTextColorHover = "#4fb9db" # "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#4fb9db" # "rgb(220,220,220)" #"rgb(200,200,200)"
  ,sidebarTabBorderWidthHover = 4
  ,sidebarTabRadiusHover = "0px"
  
  ,boxBackColor = "rgb(248,248,248)"
  ,boxBorderRadius = 10 # makes edges curved
  ,boxShadowSize = "solid"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(34,43,69)"#"rgb(52,62,72)"
  ,boxPrimaryColor = "#1d4f81" #1d4f81
  ,boxInfoColor = "#428BCA"
  ,boxSuccessColor = "#4DB848"
  ,boxWarningColor = "rgb(240,80,210)"
  ,boxDangerColor = "#D50032"
  # ,boxDefaultColor = "rgb(225,225,225)"
  # ,boxPrimaryColor = "rgb(95,155,213)"
  # ,boxInfoColor = "rgb(180,180,180)"
  # ,boxSuccessColor = "rgb(112,173,71)"
  # ,boxWarningColor = "rgb(237,125,49)"
  # ,boxDangerColor = "rgb(232,76,34)"
  
  ,tabBoxTabColor =  "rgb(248,248,248)" # when selected, bg color
  ,tabBoxTabTextSize = 12
  ,tabBoxTabTextColor = "rgb(45,45,45)" # "rgb(100,100,100)" # font color
  ,tabBoxTabTextColorSelected = "#4fb9db"# "rgb(255,255,255)"# "rgb(45,45,45)" # when selected, text color
  ,tabBoxBackColor = "rgb(248,248,248)" # bg of whole box
  ,tabBoxHighlightColor = "#4fb9db" # "rgb(255,255,255)" # "rgb(200,200,200)" # when selected, border color
  ,tabBoxBorderRadius = 10 # makes edges curved
  
  ### inputs
  ,buttonBackColor = "rgb(255,255,255)" #  "rgb(215,215,215)" # county dropdown
  ,buttonTextColor = "rgb(45,45,45)"
  ,buttonBorderColor = "rgb(150,150,150)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(240,240,240)" #"rgb(190,190,190)"
  ,buttonTextColorHover = "rgb(0,0,0)"
  ,buttonBorderColorHover = "rgb(150,150,150)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "#428BCA" # "rgb(118,118,118)"
  ,textboxBorderRadius = 10 # makes edges curved
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(108,108,108)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "#428BCA" # "rgb(238,238,238)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

# Logo: Grey Light --------------------------------------------------------------------------------------
#' @title logo_grey_light
#' @description Grey Light logo for a shinydashboard application
#'
#' @param boldText String. Bold text for the logo.
#' @param mainText String. Main text for the logo.
#' @param badgeText String. Text for the logo badge.
#'
#' @return Object produced by shinyDashboardLogoDIY
#' @seealso \code{\link{shinyDashboardLogoDIY}}
#' @export

logo_grey_light <- dashboardthemes::shinyDashboardLogoDIY(
  boldText = "",
  mainText = "<img src=www/theme_logo.png style=height:40px;display:inline-block;background-color:transparent> WisDOT Crash Dashboard",
  textSize = 20,
  badgeText = "",
  badgeTextColor = "white",
  badgeTextSize = 0,
  badgeBackColor = "#1d4f81",
  badgeBorderRadius = 0
)
# WisDOT Colors
#   light_blue = "#428BCA",
#   blue = "#003087",
#   green = "#4DB848",
#   red = "#D50032",
#   yellow = "#F9C218"
# Background blue - #1d4f81

# Theme colors (same as WisDOT, more subtle)
#   purple-blue = "#5a77db",
#   blue = "#4fb9db",
#   green = "#44dbae",
#   red = "#Db7e65",
#   yellow = "#dbb039"
#   white = "rgb(255,255,255)"  

# sidebar bg: rgb(255,255,255)
# main wrapper bg: rgb(240,240,240)
# box bg: rgb(248,248,248)
# img(src = 'zero-logo.png',  style = "width: 100px; display: block; margin-left: auto; margin-right: auto;")
