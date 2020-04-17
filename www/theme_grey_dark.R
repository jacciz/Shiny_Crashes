### theme_grey_dark ---------------------------------------------------------
# @export
mytheme_grey_dark <- shinyDashboardThemeDIY(

    ### general
    appFontFamily = "Cambria"
    ,appFontColor = "rgb(205,205,205)"
    ,primaryFontColor = "rgb(255,255,255)"
    ,infoFontColor = "rgb(255,255,255)"
    ,successFontColor = "rgb(255,255,255)"
    ,warningFontColor = "rgb(255,255,255)"
    ,dangerFontColor = "rgb(255,255,255)"
    ,bodyBackColor = "rgb(45,55,65)"
    ,logoBackColor = "rgb(70,80,90)"
    ,headerButtonBackColor = "rgb(70,80,90)"
    ,headerButtonIconColor = "rgb(25,35,45)"
    ,headerButtonBackColorHover = "rgb(40,50,60)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"

    ,headerBackColor = "rgb(70,80,90)"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"

    ### sidebar
    ,sidebarBackColor = "rgb(52,62,72)"
    ,sidebarPadding = 0

    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0

    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"

    ,sidebarUserTextColor = "rgb(205,205,205)" # grey

    ,sidebarSearchBackColor = "rgb(45,55,65)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(45,55,65)"

    ,sidebarTabTextColor = "rgb(205,205,205)" # "rgb(0,0,0)" menu is better
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0

    ,sidebarTabBackColorSelected = "rgb(70,80,90)"
    ,sidebarTabTextColorSelected = "rgb(255,255,255)" #white
    ,sidebarTabRadiusSelected = "5px"

    ,sidebarTabBackColorHover = "rgb(55,65,75)"
    ,sidebarTabTextColorHover = "rgb(255,255,255)" 
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "5px"

    ### boxes
    ,boxBackColor = "rgb(52,62,72)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(52,62,72)"
    ,boxPrimaryColor = "#428BCA"
    ,boxInfoColor = "#428BCA"
    ,boxSuccessColor = "#4DB848"
    ,boxWarningColor = "rgb(240,80,210)"
    ,boxDangerColor = "#D50032"

    ,tabBoxTabColor = "rgb(52,62,72)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(205,205,205)"
    ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
    ,tabBoxBackColor = "rgb(52,62,72)"
    ,tabBoxHighlightColor = "rgb(70,80,90)"
    ,tabBoxBorderRadius = 5

    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)" 
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5

    ,buttonBackColorHover = "rgb(180,180,180)"
    ,buttonTextColorHover = "rgb(50,50,50)"
    ,buttonBorderColorHover = "rgb(50,50,50)"

    ,textboxBackColor = "rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"

    ### tables
    ,tableBackColor = "rgb(52,62,72)"
    ,tableBorderColor = "rgb(70,80,90)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
  )

logo_mytheme <- shinyDashboardLogoDIY(
  boldText = "test"
  ,mainText = "WisDOT Vision Zero Dashboard"
  ,textSize = 14
  ,badgeText = "BETA"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
)

  # WisDOT Colors
#   light_blue = "#428BCA",
#   blue = "#003087",
#   green = "#4DB848",
#   red = "#D50032",
#   yellow = "#F9C218"
