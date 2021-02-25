### theme_grey_dark ---------------------------------------------------------
# @export

# background from rgb(45,55,65) to "rgb(22,26,48)"
# foreground from rgb(52,62,72) to "rgb(34,43,69)"
# font color from "rgb(205,205,205)" to "rgb(243,243,245)"
# icon colors will be "rgb(143,155,179)"
# hover text will be "rgb(45,81,189)", but then switched to wisDOT blue "#428BCA"
# light grey is rgb(153,153,153)
# WisDOT blue #428BCA

# bg of menu is rgb(80, 90, 100) to rgb(34,43,69)
# menu select is rgb(30,144,255) to rgb(39, 75, 219)
# menu font is rgb(243, 243, 245) to rgb(255, 255, 255)
# https://www.akveo.com/ngx-admin/pages/dashboard?theme=dark copy this theme
mytheme_grey_dark <- shinyDashboardThemeDIY(

    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(243,243,245)" #"rgb(205,205,205)"
    ,primaryFontColor = "rgb(255,255,255)"
    ,infoFontColor = "rgb(255,255,255)"
    ,successFontColor = "rgb(255,255,255)"
    ,warningFontColor = "rgb(255,255,255)"
    ,dangerFontColor = "rgb(255,255,255)"
    ,bodyBackColor = "rgb(22,26,48)"#"rgb(45,55,65)"
    ,logoBackColor = "rgb(34,43,69)"  #"rgb(70,80,90)"
    ,headerButtonBackColor = "rgb(34,43,69)"# "rgb(70,80,90)"
    ,headerButtonIconColor = "rgb(143,155,179)"
    ,headerButtonBackColorHover = "#428BCA" #"rgb(40,50,60)"
    ,headerButtonIconColorHover = "#428BCA" #"rgb(0,0,0)"

    ,headerBackColor = "rgb(34,43,69)"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"

    ### sidebar # bold font??
    ,sidebarBackColor = "rgb(34,43,69)"#"rgb(52,62,72)"
    ,sidebarPadding = 0

    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0

    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"

    ,sidebarUserTextColor = "rgb(243,243,245)"# "rgb(205,205,205)" # grey

    ,sidebarSearchBackColor = "rgb(22,26,48)"#"rgb(45,55,65)"
    ,sidebarSearchIconColor = "rgb(143,155,179)"
    ,sidebarSearchBorderColor = "rgb(45,55,65)"

    ,sidebarTabTextColor = "rgb(243,243,245)"# "rgb(205,205,205)" # "rgb(0,0,0)" menu is better
    ,sidebarTabTextSize = 16 # 14
    ,sidebarTabBorderStyle ="none none solid none" #  "none"
    ,sidebarTabBorderColor = "rgb(22,26,48)" #  "rgb(70,80,90)" #"none" #match bg
    ,sidebarTabBorderWidth = 2 #0 # space between sidebar tabs

    ,sidebarTabBackColorSelected = "rgb(34,43,69)" # "rgb(70,80,90)"
    ,sidebarTabTextColorSelected = "rgb(243,243,245)" # "rgb(255,255,255)" #white
    ,sidebarTabRadiusSelected = "5px"

    ,sidebarTabBackColorHover = "rgb(34,43,69)"# "rgb(55,65,75)"
    ,sidebarTabTextColorHover = "#428BCA" #"rgb(255,255,255)" 
    ,sidebarTabBorderStyleHover = "none none solid none" 
    ,sidebarTabBorderColorHover = "rgb(22,26,48)"
    ,sidebarTabBorderWidthHover = 2
    ,sidebarTabRadiusHover = "5px"

    ### boxes
    ,boxBackColor = "rgb(34,43,69)"#"rgb(52,62,72)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(34,43,69)"#"rgb(52,62,72)"
    ,boxPrimaryColor = "#428BCA"
    ,boxInfoColor = "#428BCA"
    ,boxSuccessColor = "#4DB848"
    ,boxWarningColor = "rgb(240,80,210)"
    ,boxDangerColor = "#D50032"

    ,tabBoxTabColor = "rgb(34,43,69)"#"rgb(52,62,72)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(243,243,245)"# "rgb(205,205,205)"
    ,tabBoxTabTextColorSelected = "#428BCA"# "rgb(205,205,205)"
    ,tabBoxBackColor = "rgb(34,43,69)"# "rgb(52,62,72)"
    ,tabBoxHighlightColor = "#428BCA" #"rgb(70,80,90)"
    ,tabBoxBorderRadius = 5

    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)" 
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5

    ,buttonBackColorHover = "#428BCA" #"rgb(180,180,180)"
    ,buttonTextColorHover = "#428BCA"
    ,buttonBorderColorHover = "#428BCA"

    ,textboxBackColor = "rgb(22,26,48)" #"rgb(34,43,69)" #"rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(34,43,69)" #"rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"

    ### tables
    ,tableBackColor = "rgb(34,43,69)"# "rgb(52,62,72)"
    ,tableBorderColor = "rgb(70,80,90)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
  )

logo_mytheme <- shinyDashboardLogoDIY(
  boldText = ""
  ,mainText = "WisDOT Crash Dashboard"
  ,textSize = 16
  ,badgeText = ""
  ,badgeTextColor = "rgb(34,43,69)"
  ,badgeTextSize = 0
  ,badgeBackColor = "rgb(34,43,69)"
  ,badgeBorderRadius = 0
)

  # WisDOT Colors
#   light_blue = "#428BCA",
#   blue = "#003087",
#   green = "#4DB848",
#   red = "#D50032",
#   yellow = "#F9C218"

# img(src = 'zero-logo.png',  style = "width: 100px; display: block; margin-left: auto; margin-right: auto;")
