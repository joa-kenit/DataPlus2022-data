############################################
# ECWA Data Visualization                  #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# ui.R file                                #
############################################

library(shinydashboard)
library(shinycssloaders)

tealLine = tags$hr(style="width:20%;text-align:left;margin-left:0;height:3px;border-width:0;background-color:#08d8b2")

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
  
  # load page layout
  dashboardPage(
    
    skin = "green",
      
    dashboardHeader(title="Ellerbe Creek Watershed", titleWidth = 300),
    
    dashboardSidebar(width = 342,
                     #Change background color
                     tags$head(tags$style(HTML('/* body */
                                 .content-wrapper, .right-side {
                                 background-color: #f0fefb;
                                 }'))),
      sidebarMenu(
        #ECWA Logo
        HTML(paste0(
          "<br>",
          "<a href='https://www.nps.gov/index.htm' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logoECWA.png' width = '186'></a>",
          "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Data", icon = icon("list"),
                 menuItem("Overview", tabName = "overview", icon = icon("map")),
                 menuSubItem("Explore Relations", tabName = "relations", icon = icon("bar-chart")),
                 menuSubItem("Explore Trends", tabName = "trends", icon = icon("line-chart")),
                 menuSubItem("Download", tabName = "download", icon = icon("download"))
        ),
        menuItem("Team", tabName = "team", icon = icon("users"))
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      
      #Change background color
      tags$head(tags$style(HTML('/* body */
                                 .content-wrapper, .right-side {
                                 background-color: #f0fefb;
                                 }'))),
      
      tabItems(
        
        tabItem(tabName = "home",
                tags$h1("Ellerbe Creek Watershed Data", style = "text-align:center;"),
                tags$hr(style="width:20%;text-align:center;height:3px;border-width:0;background-color:#08d8b2")
                
                #Insert text for home page and Ellerbe creek image
          
        ),
        
        tabItem(tabName = "overview",
                tags$h1("Overview of Data"),
                tealLine,
        
                #Insert text and simplified arcGIS embedding
                tags$div(
                  HTML('<iframe src="https://dukeuniv.maps.arcgis.com/apps/instant/minimalist/index.html?appid=0ccb2a3586e640dbbeabab10fa218c62" width="95%" height="600" frameborder="0" style="border:0" allowfullscreen>iFrames are not supported on this page.</iframe>')
        )),
        
        tabItem(tabName = "relations",
                tags$h1("Compare Data"),
                tealLine
          
                #Insert variable comparison tool from Jonny's team's data
        ),
        
        tabItem(tabName = "trends",
                tags$h1("Look at Data Over Time"),
                tealLine
              
                #Insert trend tool from Durham city data 
          
        ),
      
        tabItem(tabName = "download",
                tags$h1("Download Data"),
                tealLine
          
                #Insert mechanism to download data
          
        ), 
        
        
        tabItem(tabName = "team", 
                tags$h1("Team"),
                tealLine
                
                #Insert head shots and short bios
              
      ))
    
    ) # end dashboardBody
  
  )# end dashboardPage

))