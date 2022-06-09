############################################
# ECWA Data Visualization                  #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# ui.R file                                #
############################################

library(shinydashboard)
library(shinycssloaders)
library(sortable)
library(plotly)

tealLine = tags$hr(style="width:20%;text-align:left;margin-left:0;height:3px;border-width:0;background-color:#08d8b2")
con <- read.csv('www/ECWAWaterQuality2021Context.csv')
Contaminant = con$Feature[con$Type == "Contaminant"]
Infrastructure = con$Feature[con$Type == "Infrastructure"]
Demographics = con$Feature[con$Type == "Demographics"]
max_1_item_opts <- sortable_options(
  group = list(
    name = "my_shared_group",
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 1 child already
        return to.el.children.length < 1;
      }
    ")
  )
)

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
        menuItem("Background", tabName = "background", icon = icon("leaf")),
        menuItem("Data", icon = icon("list"),
                 menuItem("Overview", tabName = "overview", icon = icon("map")),
                 menuSubItem("Explore Relations", tabName = "relations", icon = icon("chart-bar")),
                 menuSubItem("Explore Trends", tabName = "trends", icon = icon("chart-line")),
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
                tags$hr(style="width:20%;text-align:center;height:3px;border-width:0;background-color:#08d8b2"),
                
                #Insert text for home page and Ellerbe creek image
                includeMarkdown("www/home.md")
        ),
        
        tabItem(tabName = "background",
                tags$h1("Pollutant Information"),
                tealLine,
                
                #Insert text for home page and Ellerbe creek image
                includeMarkdown("www/probando.md")
        ),
        
        tabItem(tabName = "overview",
                tags$h1("Overview of Data"),
                tealLine,
        
                tags$div(
                  HTML('<iframe src="https://dukeuniv.maps.arcgis.com/apps/instant/minimalist/index.html?appid=0ccb2a3586e640dbbeabab10fa218c62" width="95%" height="600" frameborder="0" style="border:0" allowfullscreen>iFrames are not supported on this page.</iframe>')
                )
                
        ),
        
        tabItem(tabName = "relations",
                tags$h1("Compare Data"),
                tealLine,
                
                tabBox(id = "tabset1", width="940px", height = "1400px",
                       tabPanel("Summary of correlations",  
                                column(12, align="center",plotOutput("corTable"))),
                       
                       tabPanel("Explore correlations", 
                                fluidRow(column(width = 9, plotlyOutput("value2")),column(width=3, 
                                                                                          box(width = 12, background = 'navy',
                                                                                              textOutput(outputId="text1"),
                                                                                              textOutput(outputId="text2"),
                                                                                              textOutput(outputId="text3")))),
                                fluidRow(
                                  column(width = 3,
                                         rank_list(
                                           text = "Contaminants",
                                           labels = Contaminant,
                                           input_id = "main_list1",
                                           options = sortable_options(group = "my_shared_group")
                                         )),
                                  column(width = 3,
                                         rank_list(
                                           text = "Infrastructure & Environment",
                                           labels = Infrastructure,
                                           input_id = "main_list2",
                                           options = sortable_options(group = "my_shared_group")
                                         ),
                                         rank_list(
                                           text = "Demographics",
                                           labels = Demographics,
                                           input_id = "main_list3",
                                           options = sortable_options(group = "my_shared_group")
                                         )),
                                  column(width = 6,
                                         rank_list(
                                           text = "X axis",
                                           labels = c(),
                                           input_id = "list_1",
                                           options = max_1_item_opts
                                         ),
                                         rank_list(
                                           text = "Y axis",
                                           labels = c(),
                                           input_id = "list_2",
                                           options = max_1_item_opts
                                         ),
                                         rank_list(
                                           text = "Z axis (optional)",
                                           labels = c(),
                                           input_id = "list_3",
                                           options = max_1_item_opts
                                           
                                         )))
                                #fluidRow(textOutput(outputId="text"))
                                #,textOutput(outputId="text")))
                                
                                
                       )
                )
        ),
        
        tabItem(tabName = "trends",
                tags$h1("Look at Data Over Time"),
                tealLine
              
                #Insert trend tool from Durham city data 
          
        ),
      
        tabItem(tabName = "download",
                tags$h1("Download Data"),
                tealLine,
          
                #Insert mechanism to download data
                titlePanel("Explore"),
          
                selectInput("dataset", "Pick a dataset", ls("package:datasets")), 
          helpText("Click on the download button to download dataset observations:"),
          div(style="display:inline-block",downloadButton('downloadData0','Site Summary Table', class = "btn-block"), style="float:right"),
          downloadButton('downloadData1', 'Variable Table', class = "btn-block")
         
        ), 
        
        
        tabItem(tabName = "team", 
                tags$h1("Team"),
                tealLine,
                
                #Insert head shots and short bios
  
      ))
    
    ) # end dashboardBody
  
  )# end dashboardPage

))