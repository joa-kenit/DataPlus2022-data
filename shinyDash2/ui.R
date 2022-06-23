############################################
# ECWA Data Visualization ITERATION TWO    #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# ui.R file                                #
############################################



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
                       menuItem("Background", tabName = "background", icon = icon("leaf")),
                       menuItem("Data", icon = icon("list"),
                                menuSubItem("Data Collection", tabName = "dc", icon = icon("map")),
                                menuSubItem("Ellerbe Creek Health Status", tabName = "health", icon = icon("chart-bar")),
                                menuSubItem("Environmental Justice", tabName = "justice", icon = icon("chart-line"))),
                       menuItem("Download", tabName = "download", icon = icon("download")),
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
                fluidRow( column(width=1),column(width = 10,
                                                 includeMarkdown("www/probando.md")),
                          column(width = 1))
        ),
        
        tabItem(tabName = "dc",
                tags$h1("Data Collection"),
                tealLine,
                
                #subsection 1
                #SAMPLE GRAPH, NEEDS ADJUSTING
                tags$h3("What Data was Collected?"),
                tags$p("Text for first section"),
                
                tabBox(id = "tabset1", width="470px", height = "470px",
                       tabPanel("Trends by parameter",
                                column(12, align="center",
                                       #Insert trend tool from Durham city data 
                                       titlePanel("Time Graphs"),
                                       selectInput("Site", "Select Water Sampling Station", sites, multiple = TRUE, selected = sites[1]),
                                       selectInput("Param", "Select Parameter", Parameter),
                                       #outPut for Plot
                                       plotlyOutput("Plot"))),
                       tabPanel("Overall water quality trends",
                                leafletOutput("WQImap"),
                                sliderInput("wqiDate", "Magnitudes", min(wqiData$Date), max(wqiData$Date),
                                            value = max(wqiData$Date),
                                            step = 30,
                                            timeFormat = "%b %y",
                                            width = "100%",
                                            animate = animationOptions(interval = 100, loop = FALSE)
                                ),
                                plotlyOutput("wqiLinePlot")
                       ),
                ),
                
                #subsection 2
                tags$h3("Where was the Data Collected?"),
                tags$p("Text for second section
                       
                       
                       
                       "),
                #subsection 3
                tags$h3("How do the Data Sets Compare"),
                tags$p("Text for third section
                       
                       
                       
                       "),
        ),#end of dc
        
        tabItem(tabName = "health",
                tags$h1("Health Status of Ellrbe Creek"),
                tealLine,
                                
                       ),
        
        tabItem(tabName = "justics",
                tags$h1("Environmental Justice"),
                tealLine,
        ),
        
        tabItem(tabName = "download",
                tags$h1("Download Data"),
                tealLine,
                p("Click on the download button of any dataset you would like to explore further:",style="text-align: center"),
                br(),
                downloadButton('downloadData0','SiteLocations&Descriptions', class = "btn-block"),
                downloadButton('downloadData1', 'DataBySubbasin', class = "btn-block"),
                #style="display: block; margin: 0 auto; width: 230px;color: black;"  width: 20%;
                tags$head(tags$style(".btn-block{background-color:#202A44;color: white;width: 30%;margin-left: 35%;margin-right: 30%;height:50px;font-size: 20px;}"))  
                
        ), 
        
        
        tabItem(tabName = "team", 
                tags$h1("Team"),
                tealLine,
                fluidRow(
                  tags$head(tags$style(".headShot{border: solid 2px #202A44;  width: 100% !important;height: auto !important;}")),
                  column(width=1),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='JackPhoto.PNG',class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Jack Tsenane"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Brief bio. Bio bio bio bio bio bio. Bio bio bio bio. Bio bio bio bio. Bio bio bio bio bio bio. Bio bio bio bio bio bio."))),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='JoannaPhoto.PNG',height = "400",class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Joanna Huertas"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Brief bio. Bio bio bio bio bio bio. Bio bio bio bio. Bio bio bio bio. Bio bio bio bio bio bio. Bio bio bio bio bio bio."))),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='RyanPhoto.PNG',height = "400",class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Ryan Yu"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Brief bio. Bio bio bio bio bio bio. Bio bio bio bio. Bio bio bio bio. Bio bio bio bio bio bio. Bio bio bio bio bio bio."))),
                  column(width=1)
                )
                
                
                #Insert head shots and short bios
              
             )#end of teams subtab
        
        )#end of tabs
      
    ) # end dashboardBody
    
  )# end dashboardPage
  
)
)