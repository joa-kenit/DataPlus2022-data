############################################
# ECWA Data Visualization                  #
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
    
    skin = "blue",
      
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
                fluidRow( column(width=1),column(width = 10,
                includeMarkdown("www/probando.md")),
                column(width = 1))
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
                                                                                              # dropdownButton("0 indicates no linear correlation. 1 indicates high linear correlation. 0-0.25 is often considered low correlation. 0.5-1 is often considered high correlation.", 
                                                                                              #                status = 'success', icon = icon('question'),style="color: navy"),
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
                tealLine,
                tabBox(id = "tabset1", width="940px", height = "940px",
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
        )),
      
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
  
      ))#end of tabs
    
    ) # end dashboardBody
  
  )# end dashboardPage

))