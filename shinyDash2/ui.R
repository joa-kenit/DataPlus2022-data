############################################
# ECWA Data Visualization ITERATION TWO    #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# ui.R file                                #
############################################





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
    
    #skin = "green",
    
    dashboardHeader(title="Ellerbe Creek Watershed", titleWidth = 300),
    
    dashboardSidebar(
      #tags$head(tags$style(HTML(".main-sidebar {background-color: #051040 !important;}"))), #Color sidebar
      tags$head(tags$style(HTML('.logo {
                              background-color: #022a25 !important;
                              }
                              .navbar {
                              background-color: #022a25 !important;
                              }
                              '))),
      width = 342,
      #Change background color #004058
      tags$head(tags$style(HTML('/* body */
                                 .content-wrapper, .right-side {
                                 background-color: #08d8b2;
                                 }'))),
      sidebarMenu(
        #ECWA Logo
        HTML(paste0("<br>","<a href='https://www.ellerbecreek.org/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logoECWA.png' width = '186'></a>","<br>")),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Background", tabName = "background", icon = icon("leaf")),
        menuItem("Data", icon = icon("list"),
                 menuSubItem("Data Collection", tabName = "dc", icon = icon("map")),
                 menuSubItem("Ellerbe Creek Health Status", tabName = "health", icon = icon("chart-bar")),
                 menuSubItem("Environmental Justice", tabName = "justice", icon = icon("chart-line"))),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Team", tabName = "team", icon = icon("users")))), # end dashboardSidebar
    
    dashboardBody(
      
      #Change background color
      tags$head(tags$style(HTML('/* body */.content-wrapper, .right-side {background-color: #f0fefb;}'))), 
      
      tabItems(
        tabItem(tabName = "home",
                #Header
                tags$h1("Ellerbe Creek Watershed Data", style = "text-align:center;"),
                tags$hr(style="width:20%;text-align:center;height:3px;border-width:0;background-color:#08d8b2"),
                
                #Read in home page content
                includeMarkdown("www/home.md")
        ),
        
        tabItem(tabName = "background",
                #Header
                tags$h1("Pollutant Information"),
                tealLine,
                
                #Read in pollutant page content
                fluidRow(column(width=1),column(width = 10,includeMarkdown("www/probando.md")),column(width = 1))
        ),
        
        tabItem(tabName = "dc",
                #Header
                tags$h1("Data Collection"),
                tealLine,
                
                #Subsection 1
                tags$h3("What Data was Collected?"),
                tags$p("Text for first section"),
                fluidRow(column(12, align="center", tableOutput('tableSources'))),
                #subtext
                tags$h4("City of Durham - Sites"),
                tags$h5("EL1.9EC – Glen Road"), 
                tags$h5("EL5.0EC– Club Boulevard"), 
                tags$h5("EL5.5GC – Camden Avenue "),
                tags$h5("EL5.6EC – Midland Terrace "),
                tags$h5("EL7.1EC – Club Boulevard and Acadia Street"),
                tags$h5("EL7.1SEC – Glendale Avenue"),
                tags$h5("EL7.9EC – Murray Avenue"),
                tags$h5("EL8.1GC – Holloway Street"),
                tags$h5("EL8.5SEC – Onslow Street and Club Boulevard"),
                tags$h5("EL8.6SECUT – Foster Street and Hunt Street"),
                tags$h5("EL10.7EC – Bellevue Avenue"),
                
                #Subsection 2
                tags$h3("Where was the Data Collected?"),
                tags$p("Water quality data is collected accross the whole Ellerbe Creek Watershed to study the changes in water quality and find possible patterns. The data stations are divided in two type of sites: Durham Sites and Survey Sites (Synoptic data). 
                Both of which share certain parameters and have different sampling frequency."),
                tags$div(HTML('<center><iframe src="https://dukeuniv.maps.arcgis.com/apps/instant/minimalist/index.html?appid=0ccb2a3586e640dbbeabab10fa218c62" width="80%" height="600" frameborder="0" style="border:0" allowfullscreen>iFrames are not supported on this page.</iframe><center>')),
                
                #subsection 3
                tags$h3("How do the Data Sets Compare?"),
                tags$p("Text for third section"),
                tags$p("[Insert boxplot here]")
        ),
        
        tabItem(tabName = "health",
                #Header
                tags$h1("Health Status of Ellerbe Creek"),
                tealLine,
                
                #Subsection 1
                tags$h3("What are the primary types of pollution?"),
                p("This application uses the Durham stations.It contains data from 2016 to 2022."),
                tags$h3("Key Pollution Indicators (Demo)",style="text-align: center"),
                fluidRow(column(width = 3, 
                                box(width = "100%", background = "navy",
                                    selectInput("prods", "Select parameter", choices = c("Fecal.Coliform", "Total.Phosphorus", "Zinc"), multiple = TRUE),
                                    selectInput("type", "Chart type", choices = c("bar","polar-area")),
                                    checkboxInput("labels", "Show values"))),
                         column(width = 9, box(width = "100%", background = "navy",leafletOutput("param3map")))),
                
                #Subsection 2
                tags$h3("What is a water quality index?"),
                tags$p("[Insert text here]"),
                
                #Subsection 3
                tags$h3("How does the Ellerbe Creek Watershed compare to local water?"),
                tags$p("[Insert text here]"),
                
                #Subsection 4
                tags$h3("How does the Ellerbe Creek Watershed compare to local water?"),
                tags$p("[Insert text here]"),
                
                #Subsection 5
                tags$h3("How has the Ellerbe Creek Watershed changed over time??"),
                tags$p("[Insert text here]"),
                tags$h3("Water Quality Measurements Over Time",style="text-align: center"),
                #Insert trend tool from Durham city data 
                fluidRow(column(3,box(width = "100%", background = "navy",
                                      selectInput("Site", "Select Water Sampling Station", sites, multiple = TRUE, selected = sites[1]),
                                      selectInput("Param", "Select Parameter", Parameter))),
                         column(9, align="center",
                                #outPut for Plot
                                box(width = "100%", background = "navy",
                                    plotlyOutput("Plot")))),
                br(),
                tags$p("[Insert text here]"),
                br(),
                column(12, align="center",
                       titlePanel("Water Quality Index over time"),
                       box(width = "100%", background = "navy",
                           leafletOutput("WQImap")),
                       tags$style(type="text/css", ".slider1 .irs-grid-text{font-size: 14px;}
                                     .slider1 .irs-bar {border-color: black; background-color: #08d8b2;}
                                     .slider1 .irs-bar-edge { border-color: red; background-color: red;}"),
                       box(width = "100%", background = "navy",
                           tags$div(id = "slider1", class="slider1",
                                    sliderInput("wqiDate", "Magnitudes", min(wqiData$Date), max(wqiData$Date),
                                                value = max(wqiData$Date),
                                                step = 30,
                                                timeFormat = "%b %Y",
                                                width = "90%",
                                                animate = animationOptions(interval = 100, loop = FALSE))))
                       #plotlyOutput("wqiLinePlot")
                ),
                tags$p("[Insert more text here]"),
                
                
                #bargraph     
                br(),
                tags$p("In the following graph we show the number of water quality samples (n) that comply or not with the National Recommended Water Quality Criteria - Aquatic Life Criteria Table.
                        # U.S. Environmental Protection Agency | US EPA, 2022."),
                br(),
                
                column(12, align="center",
                       titlePanel("Regulation compliance by Parameter - Aquatic Life Criteria")),
                
                fluidRow(column(3,box(width = "100%", background = "navy",
                                      
                                      selectInput("Parameter", "Select Water Contaminant: ", parameters),
                                      # hr(),
                                      # helpText("Regulation: National Recommended Water Quality Criteria - Aquatic Life Criteria Table.
                                      # U.S. Environmental Protection Agency | US EPA, 2022.")
                                      #         )
                                      
                )),
                column(9, align="center",
                       #outPut for Plot
                       box(width = "100%", background = "navy",
                           plotlyOutput("barPlot"))))
                
        ),
        
        ###########
        tabItem(tabName = "justice",
                tags$h1("Environmental Justice"),
                tealLine,
                
                #Subsection 1
                tags$h3("What is environmental justice?"),
                tags$p("[Insert text here]"),
                
                #Subsection 2
                tags$h3("How has redlining affected Durham??"),
                tags$p("[Insert text here]"),
                tags$p("[Insert redlining image here]"),
                
                #Subsection 3
                tags$h3("What other relationships exist?"),
                fluidRow(column(12, align="center",box(width = "55%", background = "navy",plotOutput("corTable")))),
                tags$p("[Insert text here]"),
                tags$p("[Insert text here]"),
                fluidRow(uiOutput("synced_maps")),
                fluidRow(                                  
                  column(width = 2,
                         rank_list(
                           text =  c("Contaminant", radioButtons("season", "Sampling Season:",
                                                                 c("Fall" = "fall",
                                                                   "Winter" = "wint",
                                                                   "Summer" = "summ"),
                                                                 inline = FALSE,
                                                                 selected = "wint")),
                           labels = Contaminant,
                           input_id = "main_list1",
                           options = sortable_options(group = "my_shared_group")
                         )),
                  column(width = 2,
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
                           options = sortable_options(group = "my_shared_group"))
                  ), column(width=2,                                         
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
                              
                            )),
                  column(width = 5,plotlyOutput("value2"), 
                         box(width = 12, background = 'navy',
                             materialSwitch(inputId = "bestFitSwitch", label = "Show best fit linear equation"),
                             textOutput(outputId="text1"), 
                             # dropdownButton("0 indicates no linear correlation. 1 indicates high linear correlation. 0-0.25 is often considered low correlation. 0.5-1 is often considered high correlation.", 
                             #                status = 'success', icon = icon('question'),style="color: navy"),
                             textOutput(outputId="text2"),
                             textOutput(outputId="text3"),
                             tags$head(tags$style("#text1#text2{font-size: 12px;}"))))) 
                
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