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
      #background color
      tags$head(tags$style(HTML('.logo {background-color: #022a25 !important;}
                                 .navbar {background-color: #022a25 !important;}'))),
      width = 342,
                     #Change background color #004058
                     tags$head(tags$style(HTML('/* body */
                                 .content-wrapper, .right-side {
                                 background-color: red;
                                 }'))),
                     sidebarMenu(
                       #ECWA Logo
                       HTML(paste0("<br>","<a href='https://www.ellerbecreek.org/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logoECWA.png' width = '186'></a>","<br>")),
                       
                       #Tab formatting
                       menuItem("Home", tabName = "home", icon = icon("home")),
                       menuItem("Data", icon = icon("list"),
                                menuSubItem("Locations + Data Source", tabName = "dc", icon = icon("map")),
                                menuSubItem("Water Quality through Time", tabName = "health", icon = icon("chart-bar")),
                                menuSubItem("Water Quality though Space", tabName = "justice", icon = icon("chart-line"))),
                       menuItem("Additional Resources", icon = icon("list"),
                                menuSubItem("Pollutant Information", tabName = "background", icon = icon("leaf")),
                                menuSubItem("Scientific Deep Dive - PCA", tabName = "PCA", icon = icon("chart-area")),
                                menuSubItem("Scientific Deep Dive - Tracer Pollution", tabName = "param3", icon = icon("chart-area"))),
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
        
        tabItem(tabName = "dc",
                #Header
                tags$h1("Locations + Data Source"),
                tealLine,
                
                #Subsection 1
                tags$h3("What Data was Collected?"),
                tags$p("Various government agencies and research groups are interested in monitoring and understanding the ability of waterways to sustain the community. This platform combines data collected by the city of Durham and several researchers at Duke University. A summary of the data contained in the datasets is shown in the table below."),
                fluidRow(column(12, align="center", tags$h4("Table 1. Summary of Water Quality Datasets"),tableOutput('tableSources'))),
                
                #Subsection 2
                tags$h3("Where was the Data Collected?"),
                tags$p("Water quality data is collected accross the whole Ellerbe Creek Watershed to study the changes in water quality and find possible patterns. The data stations are divided in two type of sites: Durham Sites and Survey Sites (Synoptic data). 
                Both of which share certain parameters and have different sampling frequency."),
                tags$div(HTML('<center><iframe src="https://dukeuniv.maps.arcgis.com/apps/instant/minimalist/index.html?appid=0ccb2a3586e640dbbeabab10fa218c62" width="80%" height="600" frameborder="0" style="border:0" allowfullscreen>iFrames are not supported on this page.</iframe><center>')),
                
                #subsection 3
                tags$h3("How do the Data Sets Compare?"),
                tags$p("As is shown in Table 1 above, each dataset was collected with different goals. As a result, each dataset is capable of telling a different story. The city of Durham has collected data for many years allowing us to understand changes over the past two decades. Duke researchers have collected data in each subbasin of the Ellerbe Creek watershed, allowing for a greater understanding of where pollution issues may be occurring relative to the community. Some water quality measurements can change drastically throughout the day. For example, dissolved oxygen will change throughout the day due to the temperature of the stream and biological activity in the stream. The StreamPulse dataset is designed to capture the variation that happens throughout the day. A comparison of the spatial and temporal variation that is captured by these datasets is shown in the interactive graphic below."),
                tags$h3("Variability of Sampling Sites",style="text-align: center"),
                fluidRow(column(3, align="left",
                         #Boxplot selection
                         box(width = "100%", background = "navy",selectInput("ParamBoxplots", "Select Parameter", matchedparams))),
                         #Boxplot
                         column(9, align = "center",
                         box(width = "100%", background = "navy", plotlyOutput("Boxplots")))),
                tags$p("*Note: Number of sites shown on the left side of the graph corresponds to the specific sites data was collected at for a specific parameter")
                ),
        
        tabItem(tabName = "health",
                #Header
                tags$h1("Water Quality through Time"),
                tealLine,
            
                #Subsection 1
                tags$h3("How does the Ellerbe Creek Watershed compare to local water?"),
                tags$p("The environmental protection agency (EPA) has created a tool titled “How’s My Waterway?” to understand where waterways that have been declared impaired are relative to the community. The EPA graphic is shown below. The graphic shows that many waterways in populated areas are impaired. However, there are also many streams that are doing well."),
                tags$h3("EPA: How's My Waterway?",style="text-align: center"),
                tags$head(tags$style(".testImage{border: solid 10px #202A44;  width: 100% !important;height: auto !important;}")),
                fluidRow(column(width = 3),column(width=6,img(src='hamww.PNG',class="testImage"))),
                tags$div(HTML('<center><iframe src="https://www.epa.gov/sites/production/files/widgets/mywaterway/index.html" id="mywaterway" width="220" height="170" frameborder="0" style="border:0" allowfullscreen>iFrames are not supported on this page.</iframe><center>')),
                
                #Subsection 2
                tags$h3("What is a water quality index?"),
                tags$p("A water quality index is a number ranging from 0 (perfectly uninhabitable) to 100 (perfectly habitable) that looks at various proxy measurements (hardness, nitrogen content, phosphorus content, dissolved oxygen, etc) to get an idea of how suitable the water is for the ecosystem. The problem is that the requirements of every ecosystem can differ. A lot of information can be lost trying to summarize everything that goes into making a good waterway down to one number. However, we do get a good starting point as to how the water is doing. The locations with the associated water quality is shown in the figure below. More information on how the waterquailt index is used in Durham is discussed in the next section"),
                
                #Subsection 3
                tags$h3("How has the Ellerbe Creek Watershed changed over time?"),
                tags$p("While the water quality index is limited in what it can do, we do get a good starting point as to how the water is doing. The sampling locations within the Ellerbe Creek Watershed with the associated water quality are shown in the figure below."),
                fluidRow(
                  column(width=2),
                  column(8, align="center",
                         tags$h3("Water Quality Index over time",style="text-align: center"),
                         #WQI Map
                         box(width = "100%", background = "navy",leafletOutput("WQImap")),
                         #WQI slider
                         box(width = "100%", background = "navy",
                             tags$div(id = "slider1", class="slider1",
                                      sliderInput("wqiDate", "Magnitudes", min(wqiData$Date), max(wqiData$Date),
                                                  value = max(wqiData$Date),
                                                  step = 30,
                                                  timeFormat = "%b %Y",
                                                  width = "90%",
                                                  animate = animationOptions(interval = 100, loop = FALSE))))),
                  column(width=2)),
                
                br(),
                tags$p("Again, a lot of information is lost when one attempts to summarize the quality of water with only a handful of metrics. For a more detailed breakdown of how individual water quality parameters vary over time, use the interactive graphic below."),
                br(),
                tags$p("Some spikes can be seen in some water quality metrics. This is expected because some samples will be taken during short term events like a rain storm or a nearby construction project. Short term spikes are not always indicative of unhealthy waterways."),
                br(),
                tags$p("In the following graph we show the number of water quality samples (n) that comply or not with the National Recommended Water Quality Criteria - Aquatic Life Criteria Table.
                        # U.S. Environmental Protection Agency | US EPA, 2022."),
                br(),
                
                fluidRow(column(width=3,box(width = "100%", background = "navy",
                                      selectInput("Site", "Select Water Sampling Station", ambientSites, multiple = TRUE, selected = ambientSites[1]),
                                      selectInput("Param", "Select Parameter", ambientParameter),
                                      materialSwitch(inputId = "aggregateSwitch", label = "Aggregate data across sites (barchart only)"))),
                                      
                         column(9, align="center",
                                #Trendline
                                tags$h3("Water Quality Measurements Over Time",style="text-align: center"),
                                box(width = "100%", background = "navy",
                                    plotlyOutput("Plot")),
                                #Regulation Barchart
                                tags$h3("Regulation compliance by Parameter - Aquatic Life Criteria",style="text-align: center"),
                                box(width = "100%", background = "navy",
                                    plotlyOutput("barPlot"))))
                ),
        
        tabItem(tabName = "justice",
                tags$h1("Water Quality though Space"),
                tealLine,
                
                #Subsection 1
                tags$h3("What is environmental justice?"),
                tags$p("According to the EPA, environmental justice is “the fair treatment and meaningful involvement of all people regardless of race, color, national origin, or income with respect to the development, implementation and enforcement of environmental laws, regulations and policies.” It is a goal of many governmental organizations to ensure that everyone within a community benefits equitably from what the environment has to offer and to ensure that is the case for future generations as well."),
                
                #Subsection 2
                tags$h3("How has redlining affected Durham?"),
                tags$p("Redlining is when financial institutions carve out neighborhoods where certain people would not be allowed to purchase homes. This practice has been historically used to discriminate against racial minorities and low income individuals. The effects of redlining are still apparent in cities today. The figure below shows where historical redlining performed in Durham overlaps with the Ellerbe Creek watershed. The redlined communities suffered from historical underinvestment, which can lead to effects that propagate into current day pollution. An example of this is some pipes in Durham have not been replaced in over 100 years in low income areas. The result is low income areas will have higher rates of wastewater pollution from sewage pipes leaking."),
                tags$h3("Redlining Overlapped on top of the Ellerbe Creek Watershed",style="text-align: center"),
                fluidRow(column(width=2),column(width =8, box(width = "100%",background = "navy", leafletOutput("redliningLeaflet"))),column(width=2)),
                
                #Subsection 3
                tags$h3("What other relationships exist?"),
                tags$p("Duke researchers have mapped out pollution within the Ellerbe creek  watershed. The graph below shows the linear correlation between each of the variables with “x’s” through all of the correlations that are not statistically significant."),
                tags$h3("Correlation table of variables within the Ellerbe Creek WaterShed",style="text-align: center"),
                fluidRow(column(width=5,box(width = "100%",background = "navy", plotOutput("corTableDemo"))),
                         column(width = 2,box(width = "100%",background = "navy",prettyRadioButtons("seasonCorr", "Season of Sampling of Water Quality Measurments",
                                                                                                    choices = synopticSeasons,
                                                                                                    shape = "curve",
                                                                                                    animation = "smooth",
                                                                                                    inline = FALSE,
                                                                                                    selected = synopticSeasons[1]))),
                         column(width=5,box(width = "100%",background = "navy", plotOutput("corTableInfra")))),

                tags$p("You can use the tool below to understand how different water quality measurements relate to the infrastructure and demographics of the region. To inform your comparison, you can use the correlation plot above. Alternatively, you can investigate common research questions:"),
                tags$p("Is there more excess nutrients (nitrogen, phosphorus) from fertilizer where there is more cultivated farm land?"),
                tags$p("Is there a stronger correlation between where salt is and where roads are in the winteror the summer?"),
                tags$h3("Variable Comparison Tool",style="text-align: center"),
                fluidRow(column(12,align="center",box(width = "100%", background = "navy",uiOutput("synced_maps", width="100%")))),
                fluidRow(column(width=6,
                                box(width = "100%", background = 'navy',
                                    prettyRadioButtons("season", "Season of Sampling of Water Quality Measurments",
                                                   choices = synopticSeasons,
                                                   shape = "curve",
                                                   animation = "smooth",
                                                   inline = FALSE,
                                                   selected = synopticSeasons[1])),
                                uiOutput("dragAndDropListChloro")
                                ),
                         column(width = 6,box(width = "100%", background = "navy",plotlyOutput("value2")), 
                           box(width = "100%", background = 'navy',
                           materialSwitch(inputId = "bestFitSwitch", label = "Show best fit linear equation"),
                           textOutput(outputId="text1"), 
                           textOutput(outputId="text2"),
                           textOutput(outputId="text3"),
                           tags$head(tags$style("#text1#text2{font-size: 12px;}")))))
                
        ),
        tabItem(tabName = "background",
                #Header
                tags$h1("Pollutant Information"),
                tealLine,
                
                #Read in pollutant page content
                fluidRow(column(width=1),column(width = 10,includeMarkdown("www/probando.md")),column(width = 1))),
        
        tabItem(tabName = "PCA",
                tags$h1("Principal Component Analysis"),
                tealLine,
                
                tags$h3("Understanding a Principle Component Analysis",style="text-align: center"),
                tags$p("The purpose of this Principle Component Analysis (PCA) 
                       is to understand the clustering of our data with respect to 
                       more than two parameters on a two dimensional plot. The vectors 
                       of contaminants on the plot indicate their magnitude of influence
                       on the variance of the data points. The axes are the first two 
                       principle components, and they represent the first, and second largest
                       directions of variance in the data."),
                
                tags$h3("Synoptic Data PCA",style="text-align: center"),
                fluidRow(column(3,box(width = "100%", background = "navy", selectInput("PCAparams", "Select Shade Parameter", pca_params, selected = pca_params[4]))),
                         column(9,box(width = "100%", background = "navy", plotOutput("PCA")))),
                tags$p('This specific PCA is designed to highlight potential influence that the factors road density, 
                pipe density, percent impervious surfaces, percent developed, and median house hold income have on the synoptic data set. 
                Along with the visual of which contaminants have a large impact on the variability of data points, the color shade allows the user to see if these specific factors are 
                influenced by the same contaminants.'),
                
                tags$h3("Synoptic Data PCA (By Sample Date)",style="text-align: center"),
                fluidRow(column(3,box(width = "100%", background = "navy", selectInput("PCAdates", "Select Sampling Dates", synopticDates, multiple = TRUE, selected = synopticDates[1]))),
                        column(9,box(width = "100%", background = "navy", plotOutput("PCA2")))),
                tags$p('The goal with the sampling dates PCA is to explore the variability in the synoptic data set between the three specific sampling dates. 
                       Clustering among the sampling dates could indicate how different seasons affect the sampling results of the data.'),
                
                tags$h3("Ambient Data PCA",style="text-align: center"),
                fluidRow(column(3,box(width = "100%", background = "navy", selectInput("PCAsites", "Select Water Sampling Station", ambientSites, multiple = TRUE, selected = ambientSites[1]))),
                         column(9,box(width = "100%", background = "navy", plotOutput("PCA3")))),
                tags$p('The sampling site PCA is designed to explore clustering among the eight sampling sites that are monitored in the ambient data set. 
                Variability in the data over space is an important aspect to consider in analysis as the sampling sites within the water shed can vary in features and water sourcing.')
        ),
        
        tabItem(tabName = "param3",
                tags$h1("Tracer Pollution"),
                tealLine,
                
                #Subsection 1
                tags$h2("What are the primary types of pollution?",style="text-align: center"),
                p("Environmental scientists use various pollution metrics to get an idea of the pollution landscape. The sources of pollution can be inferred by looking at particular contaminants known to come from unique sources. Sucralose is found in wastewater. Dipropylene glycol is a tire additive, and is therefore expected to be associated with car pollution. Glyphosate is used in the herbicides homeowners would put on their lawn. In conjunction, we can get an idea of where certain types of pollution should be expected. This is shown in the interactive figure below."),
                tags$h2("Normalization method",style="text-align: center"),
                tags$p("In order to allow users to simultaneously compare all three tracers at the same time on the same axis, the values have been normalized by subtracting the historical mean and dividing by the historical standard deviation. To ensure that all values are positive for polar radius graphs and that outliers don't make the remaining data uninterpretable, the z score was mapped to a value between 0 and 1 using the logistic function shown below."),
                fluidRow(column(width=3),
                         column(width=6,box(width = "100%", background = "navy", plotlyOutput("logisticPlot"))),
                         column(width=3)),
                tags$h3("Key Pollution Indicators (Demo)",style="text-align: center"),
                fluidRow(column(width = 3, 
                                box(width = "100%", background = "navy",
                                    selectInput("prods", "Select parameter", choices = c("Ammonium","Chloride", "Sulfate"),selected = "Ammonium", multiple = TRUE),
                                    selectInput("type", "Chart type", choices = c("polar-area","polar-area","bar"),selected = "bar"),
                                    checkboxInput("labels", "Show values"))),
                         column(width = 9, box(width = "100%", background = "navy",leafletOutput("param3map")))),
                
        ),
        tabItem(tabName = "download",
                tags$h1("Download Data"),
                tealLine,
                p("Click on the download button of any dataset you would like to explore further:",style="text-align: center"),
                br(),
                downloadButton('downloadData0','SiteLocations&Descriptions', class = "btn-block"),
                downloadButton('downloadData1', 'DataBySubbasin', class = "btn-block"),
                a("GitHub Codebase", href="https://github.com/joa-kenit/DataPlus2022-data", class = "btn-block"),
                #style="display: block; margin: 0 auto; width: 230px;color: black;"  width: 20%;
                tags$head(tags$style(".btn-block{background-color:#202A44;color: white;width: 30%;margin-left: 35%;margin-right: 30%;height:50px;font-size: 20px;text-align: center;}"))  
                
        ), 
        
        
        tabItem(tabName = "team", 
                tags$h1("Team"),
                tealLine,
                tags$h2("Coding Team",style="text-align: center"),
                fluidRow(
                  tags$head(tags$style(".headShot{border: solid 2px #202A44;  width: 100% !important;height: auto !important;}")),
                  column(width=1),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='JackPhoto.PNG',class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Jack Tsenane"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Jack studied chemical engineering at Vanderbilt University. He first began river monitoring for the Wisconsin DNR in middle school. Jack enjoys employing data science tools to better understand and improve the world.",style="text-align:left;padding:5px;border-radius:10px"))),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='JoannaPhoto.PNG',height = "400",class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Joanna Huertas"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Joanna is an environmental engineer with more than four years of experience in environmental consulting. She is currently pursuing a Master’s degree in Environmental Engineering at Duke University. Her experience in private, academic, and government sectors in regards to water quality compliance aids the technical underpinning of the display of water quality monitoring results." ,style="text-align:left;padding:5px;border-radius:10px"))),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='RyanPhoto.PNG',height = "400",class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Ryan Yu"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Ryan is a rising sophomore at Duke University who has taken coursework in mathematics, computer science, and statistical science. In addition to these areas, he is exploring finance and economics. Ryan is planning on declaring for a major in Computer Science and Statistics in the Spring of 2023." ,style="text-align:left;padding:5px;border-radius:10px"))),
                  column(width=1),
                  ),
                tags$h2("Acknowledgments",style="text-align: center"),
                tags$p("This project was made possible by the extensive support of the Duke River Center. Namely, Jonathan Behrens, Steven Anderson, and Emily Bernhardt. These people had the roles of project manager, project mentor, and project lead. We would also like to thank the Ellerbe Creek Watershed Association for their collaboration.")
      
        )#end of team subtab
        
      )#end of tabs
      
    )#end dashboard body
    
  )#end dashboard page
  
)
)