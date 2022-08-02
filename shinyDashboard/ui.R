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
                tags$p("Various government agencies and research groups are interested in monitoring and understanding the ability of waterways to sustain human and wildlife communities. This platform combines data collected by the city of Durham and several researchers at Duke University. A summary of the data contained in the datasets is shown in the table below. Please appropriately cite any data or figures taken from this site."),
                fluidRow(column(12, align="center", tags$h4("Table 1. Summary of Water Quality Datasets"),tableOutput('tableSources'))),
                
                #Subsection 2
                tags$h3("Where was the Data Collected?"),
                tags$p("Water quality data is collected throughout the Ellerbe Creek Watershed to study the changes in water quality and find possible patterns. The data stations are divided into: Ambient Sampling (Durham Sites) and Synoptic Sampling Sites (Duke Survey Sites). 
                Both of which share certain parameters and have different sampling frequency."),
                tags$div(HTML('<center><iframe src="https://dukeuniv.maps.arcgis.com/apps/instant/minimalist/index.html?appid=0ccb2a3586e640dbbeabab10fa218c62" width="80%" height="600" frameborder="0" style="border:0" allowfullscreen>iFrames are not supported on this page.</iframe><center>')),
                
                #subsection 3
                tags$h3("How do the Data Sets Compare?"),
                tags$p("As is shown in Table 1 above, each dataset was collected with different goals. As a result, each dataset is capable of telling a different story. In the Ambient Sampling dataset, the City of Durham collected data for many years at a few sites, allowing us to understand changes over the past two decades. The Synoptic Sampling dataset was collected by Duke researchers at many sites (each subbasin of the Ellerbe Creek watershed), but only a few points in time, allowing for a more detailed understanding of how water quality changes spatially throughout the watershed. Furthermore, some water quality measurements can change drastically throughout the day. For example, dissolved oxygen will change throughout the day due to the temperature of the stream and biological activity in the stream. Thus, a comparison of the spatial and temporal variation that is captured by these datasets is shown in the interactive graphic below."),
                tags$h3("Variability of Sampling Sites",style="text-align: center"),
                fluidRow(column(3, align="left",
                         #Boxplot selection
                         box(width = "100%", background = "navy",selectInput("ParamBoxplots", "Select Parameter", matchedparams))),
                         #Boxplot
                         column(9, align = "center",
                         box(width = "100%", background = "navy", plotlyOutput("Boxplots")))),
                tags$p("*Note: The number of sites shown on the left side of the graph will change depending on data availability for a specific parameter",style="text-align: right"),
                
                #subsection 3
                tags$h3("What Other Data Exists?"),
                tags$p("While only these two datasets are used to visualize water quality on this website, other datasets do exist. Below are a few examples."),
                tags$div(HTML('<ul style ="font-family: sans-serif;font-size: 18px;font-weight: 500;line-height: 1.75;margin: 0; auto 30px;"><li><a href="https://www.epa.gov/waterdata/storage-and-retrievalwater-quality-exchange-related-links"><b>EPA STORET sites</b></a> Inclusive of one-off studies and ongoing regulatory monitoring in the watershed. Monitoring is variable across sites, with some sites only sampled once. </li><li><a href="https://www.usgs.gov/publications/design-and-methods-southeast-stream-quality-assessment-sesqa-2014"><b>USGS SESQA 2014</b></a> A one time sampling blitz conducted by the US Geological Survey. It identified 100s of pesticides and wastewater contaminants over 10 weeks of sampling.</li></ul>')),
                br(),
                br()
                ),
        
        tabItem(tabName = "health",
                #Header
                tags$h1("Water Quality through Time"),
                tealLine,
            
                #Subsection 1
                tags$h3("How does the Ellerbe Creek Watershed compare to local water?"),
                tags$p("The Environmental Protection Agency's (EPA) “How’s My Waterway?” tool allows one to explore the status of waterways locally. The EPA graphic is shown below for the Durham and Orange Counties. "),
				br(),
                tags$p("Notice how Ellerbe Creek, which is highlighted, is marked as degraded (red). While other streams in the region, mostly in highly developed areas, are degraded, others are doing well. Ellerbe Creek is the most degraded tributary to the Fall Lake drinking water reservoir."),
                br(),
                tags$h3("EPA: How's My Waterway?",style="text-align: center"),
                tags$head(tags$style(".testImage{border: solid 10px #202A44;  width: 100% !important;height: auto !important;}")),
                fluidRow(column(width = 3),column(width=6,img(src='hamww.PNG',class="testImage"))),
                tags$div(HTML('<center><iframe src="https://www.epa.gov/sites/production/files/widgets/mywaterway/index.html" id="mywaterway" width="220" height="170" frameborder="0" style="border:0" allowfullscreen>iFrames are not supported on this page.</iframe><center>')),
                
                #Subsection 2
                tags$h3("What is a water quality index?"),
                tags$p("A water quality index seeks to provide a combined assessment of how suitable the water is for life. It ranges from 0 (perfectly uninhabitable) to 100 (perfectly habitable), calculated from various proxy measurements (hardness, nitrogen content, phosphorus content, dissolved oxygen, etc). However, the requirements of every ecosystem can differ. A lot of information can be lost trying to summarize everything that goes into making a good waterway down to one number. However, it is a starting point. The water quality index for each Ambient Site is shown below. More information on how the water quailty index is used in Durham is discussed in the next section"),
                
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
                tags$p("Let's dig deeper into this dataset and visualize how different indicators of water quality change over time and across sites. Use the interactive Water Quality Measurements Over Time graphic below to see what might be driving changes to water quality."),
                br(),
                tags$p("Notice any spikes in a given parameter? That could be a pulse from a rain storm, illicit discharge, or other activity like construction. Check out the Regulation Compliance by Parameter graph to explore how often collected water samples analyized comply with the National Recommended Water Quality Criteria - Aquatic Life Criteria Table. U.S. Environmental Protection Agency | US EPA, 2022."),
                br(),
                
                fluidRow(column(width=3, br(), br(), br(),br(),br(),br(), br(), br(),
                                box(width = "100%", background = "navy",
                                      selectInput("Site", "Select Water Sampling Station", ambientSites, multiple = TRUE, selected = ambientSites[1:2]),
                                      selectInput("Param", "Select Parameter", ambientParameter,selected="Fecal Coliform"),
                                      materialSwitch(inputId = "aggregateSwitch", label = "Aggregate data across sites (barchart only)"))),
                                      
                         column(9, align="center",
                                #Trendline
                                tags$h3("Water Quality Measurements Over Time",style="text-align: center"),
                                box(width = "100%", background = "navy",
                                    plotlyOutput("Plot")))),
                fluidRow(column(width=12,
                                #Regulation Barchart
                                tags$h3("Regulation Compliance by Parameter - Aquatic Life Criteria",style="text-align: center"),
                                box(width = "100%", background = "navy",
                                    plotlyOutput("barPlot"))))
                ),

        
        tabItem(tabName = "justice",
                tags$h1("Water Quality though Space"),
                tealLine,
                
                
                #Subsection 1
                tags$h3("How do water quality indicators change throughout the watershed?"),
                tags$p("Duke researchers mapped out nutrients, salts, and other contaminants within the Ellerbe creek watershed in their synoptic sampling dataset. Let's dive into their preliminary data below!"),
                tags$p("To start, let's do a quick investigation into whether there are simple linear relationships between water quality indicator compounds and measures of land-use (left panel) and demographics (right panel). Note: Any box with an X indicates there is no statistically significant linear relationship between the two parameters."),
                
                tags$h3("Correlation table of variables within the Ellerbe Creek WaterShed",style="text-align: center"),
                fluidRow(column(width=5,box(width = "100%",background = "navy", plotOutput("corTableDemo"))),
                         column(width = 2,box(width = "100%",background = "navy",prettyRadioButtons("seasonCorr", "Season of Sampling of Water Quality Measurments",
                                                                                                    choices = synopticSeasons,
                                                                                                    shape = "curve",
                                                                                                    animation = "smooth",
                                                                                                    inline = FALSE,
                                                                                                    selected = synopticSeasons[1]))),
                         column(width=5,box(width = "100%",background = "navy", plotOutput("corTableInfra")))),

                tags$h3("Dig Deeper!"),
                tags$p("Use the tool below to explore how different water quality parameters relate to the infrastructure and demographics of the region. To inform your comparison, you can use the correlation plot above. Alternatively, you can investigate common research questions: Are there excess nutrients (nitrogen, phosphorus) from fertilizer on developed land? Is there a stronger correlation between where salt is and where roads are in the winter or the summer?"),
                tags$h3("Variable Comparison Tool",style="text-align: center"),
                fluidRow(column(12,align="center",box(width = "100%", background = "navy",uiOutput("synced_maps", width="100%"),style="padding-left: 35px"))),
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
                           tags$head(tags$style("#text1#text2{font-size: 12px;}"))))),
                
                #Subsection 2
                tags$h3("How does environmental justice impact water quality?"),
                tags$p("Historic and current investment into stormwater, sewer, and “green“ infrastructure (e.g., rain gardens) can greatly impact water quality, by slowing and filtering out contaminants. However, the history of discriminatory policies throughout the U.S. has led to disproportionate investment in this infrastructure. Understanding modern distributions of water quality challenges can help governmental organizations to ensure that everyone within a community benefits equitably from what stream ecosystems and ensure that is the case for future generations as well."),
                tags$p("Redlining is one example. The historic policies led to financial institutions carving out neighborhoods where certain people would not be allowed to use federal loans to purchase homes. This practice has been historically used to discriminate against racial minorities and low income individuals. The effects of redlining are still apparent in cities today. The figure below shows where historical redlining performed in Durham overlaps with the Ellerbe Creek watershed. The redlined communities suffered from historical underinvestment, which can lead to effects that propagate into current day water quality challenges. An example of this is some pipes in Durham have not been replaced in over 100 years in low income areas, placing the community at higher risk of exposure to leaky sewer infrastructure."),
                tags$h3("Redlining Overlapped on top of the Ellerbe Creek Watershed",style="text-align: center"),
                fluidRow(column(width=2),column(width =8, box(width = "100%",background = "navy", leafletOutput("redliningLeaflet"))),column(width=2)),
                
		

		
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
                tags$p("*At the time this website was made, data was not available on the tracer pollutants. In an atempt to have a meaningful but less precise measure of pollution sources, chloride was used in place of dipropylene glycol, sulfate was used in place of sucralose, and ammonium was used in place of glyphosate"),
                tags$h2("Normalization method",style="text-align: center"),
                tags$p("In order to allow users to simultaneously compare all three tracers at the same time on the same axis, the values have been normalized by subtracting the historical mean and dividing by the historical standard deviation. To ensure that all values are positive for polar radius graphs and that outliers don't make the remaining data uninterpretable, the z score was mapped to a value between 0 and 1 using the logistic function shown below."),
                fluidRow(column(width=3),
                         column(width=6,box(width = "100%", background = "navy", plotlyOutput("logisticPlot"))),
                         column(width=3)),
                tags$p("The result of the normalization method on the proxy measruments can be seen in the graph below. 0.5 represents the average value of a particular pollutant over the entire time period."),
                tags$h3("Key Pollution Indicators",style="text-align: center"),
                fluidRow(column(width = 3, 
                                box(width = "100%", background = "navy",
                                    selectInput("prods", "Select parameter", choices = c("Ammonium","Chloride", "Sulfate"),selected = "Ammonium", multiple = TRUE),
                                    selectInput("type", "Chart type", choices = c("polar-area","polar-radius","bar"),selected = "bar"),
                                    checkboxInput("labels", "Show values"))),
                         column(width = 9, box(width = "100%", background = "navy",leafletOutput("param3map")))),
                
        ),
        tabItem(tabName = "download",
                tags$h1("Download Data"),
                tealLine,
                h2("Excel files for download",style="text-align: center"),
                p("Click on the download button of any dataset you would like to explore further:",style="text-align: center"),
                h3("Ambient Data from the City of Durham",style="text-align: center"),
                downloadButton('downloadambientData','Ambient Data', class = "btn-block"),
                downloadButton('downloadwaterQualityIndex', 'Water Quality Index Data', class = "btn-block"),
                downloadButton('downloadambientDataLocation', 'Ambient Data Location', class = "btn-block"),
                h3("Synoptic Data from Duke Researchers",style="text-align: center"),
                downloadButton('output$downloadsynopticData','Synoptic Data', class = "btn-block"),
                downloadButton('downloadsynopticDataLocation', 'Synoptic Data Location', class = "btn-block"),
                br(),
                h2("Links",style="text-align: center"),
                p("Links to the code and public data sources ",style="text-align: center"),
                a("Link to Source of Ambient Data", href="http://durhamwaterquality.org/", class = "btn-block"),
                a("Link to GitHub Codebase", href="https://github.com/joa-kenit/DataPlus2022-data", class = "btn-block"),
                
                tags$head(tags$style(".btn-block{background-color:#202A44;color: white;width: 30%;margin-left: 35%;margin-right: 30%;height:40px;font-size: 20px;text-align: center;}"))  
                
        ), 
        
        tabItem(tabName = "team", 
                tags$h1("Team"),
                tealLine,
                tags$h2("Contact Information",style="text-align: center"),
                tags$p("Please direct any questions to Jonny Behrens: jonathan.behrens@duke.edu."),
                tags$h2("Coding Team",style="text-align: center"),
                fluidRow(
                  tags$head(tags$style(".headShot{border: solid 2px #202A44;background-color: #202A44;width: 100% !important;height: auto !important;}")),
                  column(width=1),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='JackPhoto.png',class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Jack Tsenane"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Jack studied chemical engineering at Vanderbilt University. He first began river monitoring for the Wisconsin DNR in middle school. Jack enjoys employing data science tools to better understand and improve the world.",style="text-align:left;padding:5px;border-radius:10px"))),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='JoannaPhoto.png',height = "400",class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Joanna Huertas"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Joanna is an environmental engineer with more than four years of experience in environmental consulting. She is pursuing a Master of Engineering in Environmental Engineering at Duke University, class of 2023. Her experience in private, academic, and government sectors in regards to water quality compliance aids the technical underpinning of the display of water quality monitoring results." ,style="text-align:left;padding:5px;border-radius:10px"))),
                  column(width=3, align="center",
                         br(),br(),br(),
                         img(src='RyanPhoto.png',height = "400",class = "headShot"),
                         box(width = "100%", background = "navy",
                             h1("Ryan Yu"),
                             tags$hr(style = "width: 30%;height: 2px;"),
                             p("Ryan is a rising sophomore at Duke University who has taken coursework in mathematics, computer science, and statistical science. In addition to these areas, he is exploring finance and economics. Ryan is planning on declaring for a major in Computer Science and Statistics in the Spring of 2023." ,style="text-align:left;padding:5px;border-radius:10px"))),
                  column(width=1),
                  ),
                tags$h2("Acknowledgments",style="text-align: center"),
                tags$p("This project was made possible by the extensive support of the Duke River Center, namely, Jonathan Behrens, Steven Anderson, and Emily Bernhardt. These people had the roles of project manager, project mentor, and project lead. We would also like to thank the Ellerbe Creek Watershed Association for their collaboration.")
      
        )#end of team subtab
        
      )#end of tabs
      
    )#end dashboard body
    
  )#end dashboard page
  
)
)