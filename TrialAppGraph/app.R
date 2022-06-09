#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(plotly)

active_sitesReal <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/asites.csv")

active_sites_param <- active_sitesReal  
   
#list of options for sites:
sites <- unique(active_sitesReal$Station.Name)
Parameter <- unique(active_sitesReal$Parameter)
Parameter <- as.data.frame(Parameter)
# units_Set <- left_join(Parameter, active_sites_param)
short_list <- active_sites_param[c("Parameter", "Unit")] 
units_set <- left_join(Parameter, short_list)
#list of 38 params with corresponding unit
units_set <- units_set[!duplicated(units_set$Parameter), ]
#use join to match units
#potentially list param options in server instead?

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Time Graphs"),
    selectInput("Site", "Select Water Sampling Station", sites, multiple = TRUE),
    selectInput("Param", "Select Parameter", Params),
    #outPut for Plot
    plotlyOutput("Plot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #narrow down unit
    # reactive(units_label <- units_set %>% filter(Parameter == input$Param))
    
    output$Plot <- renderPlotly({
        
         fig3 <- plot_ly()%>% 
             layout(title = paste(input$Param, 'Levels Over Time'),
                    plot_bgcolor='#e5ecf6',  
                    xaxis = list(  
                        title = 'Date',
                        zerolinecolor = '#ffff',  
                        zerolinewidth = 2,  
                        gridcolor = 'ffff'),  
                    yaxis = list(  
                        #need specific type of unit here
                        title = paste("Unit"),
                        zerolinecolor = '#ffff',  
                        zerolinewidth = 2,  
                        gridcolor = 'ffff'),
                    showlegend = TRUE, width = 1100)
         
         for(s in input$Site) {
             #reset asz to original just zinc a.s (a.s is the constant)
             #active_sitesReal sorted first for date
             active_sites_param <- active_sitesReal %>% filter(Parameter == input$Param) %>% 
                 filter(Station.Name == s)
             
             # active_sites_zinc <- active_sites_zinc %>% filter(Station.Name == s) %>%
             #   order((Date.Time))
             #filter for station name and then sort by date
             fig3 <- fig3 %>% add_trace(
                 x = (as.Date(active_sites_param$Date.Time)), 
                 y = active_sites_param$Value, name = s, type = 'scatter', mode = 'lines + markers')
         }
         fig3
         
    #end of renderplotly
     }
    )
    #end of server function
}

# Run the application 
shinyApp(ui = ui, server = server)
