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
library(stringr)  


active_sitesReal <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/asites.csv")

active_sites_param <- active_sitesReal  
   
#list of options for sites:
sites <- unique(active_sitesReal$Station.Name)
Parameter <- unique(active_sitesReal$Parameter)
Parameter1 <- as.data.frame(Parameter)
# units_Set <- left_join(Parameter, active_sites_param)
shortlist <- active_sites_param[, c("Parameter", "Unit")] 
units_set <- left_join(Parameter1, shortlist)
#list of 38 params with corresponding unit
units_set <- units_set[!duplicated(units_set$Parameter), ]



# unit_param <- str_c(units_set$Parameter, "&",  units_set$Unit)
# 
# unit_p <- as.data.frame(unit_param) 
# 
# unit_p <- unit_p %>% filter(unit_param == "Dissolved Oxygen&mg/L")
# 
# Str <- as.character(unit_p[1])
# 
# newStr <- strsplit(Str, split = '&')

#use join to match units
#potentially list param options in server instead?

#no errors above this point.

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Time Graphs"),
    selectInput("Site", "Select Water Sampling Station", sites, multiple = TRUE),
    selectInput("Param", "Select Parameter", Parameter),
    #outPut for Plot
    plotlyOutput("Plot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #narrow down unit


    
    output$Plot <- renderPlotly({
        
        
        units_label <- units_set %>% filter(Parameter == input$Param)
        units_label <- units_label$Unit
        
            
         fig3 <- plot_ly()%>% 
             layout(title = paste(input$Param, "Level Over Time"),
                    plot_bgcolor='#e5ecf6',  
                    xaxis = list(  
                        title = 'Date',
                        zerolinecolor = '#ffff',  
                        zerolinewidth = 2,  
                        gridcolor = 'ffff'),  
                    yaxis = list(  
                        #need specific type of unit here
                        title = paste(units_label),
                        zerolinecolor = '#ffff',  
                        zerolinewidth = 2,  
                        gridcolor = 'ffff'),
                    shapes = list(
                        list(type = "rect",
                             fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.3,
                             x0 = as.Date("2004-01-20 09:15:00"), x1 = as.Date("2022-03-01 09:15:00"), xref = "x",
                             y0 = 0, y1 = 14, yref = "y"),
                        list(type = "rect",
                             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
                             x0 = as.Date("2004-01-20 09:15:00"), x1 = as.Date("2022-03-01 09:15:00"), xref = "x",
                             y0 = 2, y1 = 6, yref = "y")),
                    showlegend = TRUE, width = 1100)
         
    #Dates info:
    #2004-01-20 09:15:00
    #2022-03-01 09:15:00
         
         
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
