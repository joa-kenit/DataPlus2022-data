############################################
# ECWA Data Visualization                  #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# server.R file                            #
############################################
library(shiny)
library(sortable)
library(plotly)
library('plot3D')
library(ggcorrplot)

jonnyData <- na.omit(read.csv('www/ECWAWaterQuality2021.csv'))
con <- read.csv('www/ECWAWaterQuality2021Context.csv')
emptyFig <- plot_ly()
jonnyData1<-jonnyData[,!(names(jonnyData) %in% c("SubBasin","Type","Longitude","Latitude"))]
corr <- cor(jonnyData1)
p.mat <- cor_pmat(jonnyData1)
options(repr.plot.width = 14, repr.plot.height = 14)
corr.plot <- ggcorrplot(corr, hc.order = TRUE, type = "upper", outline.col = "white",colors = c("navy", "white", "#08d8b2"), p.mat = p.mat)


shinyServer(function(input, output) {
  #######################
  #Explore relations tab#
  #######################
  fit <- reactive({if(length(input$list_1) + length(input$list_2)  == 2){
    lm(jonnyData[, input$list_2] ~ jonnyData[, input$list_1])}
    else if((length(input$list_1) + length(input$list_2))  == 2){
      lm(input$list_3 ~ input$list_2 + input$list_1)
    }}) 
  
  output$corTable <- renderPlot(corr.plot, width = 850, height = 850)
  
  output$value2 <- renderPlotly({ if( (length(input$list_3) == 1) & ((length(input$list_1) + length(input$list_2))  == 2)){
    xVar = jonnyData[, input$list_1] # road
    yVar = jonnyData[, input$list_2] #pipe
    zVar = jonnyData[, input$list_3] #Cl
    fig3D <- plot_ly(x = xVar, y = yVar, z = zVar)
    fig3D <- fig3D %>% add_markers(x = xVar, y = yVar, marker = list(size = 10,color = '#08d8b2',line = list(color = '#004058', width = 2)))
    
    fig3D <- fig3D %>% layout(scene = list(xaxis = list(title = paste(input$list_1,con$Unit[con$Feature == input$list_1])),
                                           yaxis = list(title = paste(input$list_2,con$Unit[con$Feature == input$list_2])),
                                           zaxis = list(title = paste(input$list_3,con$Unit[con$Feature == input$list_3])),
                                           #aspectmode='cube',
                                           showlegend = F))
    
    # Compute the linear regression 
    M <- (mesh(yVar, xVar))
    fit3D <- lm(zVar ~ yVar + xVar)
    zNew <- with (M, fit3D[[1]][[1]] + fit3D[[1]][[2]]*x+fit3D[[1]][[3]]*y)
    fig3D <- fig3D %>%  add_surface(type = 'surface', x = xVar, y = yVar, z = zNew,opacity = .15, colorscale = list(c(0,1),c("#08d8b2",'#004058')))
    
    fig3D <- fig3D %>% hide_colorbar()
    
    fig3D}
    else if((length(input$list_1) + length(input$list_2))  == 2){
      fig <- plot_ly(type = "scatter",mode = "markers")
      xVar = jonnyData[, input$list_1]
      yVar = jonnyData[, input$list_2]
      subBasin = jonnyData[, "SubBasin"]
      fig <- fig %>% layout(title = 'Variable Comparison',
                            xaxis = list(title = paste(input$list_1,con$Unit[con$Feature == input$list_1])),
                            yaxis = list(title = paste(input$list_2,con$Unit[con$Feature == input$list_2])),
                            showlegend = F)
      
      fig <- fig %>% 
        add_markers(x = xVar, y = yVar,hoverinfo = 'subBasin',marker = list(size = 10, color = '08d8b2', line = list(color = '004058', width = 2))) %>% 
        add_trace(x = xVar, y = fitted(fit()),mode = "lines",line=list(color='004058'))
      
      fig}
    else{
      emptyFig
    }})
  
  output$text1 <- renderText({paste("R squared:", signif(summary(fit())$r.squared,2))})
  output$text2 <- renderText({paste("P value: ", signif(summary(fit())$coefficients[2,4], 2))})
  output$fitText <- reactive({if( (length(input$list_3) == 1) & ((length(input$list_1) + length(input$list_2))  == 2)){
    print("Running")
    paste(input$list_3,"=",fit()[[1]][[3]],'x',input$list_2,'+',fit()[[1]][[2]],'x',input$list_1,"+",fit()[[1]][[1]])
  }
    else if((length(input$list_1) + length(input$list_2))  == 2){paste(input$list_2,"=",fit()[[1]][[2]],'x',input$list_1,"+",fit()[[1]][[1]])}
    else{"line"}
    
  })
  output$text3 <- renderText({fitText()})


  ############
  #Downloader#
  ############
  data <- read.csv(file = 'www/merged_data.csv')
  output$downloadData0 <- downloadHandler(
    filename = function() {
      paste0("Site summary data", ".csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  data <- read.csv(file = 'www/merged_data.csv')
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("Variable data", ".csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  ################
  #Data Over Time#
  ################
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
  #End of Data Over Time
}
)
