############################################
# ECWA Data Visualization                  #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# server.R file                            #
############################################
source("./global.R")
shinyServer(function(input, output, session) {
  #######################
  #Explore relations tab########################################################
  #######################
  fit <- reactive({if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){
    lm(jonnyData[, input$list_3] ~ jonnyData[, input$list_2] + jonnyData[, input$list_1])}
    else if((length(input$list_1) + length(input$list_2))  == 2){
      lm(jonnyData[, input$list_2] ~ jonnyData[, input$list_1])}
    }) 
  
  output$corTable <- renderPlot(corr.plot, width = 850, height = 800)
  
  output$value2 <- renderPlotly({if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){
    xVar = jonnyData[, input$list_1] #road
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
  
  observe({
  output$text1 <- renderText({paste("R squared:", signif(summary(fit())$r.squared,2))})
  output$text2 <- renderText({if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){""}
                              else if((length(input$list_1) + length(input$list_2))  == 2){paste("P value: ", signif(summary(fit())$coefficients[2,4], 2))}
                              else{""}})
  
  fitText <- reactive({if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){
    paste(input$list_3,"=",signif(fit()[[1]][[3]],2),'x',input$list_2,'+',signif(fit()[[1]][[2]],2),'x',input$list_1,"+",signif(fit()[[1]][[1]],2))
  }
    else if((length(input$list_1) + length(input$list_2))  == 2){paste(input$list_2,"=",signif(fit()[[1]][[2]],2),'x',input$list_1,"+",signif(fit()[[1]][[1]],2))}
    else{""}
    
  })
  output$text3 <- renderText({fitText()})
  })


  ############
  #Downloader###################################################################
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
  #Data Over Time###############################################################
  ################
  output$Plot <- renderPlotly({
    units_label <- units_set %>% filter(Parameter == input$Param)
    units_label <- units_label$Unit
    
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
               title = paste(units_label),
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
        x = as.Date(active_sites_param$Date.Time), 
        y = active_sites_param$Value, name = s, type = 'scatter', mode = 'lines + markers')
    }
    fig3})
 #######################
 #WQI leaflet over time#########################################################
 #######################
  output$WQImap <- renderLeaflet({
    
    leaflet()%>% addTiles()%>%addCircleMarkers(lng= stationData1$Longitude, lat = stationData1$Latitude,layerId = stationData1$Name,color = "black", fillOpacity = 0.9,
                                               fillColor = beatCol(as.numeric(wqiData1[nrow(wqiData1),])),opacity = 1, radius = 15,
                                               label = paste("Station name:",stationData1$Name,"\n","Water quality index:",wqiData1[nrow(wqiData1),]))})
  
  colors <- c("#184e77", "#1e6091", "#1a759f","#168aad", "#34a0a4", "#52b69a", "#76c893", "#99d98c", "#b5e48c","#d9ed92")
  output$wqiLinePlot <- renderPlotly({figWQI <- plot_ly()%>%layout(title = 'Water Quality over Time',colorscale="blues2green",shapes = vline(wqiData$Date[nrow(wqiData1)]), plot_bgcolor = "#e5ecf6", 
                                                                yaxis = list(title = 'Durham City Water Quality Index'),
                                                                legend = list(orientation = 'h'))
  i = 1
  for (site in sites){figWQI <- figWQI %>% add_trace(x = wqiData$Date, y = wqiData[[site]], line = list(color = colors[i]),
                                               type = 'scatter', mode = 'lines', name = site)
  i = i + 1}
  figWQI})
  
  #Dynamically update
  observe({
    dateRow = which.min(abs(wqiData$Date-input$wqiDate)) #Gets index of date closest to the date shown on the slider
    rowVals = as.numeric(wqiData1[dateRow,])
    leafletProxy('WQImap') %>%
    addCircleMarkers(lng= stationData1$Longitude, lat = stationData1$Latitude, layerId = stationData1$Name,color = "black",fillOpacity = 0.9,
                       fillColor = ifelse(rowVals > 50 , beatCol(rowVals), "#d73027"),opacity = 1, radius =15,
                       label = paste("Station name:",stationData1$Name,"\n","Water quality index:",wqiData1[dateRow,]))
    plotlyProxy("wqiLinePlot", session) %>% plotlyProxyInvoke("relayout",c(shapes = vline(wqiData$Date[dateRow])))
    })
  
}) #End of server


