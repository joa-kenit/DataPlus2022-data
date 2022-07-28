############################################
# ECWA Data Visualization ITERATION TWO    #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# server.R file                            #
############################################
source("./Global.R")
shinyServer(function(input, output, session) {
  ########################
  #Explore relations tab #######################################################
  ########################
  output$dragAndDropListChloro = renderUI({
    
    #Filter parameters available on the drop down selection based on where data is available
    Contaminant = con$displayedTitle[(con$Type == "Contaminants") & (unusableVars[unusableVars$Group.1==input$season,con$Feature]==FALSE)]
    Infrastructure = con$displayedTitle[(con$Type == "Infrastructure") & (unusableVars[unusableVars$Group.1==input$season,con$Feature]==FALSE)]
    Demographics = con$displayedTitle[(con$Type == "Demographics") & (unusableVars[unusableVars$Group.1==input$season,con$Feature]==FALSE)]
    
    return(fluidRow(box(width = 12, background = "navy",                               
                        column(width = 4,
                               rank_list(
                                 text = "Water Quality Measurements",
                                 labels = Contaminant,
                                 input_id = "main_list1",
                                 options = sortable_options(group = "my_shared_group"),
                                 class = "rankJack"
                               )),
                        column(width = 4,
                               rank_list(
                                 text = "Infrastructure & Environment",
                                 labels = Infrastructure,
                                 input_id = "main_list2",
                                 options = sortable_options(group = "my_shared_group"),
                                 class = "rankJack"
                               ),
                               rank_list(
                                 text = "Demographics",
                                 labels = Demographics,
                                 input_id = "main_list3",
                                 options = sortable_options(group = "my_shared_group"),
                                 class = "rankJack")
                        ), column(width=4,                                         
                                  rank_list(
                                    text = "X axis",
                                    labels = c(),
                                    input_id = "list_1",
                                    options = max_1_item_opts,
                                    class = "rankJack"
                                  ),
                                  rank_list(
                                    text = "Y axis",
                                    labels = c(),
                                    input_id = "list_2",
                                    options = max_1_item_opts,
                                    class = "rankJack"
                                  ),
                                  rank_list(
                                    text = "Z axis (optional)",
                                    labels = c(),
                                    input_id = "list_3",
                                    options = max_1_item_opts,
                                    class = "rankJack"
                                    
                                  )))))
  })
  
  varsDataframe <- reactive({
    words = c(input$list_1,input$list_2,input$list_3)
    words = words[length(words) != 0]
    words = append(words, c("Site","Season","SUBBA"))
    b = con$Feature[con$displayedTitle %in% words]
    a = na.omit(synopticData[b])
    a <- a %>% filter(Season == input$season)
    #Get highest stream order representing subbasin
    a = a[order(a$SITE, decreasing = TRUE), ]
    a = distinct(a,SUBBA, .keep_all = TRUE) 
    return(a)
  })

  xVar = reactive({  
    if(length(input$list_1)==1){
      return(varsDataframe()[[2]])}
    else{
      return(input$list_1) #should be empty 
    }})
  yVar = reactive({
    if(length(input$list_2)==1){
      if(length(input$list_1)==1){
        return(varsDataframe()[[3]])
      }else{
        return(varsDataframe()[[2]])
      }}
    else{
      yVar = return(input$list_2) #should be empty 
    }})
  zVar = reactive({
    if(length(input$list_3)==1){
      if(length(input$list_1)==1 && length(input$list_1)==1){
        return(varsDataframe()[[4]])
      }else if(length(input$list_1)==1 || length(input$list_1)==1){
        return(varsDataframe()[[3]])
      }else{
        return(varsDataframe[[2]])
      }}
      else{
        return(input$list_3) #should be empty 
      }})
  
  fit <- reactive({
    xVar = xVar()
    yVar = yVar()
    zVar = zVar()
    if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){
    lm(zVar ~ yVar + xVar)}
    else if((length(input$list_1) + length(input$list_2))  == 2){
      lm(yVar ~ xVar)}}) 
  
  output$value2 <- renderPlotly({
    xVar = xVar()
    yVar = yVar()
    zVar = zVar()
    if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){
    fig3D <- plot_ly(x = xVar, y = yVar, z = zVar)
    fig3D <- fig3D %>% add_markers(x = xVar, y = yVar, marker = list(size = 10,color = '#08d8b2',line = list(color = '#004058', width = 2)))
    
    fig3D <- fig3D %>% layout(scene = list(xaxis = list(title = paste(input$list_1,con$Unit[con$displayedTitle == input$list_1])),
                                           yaxis = list(title = paste(input$list_2,con$Unit[con$displayedTitle == input$list_2])),
                                           zaxis = list(title = paste(input$list_3,con$Unit[con$displayedTitle == input$list_3])),
                                           #aspectmode='cube',
                                           showlegend = F))
    
    # Compute the linear regression 
    if(input$bestFitSwitch){
      M <- (mesh(yVar, xVar))
      fit3D <- fit()
      zNew <- with (M, fit3D[[1]][[1]] + fit3D[[1]][[2]]*x+fit3D[[1]][[3]]*y)
      fig3D <- fig3D %>%  add_surface(type = 'surface', x = xVar, y = yVar, z = zNew,opacity = .15, colorscale = list(c(0,1),c("#08d8b2",'#004058')))
    }
    
    fig3D <- fig3D %>% hide_colorbar()
    
    fig3D}
    else if((length(input$list_1) + length(input$list_2))  == 2){
      fig <- plot_ly(type = "scatter",mode = "markers")
      fig <- fig %>% layout(title = paste(input$list_2,"Vs.",input$list_1,"on",unique(synopticData$DATE[synopticData$Season==input$season])),
                            xaxis = list(title = paste(input$list_1,con$Unit[con$displayedTitle == input$list_1])),
                            yaxis = list(title = paste(input$list_2,con$Unit[con$displayedTitle == input$list_2])),
                            showlegend = F)
      fig <- fig %>% add_markers(x = xVar, y = yVar,marker = list(size = 10, color = '08d8b2', line = list(color = '004058', width = 2))) 
      if(input$bestFitSwitch){
        fig <- fig%>%add_trace(x = xVar, y = fitted(fit()),mode = "lines",line=list(color='004058'))
      }
      fig}
    else{
      emptyFig
    }})
  
  observe({
    if(!is.null(input$bestFitSwitch) && input$bestFitSwitch){
      output$text1 <- renderText({paste("R squared:", signif(summary(fit())[r.squared],2))})
      output$text2 <- renderText({if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){""}
        else if((length(input$list_1) + length(input$list_2))  == 2){paste("P value: ", signif(summary(fit())$coefficients[2,4], 2))}
        else{""}})
      
      fitText <- reactive({if((length(input$list_3)+length(input$list_1) + length(input$list_2))  == 3){paste(input$list_3,"=",signif(fit()[[1]][[3]],2),'x',input$list_2,'+',signif(fit()[[1]][[2]],2),'x',input$list_1,"+",signif(fit()[[1]][[1]],2))}
        else if((length(input$list_1) + length(input$list_2))  == 2){paste(input$list_2,"=",signif(fit()[[1]][[2]],2),'x',input$list_1,"+",signif(fit()[[1]][[1]],2))}
        else{""}})
      output$text3 <- renderText({fitText()})}
    else{
      output$text1 <- renderText("")
      output$text2 <- renderText("")
      output$text3 <- renderText("")
    }
  })
  

  
  #############
  #Downloader ##################################################################
  #############
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
  #################
  #Data Over Time ##############################################################
  #################
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
             legend = l,
             showlegend = TRUE)
    
    for(s in input$Site) {
      #reset asz to original just zinc a.s (a.s is the constant)
      #active_sitesReal sorted first for date
      active_sites_param <- ambientData %>% filter(Parameter == input$Param) %>% 
        filter(Station.Name == s)
      # active_sites_zinc <- active_sites_zinc %>% filter(Station.Name == s) %>%
      #   order((Date.Time))
      #filter for station name and then sort by date
      fig3 <- fig3 %>% add_trace(
        x = as.Date(active_sites_param$Date.Time), 
        y = active_sites_param$Value, name = s, type = 'scatter', mode = 'lines+markers', line=list(dash='dot'))
    }
    fig3})
  ########################
  #WQI leaflet over time ########################################################
  ########################
  output$WQImap <- renderLeaflet({
    
    leaflet()%>% addTiles()%>%addCircleMarkers(lng= stationData1$Longitude, lat = stationData1$Latitude,layerId = stationData1$Name,color = "black", fillOpacity = 0.9,
                                               fillColor = palwqi(as.numeric(wqiData1[nrow(wqiData1),])),opacity = 1, radius = 15,
                                               label = paste("Station name:",stationData1$Name,"\n","Water quality index:",wqiData1[nrow(wqiData1),]))%>%addLegend(position = "bottomleft",pal = palwqi
                                                                                                                                                                   , values=c(0,100), opacity=2, title= "Water Quality Index")})
  
  colors <- c("#184e77", "#1e6091", "#1a759f","#168aad", "#34a0a4", "#52b69a", "#76c893", "#99d98c", "#b5e48c","#d9ed92")
  output$wqiLinePlot <- renderPlotly({figWQI <- plot_ly()%>%layout(title = 'Water Quality over Time',colorscale="blues2green",shapes = vline(wqiData$Date[nrow(wqiData1)]), plot_bgcolor = "#e5ecf6", 
                                                                   yaxis = list(title = 'Durham City Water Quality Index'),
                                                                   legend = list(orientation = 'h'))
  i = 1
  for (site in ambientSites){figWQI <- figWQI %>% add_trace(x = wqiData$Date, y = wqiData[[site]], line = list(color = colors[i]),
                                                     type = 'scatter', mode = 'lines', name = site)
  i = i + 1}
  figWQI})
  
  #Dynamically update
  observe({
    dateRow = which.min(abs(wqiData$Date-input$wqiDate)) #Gets index of date closest to the date shown on the slider
    rowVals = as.numeric(wqiData1[dateRow,])
    leafletProxy('WQImap') %>%
      addCircleMarkers(lng= stationData1$Longitude, lat = stationData1$Latitude, layerId = stationData1$Name,color = "black",fillOpacity = 0.9,
                       fillColor = ifelse(rowVals > 50 , palwqi(rowVals), "#d73027"),opacity = 1, radius =15,
                       label = paste("Station name:",stationData1$Name,"\n","Water quality index:",wqiData1[dateRow,]))
    plotlyProxy("wqiLinePlot", session) %>% plotlyProxyInvoke("relayout",c(shapes = vline(wqiData$Date[dateRow])))
  })
  
  ##############################
  #leafletminicharts param3map #################################################
  ##############################
  # Initialize map #input$prods=choices in ui.R
  output$param3map <- renderLeaflet({
    Xtitle <- tags$div(HTML(paste(input$prods," (",unit$Unit[unit$Parameter == input$prods],")",sep="")))
    leaflet()%>% addTiles %>% 
      addPolygons(data = huc14, 
                  #fillColor = ~pal(durham_contaminants$Value),
                  weight = 1.5,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0) %>% 
      addMinicharts(
        tracerData$Longitude,tracerData$Latitude,
        type = input$type, #"polar-area",
        chartdata = tracerData[, param3variables], #input$prods
        time = tracerData$DATE,
        layerId = tracerData$SITE, 
        showLabels = input$labels,
        #colorPalette = colors,
        width = 45)%>% #, height = 25
      addControl(Xtitle, position = "bottomleft") 
  })
  #Update charts each time input value changes
  
  
  #Change fr test commit
  observe({
    if (length(input$prods) == 0) {
      data <- 1
    } else {
      data <- tracerData[ ,param3variables]
    }
    maxValue <- max(as.matrix(data))
    
    leafletProxy("param3map", session) %>%
      updateMinicharts(
        tracerData$Longitude,tracerData$Latitude,
        chartdata = data, 
        maxValues = maxValue,
        time = tracerData$DATE,
        type = input$type, #"polar-area",
        showLabels = input$labels
      )
  })
  ###############
  # Read tables ################################################################
  ###############
  output$tableSources <- renderTable({read.csv('www/tableOfCollectionSources.csv',check.names = TRUE)},width = "50%")
  
  #######################
  #Generate Choropleth #########################################################
  #######################
  
  output$synced_maps <- renderUI({
    
    if(length(input$list_1)==1){
      x = xVar()
      choroXColor <- colorNumeric(palette = 'Blues',  domain=c(0,max(x)))
      Xtitle <- tags$div(HTML(paste(input$list_1," (",con$Unit[con$displayedTitle == input$list_1],")",sep="")))  
      m1 <- leaflet()%>% addTiles()%>%
        addPolygons(data = huc14, fillColor = choroXColor(x), weight = 1, opacity = 1, color = "black", fillOpacity = 0.3,
                    label = paste("Subbasin:",huc14$SUBBA,input$list_1,":",signif(x,3),con$Unit[con$displayedTitle == input$list_1]))%>%
        addControl(Xtitle, position = "bottomleft")
    }else{
      m1 = emptyMap
    }
    if(length(input$list_2)==1){
      y = yVar()
      choroYColor <- colorNumeric(palette = 'Blues',  domain=c(0,max(y)))
      Ytitle <- tags$div(HTML(paste(input$list_2," (",con$Unit[con$displayedTitle == input$list_2],")",sep=""))) 
      m2 <- leaflet()%>% addTiles()%>%addPolygons(data = huc14, fillColor = choroYColor(y), weight = 1, opacity = 1, color = "black", fillOpacity = 0.3,
                                                  label = paste("Subbasin:",huc14$SUBBA,input$list_2,":",signif(y,3),con$Unit[con$displayedTitle == input$list_2]))%>%
        addControl(Ytitle, position = "bottomleft")
    }else{
      m2 = emptyMap
    }
    
    if(length(input$list_1) + length(input$list_2) + length(input$list_3)==3){
      z = zVar()
      choroZColor <- colorNumeric(palette = 'Blues',  domain=c(0,max(z)))
      Ztitle <- tags$div(HTML(paste(input$list_3," (",con$Unit[con$displayedTitle  == input$list_3],")",sep=""))) 
      m3 <- leaflet()%>% addTiles()%>%addPolygons(data = huc14, fillColor = choroZColor(z), weight = 1, opacity = 1, color = "black", fillOpacity = 0.3,
                                                  label = paste("Subbasin:",huc14$SUBBA,input$list_3,":",signif(z,3),con$Unit[con$displayedTitle == input$list_3]))%>%
        addControl(Ztitle, position = "bottomleft")
      sync(m1, m2, m3, ncol = 3)}
    else{sync(m1,m2)}})
  
  ############
  #Box Plots ###################################################################
  ############
  output$Boxplots <- renderPlotly({
    #creating the plot
    curParam <- con$Feature[con$displayedTitle==input$ParamBoxplots]
    
    wider_select_2$Date.Time <- substring(wider_select_2$Date.Time, 0, 4)
    
    wider_select_2 <- wider_select_2[which(wider_select_2$Date.Time %in% c("2016", "2017", "2018", "2019","2020", "2021", "2022")),]
    
    names(wider_select_2)[names(wider_select_2) == input$ParamBoxplots] <- 'targetVar1'
    
    #rename column of currently selected Param as 'targetVar1'
    p1 <- plot_ly(wider_select_2, x = ~Station.Name, y = ~targetVar1, type = "box", name = "Durham Data Sites") %>%
      layout(yaxis = list(title = "Mg/L"))
    
    names(wider_select_2)[names(wider_select_2) == 'targetVa1r'] <- input$ParamBoxplots
    
    names(alt_bc_data3)[names(alt_bc_data3) == curParam] <- 'targetVar2'
    p2 <- plot_ly(alt_bc_data3, x = ~DATE, y = ~targetVar2, type = "box", name = "Bass Connection Samples") %>%
      layout(yaxis = list(title = "Mg/L"))
    names(alt_bc_data3)[names(alt_bc_data3) == 'targetVar2'] <- curParam
    
    fig =subplot(p1, p2, nrows = 1, shareX = FALSE, shareY = TRUE) %>% layout(yaxis = list(title = "Mg/L"))
    return(fig)
  })
  
  ############
  #Cor table ###################################################################
  ############
  synopticData1 <- reactive({
    synopticData1 <- synopticData %>% filter(Season == input$seasonCorr)
    toKeep <- con$Feature[con$Type%in%c("Contaminants","Infrastructure","Demographics")]
    synopticData1<-synopticData1[,(names(synopticData1) %in% toKeep)]
    
    #Remove empty columns
    empty_columns <- sapply(synopticData1, function(x) all(is.na(x) | x == ""))
    synopticData1 = synopticData1[, !empty_columns]
    return(synopticData1)})
  
  output$corTableDemo <- renderPlot({
    synopticData1X <- con$Feature[con$Type=="Contaminants"]
    synopticData1Y1 <- con$Feature[con$Type=="Infrastructure"]
    a = which(colnames(synopticData1()) %in% synopticData1X)
    b = which(colnames(synopticData1()) %in% synopticData1Y1)

    matrixCor <- cor(synopticData1(),use="pairwise.complete.obs")
    
    fig1 <- ggcorrplot(matrixCor[b,a], method = "square",colors = c("navy", "white", "#08d8b2"), p.mat = cor_pmat(synopticData1())[b,a]) + 
      scale_x_discrete(labels = con$displayedTitle[which(con$Feature %in% colnames(matrixCor))][b]) + 
      scale_y_discrete(labels = con$displayedTitle[which(con$Feature %in% colnames(matrixCor))][a])
    return(fig1)})
  
  output$corTableInfra <- renderPlot({
    synopticData1X <- con$Feature[con$Type=="Contaminants"]
    synopticData1Y2 <- con$Feature[con$Type=="Demographics"]
    a = which(colnames(synopticData1()) %in% synopticData1X)
    c = which(colnames(synopticData1()) %in% synopticData1Y2)
    
    matrixCor <- cor(synopticData1(),use="pairwise.complete.obs")
    
    fig2 <- ggcorrplot(matrixCor[c,a], method = "square",colors = c("navy", "white", "#08d8b2"),p.mat = cor_pmat(synopticData1())[c,a]) + 
      scale_x_discrete(labels = con$displayedTitle[which(con$Feature %in% colnames(matrixCor))][c]) + 
      scale_y_discrete(labels = con$displayedTitle[which(con$Feature %in% colnames(matrixCor))][a])
    return(fig2)})
  
  ##########################
  #Generate redlining plot #####################################################
  ##########################
  output$redliningLeaflet<- renderLeaflet({
    leaflet(redlining) %>%
      addTiles('https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}') %>%
      addPolygons(fillColor = redliningCols(redlining$holc_grade), weight = 2, opacity = 1, color = redliningCols(redlining$holc_grade), fillOpacity = 0.3,label = redlining$holc_id)%>%
      addPolygons(data = huc14, weight = 1, opacity = 1, color = "black", fillOpacity = 0)%>%
      addLegend(colors = redliningCol, labels = holcVals, opacity = 1,title = "Housing Grade")
  })
  
  ####################
  #Generate barchart ###########################################################
  ###################
  output$barPlot <- renderPlotly({
    
    if(input$aggregateSwitch){
      #Generate year bar chart data
      bardatatableFiltered <- bardatatable %>% filter(Station.Name %in% input$Site) %>% filter(Parameter == input$Param)
      bardata <- na.omit(bardatatableFiltered %>% count(Year, Regulation.compliance, vars = input$Param))
      bardata_percent <- aggregate(bardata$n, by=list(bardata$vars,bardata$Year), FUN=sum) 
      colnames(bardata_percent) <- c('vars','Year','x') #changing the name of the columns for the next step
      bla <- merge(bardata, bardata_percent,all=TRUE)                  # merge according to conditions (same col names)
      bardata_percent1_agg <- bla %>% mutate (Percentage = bla$n*100 / bla$x) 
      
      barplotly <- plot_ly()
      barDataParam <-bardata_percent1_agg %>% filter(vars == input$Param)
      barplotly <- plot_ly(data = barDataParam,x = ~Year, y = ~Percentage,type = "bar", color = ~Regulation.compliance, colors=c("#c63637","#a6d96a"), name = ~Regulation.compliance, marker = list(line = list(color = '#001f3f', width = 1))) %>% 
        layout(barmode = "stack", showlegend=T, yaxis = list(title = 'Percentage (%)'))
      return(barplotly)}
    else{
      subplot(
        map(bardata_percent1$Year %>% unique() , function(.x){
          
          purr_data <- bardata_percent1 %>% filter(vars == input$Param) %>% filter(Year == .x) %>%
            filter(Station.Name %in% input$Site) %>%
            group_by(Regulation.compliance) %>% 
            arrange(Regulation.compliance) 
          
          x_title <- unique(purr_data$Year)# %>% unique() 
  
          show_legend_once = ifelse(x_title == "2016",TRUE,FALSE)
          
          plot_ly(data = purr_data, 
                  x = ~Station.Name, 
                  y = ~Percentage, 
                  color= ~Regulation.compliance,
                  colors = c("#c63637","#a6d96a"),
                  marker = list(line = list(color = '#001f3f', width = 1)),
                  type = 'bar', 
                  legendgroup=~Regulation.compliance,
                  showlegend=show_legend_once)%>%
                  layout(xaxis = list(title = x_title))
        })
        ,titleX = TRUE,shareY = T) %>% layout(barmode = 'stack', showlegend = TRUE, legend = l)
        }
  })
  
  ###########
  #PCA Plot #####################################################################
  ###########
  output$PCA <- renderPlot({
    bc_dataRepNA <- bc_dataRepNA[, -c(23, 25:26, 28, 30:32)]
    bc_dataRepNA <- na.omit(bc_dataRepNA)
    pca1 <- stats::prcomp(bc_dataRepNA[, c(4:15)], center = TRUE, scale=TRUE)
    
    plota <- ggplot2::autoplot(pca1, x=1, y=2, data=bc_dataRepNA, colour = input$PCAparams,  
                               main = "PCA of Ellerbe Creek September, February, June Surveys",
                               alpha=0.7, loadings.colour='black', loadings.label.colour='black',
                               loadings=TRUE, loadings.label=TRUE, size=3) + 
      scale_color_gradient(low = '#ebfbfb', high = '#0d2e27')
    
    plota
    
  })
  output$PCA2 <- renderPlot({
    list_dates <- input$PCAdates
    bc_dataRepNA <- bc_dataRepNA[which(bc_dataRepNA$DATE %in% list_dates),]
    pca1 <- stats::prcomp(bc_dataRepNA[, c(4:15)], center = TRUE, scale=TRUE)
    plotb <- ggplot2::autoplot(pca1, x=1, y=2, data=bc_dataRepNA, colour = 'DATE',  
                               frame = TRUE, 
                               main = "PCA of Ellerbe Creek September, February, June Surveys",
                               alpha=0.7, loadings.colour='black', loadings.label.colour='black',
                               loadings=TRUE, loadings.label=TRUE, size=3) + theme_classic()
    
    plotb
    
  })
  output$PCA3 <- renderPlot({
    list_sites <- input$PCAsites
    wider_select <- wider_select[which(wider_select$Station.Name %in% input$PCAsites),]
    
    #wider_selectfixed <- data.frame(t(na.omit(t(wider_select))))
    pca2 <- stats::prcomp(wider_select[, c(3:11)], center = TRUE, scale = TRUE)
    
    plotc <-ggplot2::autoplot(pca2, x=1, y=2, data = wider_select, colour = 'Station.Name', frame = TRUE, main = "PCA of Ellerbe Creek Sampling Sites Durham Data", 
                              #could take off arrows option
                              alpha=0.7, loadings.colour='black', loadings.label.colour ='black',
                              loadings=TRUE, loadings.label=TRUE, size=3) + theme_classic()
    plotc
  })
  
  
  
  
  #################
  #Logistic Curve ##############################################################
  #################
  output$logisticPlot <- renderPlotly({
    return(logisticFig)
  })
  
}) #End of server

