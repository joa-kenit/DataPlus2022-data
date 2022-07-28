#libraries and object used for Over-Time Graphs
library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shinycssloaders)
library(sortable)
library('plot3D')
library(ggcorrplot)
library(shinyWidgets)
library(leaflet)
library(leaflet.minicharts)
library(zoo)
library('sf')
library(tidyverse)
library(lubridate)
library(sf)
library(arcpullr)
library(mapview)
library(wesanderson)
library(geojsonio)
library(RColorBrewer)
library('sf')
library(leaflegend)
library(leafsync)
library(htmlwidgets)
library(htmltools)
library(psych)
library(purrr)
library(ggfortify)
library(ggplot2)

#Styling########################################################################
tealLine = tags$hr(style="width:20%;text-align:left;margin-left:0;height:3px;border-width:0;background-color:#08d8b2")

#Trends graph###################################################################
ambientData <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/updatedData/updatedParameterDurhamData.csv")
ambientSites <- unique(ambientData$Station.Name)
ambientParameter <- unique(ambientData$Parameter)
Mode <- function(x) {ux <- unique(x) 
                      ux[which.max(tabulate(match(x, ux)))]}
units_set = aggregate(Unit ~ Parameter,ambientData, Mode)

#Compare vars graph#############################################################
#S+U
con <- read.csv('www/synopticDataContext.csv')
#Server
synopticData <- read.csv('https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/updatedData/synopticData.csv')
bc_data3 <- synopticData
synopticDates <- unique(synopticData$DATE)
synopticData$DATE = as.Date(synopticData$DATE, format = "%m/%d/%Y")
pcaSynopticData <-synopticData
synopticData = synopticData[order(synopticData$DATE, decreasing = TRUE), ]
synopticData$SUBBA <- as.integer(trunc(synopticData$SITE))
extraVars <- read.csv('www/extraVars.csv')
synopticData <- merge(synopticData, extraVars, by="SUBBA")
synopticData <- synopticData %>% filter(SITE != 27.3) #remove waste water treatment plant 
getApproximateSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
var = synopticData$DATE# as.Date(c("2/14/2022","6/10/2022"), format = "%m/%d/%Y")
synopticData$Season = paste(getApproximateSeason(var),format(var,"%Y"))
synopticSeasons = unique(synopticData$Season)
#synopticDates <- unique(synopticData$DATE)


#Plotly
emptyFig <- plot_ly()

#UI
checkIfEmpty = function(x){return(all(is.na(x)))}
unusableVars =   aggregate(synopticData,by=list(synopticData$Season),FUN=checkIfEmpty)
max_1_item_opts <- sortable_options(group = list(name = "my_shared_group", put = htmlwidgets::JS("function(to) {return to.el.children.length < 1;}"))) #Prevents drop boxes from having more than 1 element

#WQI Leaflet graphic############################################################
wqiData <- read.csv('https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/updatedData/updatedWQIDurhamData.csv')
wqiData <- wqiData[c("Station","Date","WQI")]
wqiData <- reshape(wqiData, direction = "wide", idvar = "Date", timevar = "Station")
colnames(wqiData ) = c("Date","EL1.9EC","EL5.5GC","EL5.6EC","EL7.1EC","EL7.1SEC","EL8.1GC","EL8.5SEC","EL8.6SECUT","Name")
wqiData <- wqiData[c("Date","EL1.9EC","EL5.5GC","EL5.6EC","EL7.1EC","EL7.1SEC","EL8.1GC","EL8.5SEC","EL8.6SECUT")]
wqiData$Date = as.Date(wqiData$Date, format = "%Y-%m-%d")
wqiData = na.locf(wqiData, na.rm = FALSE)
wqiData1 = distinct(wqiData, Date, .keep_all = TRUE)
toKeep = !(names(wqiData) %in% c("Date"))
wqiData1 <- wqiData[toKeep]

beatCol <- colorNumeric(palette = 'RdYlGn',  domain=c(50,100),na.color = "#808080")

pali <- brewer.pal(11,"Paired")
pali[1] <- "#d73027"
pali[2] <- "#d73027"
pali[3] <- "#d73027"
pali[4] <- "#d73027"
pali[5] <- "#d73027"
pali[6] <- "#fdae61"
pali[7] <- "#fdae61"
pali[8] <- "#fee08b"
pali[9] <- "#ffffbf"
pali[10] <- "#d9ef8b"
pali[11] <- "#a6d96a"
pali[12] <- "#1a9850"
palwqi <- colorNumeric(palette = pali,  domain=c(0,50, 100),na.color = "#808080")

stationData = read.csv('www/durham_station.csv')
stationData1 = stationData[stationData$Name %in% c("EL5.5GC","EL1.9EC","EL5.6EC","EL8.1GC","EL8.5SEC","EL7.1SEC","EL8.6SECUT","EL7.1EC"),]

vline <- function(x = 0) {
  return(list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = "black", dash="dot")))}

#leafletminicharts param3map####################################################
huc14 <- get_spatial_layer(url = "https://services1.arcgis.com/XBhYkoXKJCRHbe7M/arcgis/rest/services/Ellerbe_Creek_CatchmentsWMIP_view/FeatureServer/0") 
huc15 <- mapview(huc14)

unit <- read.csv('www/units.csv', header = TRUE, sep = ";")

param3variables = c("NH4.N.mg.L","Cl.mg.L","SO4.mg.L")

jonnyLoc<- read.csv('www/jonnySites.csv')
  
tracerData = synopticData[c("DATE","SITE",param3variables)]
tracerData = merge(tracerData, jonnyLoc, by="SITE")
tracerData <- na.omit(tracerData)
tracerData[param3variables] <- scale(tracerData[param3variables])
tracerData[param3variables] <- logistic(tracerData[param3variables])
tracerData[param3variables] <- round(tracerData[param3variables],2)

#adding some color
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", bins = bins)

#Redlining######################################################################
redlining <- read_sf('www/NCDurham1937.geojson')
holcVals <- unique(redlining$holc_grade)
redliningCol<-rev(brewer.pal(length(holcVals),"RdYlGn"))
redliningCols <- colorFactor(palette=redliningCol,levels=holcVals)

#Choro data#####################################################################
choroData = merge(synopticData,huc14,by = "SUBBA")
emptyMap = leaflet()%>% addTiles()%>%addPolygons(data = huc14, weight = 1, opacity = 1, color = "black", fillOpacity = 0)

#Bar Chart Data#################################################################
#Prepare bardatable data
EPAstandards <- read.csv("www/standard.csv")
testbarDataTable <- ambientData[,c("Station.Name","Parameter","Date.Time","Value","Unit")]
testbarDataTable$Year <-with(testbarDataTable, substr(Date.Time, 1, 4))
testbarDataTable$Year <- as.factor(testbarDataTable$Year)
#testbarDataTable$Standard <- EPAstandards$Standard[testbarDataTable$Parameter==EPAstandards$Parameter]
testbarDataTable = merge(testbarDataTable , EPAstandards, by="Parameter")

calcCompliance <- function(Standard, Value){
  if(is.na(Value) || is.na(Standard) || (Standard==0)){
    return(NA)
  }
  ratio <- Value/Standard
  if(ratio>2){
    return(">200% of the acceptable level")
  }else if(ratio>1){
    return("100% - 200% the acceptable level")
  }else{
    return("Acceptable level")
  }
}

bardatatable <- testbarDataTable %>% rowwise() %>% mutate(Regulation.compliance= calcCompliance(Standard, Value))

####
#bardatatable <- na.omit (read.csv(file = 'www/durham_data_bar.csv', header= TRUE, sep= ","))

bardatatable$Regulation.compliance <- as.factor(bardatatable$Regulation.compliance)

# Conver character to a factor with ordered level
bardatatable$Regulation.compliance <- factor(bardatatable$Regulation.compliance, order=TRUE, levels = c(">200% of the acceptable level","100% - 200% the acceptable level","80% - 99% of the acceptable level","Acceptable level"))
bardatatable$Year <- factor(bardatatable$Year, order=TRUE, levels = unique(bardatatable$Year))

#Generate station-year bar chart data
bardata <- na.omit(bardatatable %>% count(Year, Regulation.compliance, Station.Name, vars = bardatatable$Parameter))
bardata_percent <- aggregate(bardata$n, by=list(bardata$vars,bardata$Year,bardata$Station.Name), FUN=sum) 
colnames(bardata_percent) <- c('vars','Year','Station.Name','x') #changing the name of the columns for the next step
bla <- merge(bardata, bardata_percent,all=TRUE)                  # merge according to conditions (same col names)
bardata_percent1 <- bla %>% mutate (Percentage = bla$n*100 / bla$x) 

#input vector
parameters <- unique(bardatatable$Parameter) 
parameters <- as.list(parameters)

#legend in bar chart
l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2,
  orientation = 'h',
  xanchor = "center",
  x = 0.5,
  y=-0.5)

#Boxplot########################################################################
matchedparams <- as.list(con$displayedTitle[con$boxplotMatch=="yes"])
wider_durham <- ambientData[, c(2, 4:6)]

#PCA Plot Extras################################################################
#Fill in NA values with half of the detection limit
contams1 <- c("Cl.mg.L", "SO4.mg.L", "Br.mg.L", "NO3.N.mg.L", "Na.mg.L", "K.mg.L", "Mg.mg.L", "Ca.mg.L", "NH4.N.mg.L", "PO4.P", "DOC.mg.L", "TDN.mg.L")
bc_dataRepNA <- bc_data3 %>% mutate_at(vars(contams1), ~replace_na(., .005))

#remove column at bottom with NAs
bc_dataRepNA <- bc_dataRepNA[-(99),]

extraCols <- synopticData[, c("X","MedianHHIn","roadDensity_kmkm2","pipeDensity_kmkm2","Developed","Impervious")]

bc_dataRepNA <- left_join(bc_dataRepNA, extraCols)
bc_dataRepNA <- bc_dataRepNA[!duplicated(bc_dataRepNA$X), ]

pca_params <- c("roadDensity_kmkm2", "pipeDensity_kmkm2", "Impervious", "Developed", "MedianHHIn")

wider_durham <- wider_durham %>% pivot_wider(id_cols = c("Date.Time", "Station.Name"), names_from = "Parameter", values_from = "Value", values_fn = list("Value" = mean))
wider_select <- wider_durham
wider_select <- wider_select[,c("Date.Time","Station.Name","Fecal Coliform","Ammonia Nitrogen","Copper","Nitrate + Nitrite as N","Total Kjeldahl Nitrogen","Total Phosphorus","Zinc","Calcium","Magnesium")]

#for box plot
wider_select_2 <- wider_durham[,c("Date.Time", "Station.Name","Dissolved Oxygen","pH","Calcium","Magnesium","Chloride","Potassium","Sodium","Sulfate")]
wider_select_2[, c("Calcium","Magnesium","Potassium","Sodium")] <- wider_select_2[, c("Calcium","Magnesium","Potassium","Sodium")]*.001
immutable_ws <- wider_select_2


alt_bc_data3 <- synopticData[, c("DATE","Cl.mg.L","SO4.mg.L","Na.mg.L","K.mg.L","Mg.mg.L","Ca.mg.L","DO.mg.L","pH")]
#eliminate all null values
wider_select <- na.omit(wider_select)


#Sigmoid curve##################################################################
xVal = seq(from = -5, to = 5, by =.1)
yVal = logistic(xVal)
logisticFig <- plot_ly(x=xVal,y=yVal) %>% layout(title = "Logistic Function",
                                                 xaxis = list(title = "Z Score"),
                                                 yaxis = list(title = "Value used in plot"),
                                                 showlegend = F)




