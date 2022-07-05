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

library(tidyverse)
library(lubridate)
library(sf)
library(arcpullr)
library(mapview)
library(wesanderson)

tealLine = tags$hr(style="width:20%;text-align:left;margin-left:0;height:3px;border-width:0;background-color:#08d8b2")
#Trends graph
active_sitesReal <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/asites.csv")
active_sites_param <- active_sitesReal  
sites <- unique(active_sitesReal$Station.Name)
Parameter <- unique(active_sitesReal$Parameter)
Parameter1 <- as.data.frame(Parameter)
shortlist <- active_sites_param[, c("Parameter", "Unit")] 
units_set <- left_join(Parameter1, shortlist)
#list of 38 params with corresponding unit
units_set <- units_set[!duplicated(units_set$Parameter), ]

#Compare vars graph
#S+U
con <- read.csv('www/ECWAWaterQuality2021Context.csv')
#Server
jonnyData <- na.omit(read.csv('www/ECWAWaterQuality2021.csv'))
emptyFig <- plot_ly()
jonnyData1<-jonnyData[,!(names(jonnyData) %in% c("SubBasin","Type","Longitude","Latitude"))]
corr <- cor(jonnyData1)
p.mat <- cor_pmat(jonnyData1)
options(repr.plot.width = 14, repr.plot.height = 14)
corr.plot <- ggcorrplot(corr, hc.order = TRUE, type = "upper", outline.col = "white",colors = c("navy", "white", "#08d8b2"), p.mat = p.mat)
#UI
Contaminant = con$Feature[con$Type == "Contaminant"]
Infrastructure = con$Feature[con$Type == "Infrastructure"]
Demographics = con$Feature[con$Type == "Demographics"]
max_1_item_opts <- sortable_options(group = list(name = "my_shared_group", put = htmlwidgets::JS("function(to) {return to.el.children.length < 1;}"))) #Prevents drop boxes from having more than 1 element

#WQI Leaflet graphic
wqiData <- read.csv('www/wqiReport.csv')
wqiData$Date = as.Date(wqiData$Date, format = "%m/%d/%Y")
#wqiData <- na.approx(wqiData,na.rm = FALSE)
wqiData = na.locf(wqiData, na.rm = FALSE)
wqiData1 = distinct(wqiData, Date, .keep_all = TRUE)
toKeep = !(names(wqiData) %in% c("Date"))
wqiData1 <- wqiData[toKeep]

beatCol <- colorNumeric(palette = 'RdYlGn',  domain=c(50,100),na.color = "#808080")

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

#1. leafletminicharts param3map
#durham_stations <- read.csv(file= "durham_station_filtered.csv", header = TRUE, sep = ";")
durham_contaminants <- na.omit(read.csv(file= "www/durham_contaminants.csv", header = TRUE, sep = ";"))
durham_contaminants$Date.Time = as.Date (durham_contaminants$Date.Time, format = "%d/%m/%Y")

huc14 <- get_spatial_layer(url = "https://services1.arcgis.com/XBhYkoXKJCRHbe7M/arcgis/rest/services/Ellerbe_Creek_CatchmentsWMIP_view/FeatureServer/0") 
huc15 <- mapview(huc14)

#adding some color
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", bins = bins) #domain = durham_contaminants$Value, )

##############BARDATATABLE######
bardatatable <- na.omit (read.csv(file = 'www/durham_data_bar.csv', header= TRUE, sep= ";"))
bardatatable$Regulation.compliance <- as.factor(bardatatable$Regulation.compliance)

# CONVERT Character to a factor with ordered level
bardatatable$Regulation.compliance <- factor(bardatatable$Regulation.compliance, order=TRUE, levels = c("Below the standard","80% or more of the standard","Exceed the standard",">200% of the standard"))
bardatatable$Year <- factor(bardatatable$Year, order=TRUE, levels = c("2016", "2017", "2018", "2019", "2020", "2021", "2022"))

#barchart
bardata <- na.omit(bardatatable %>% count(Year, Regulation.compliance, vars = bardatatable$Parameter))

#input vector
parameters <- unique(bardatatable$Parameter) 
parameters <- as.list(parameters)
