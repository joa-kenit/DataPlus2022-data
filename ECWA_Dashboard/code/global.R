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


#Trends graph
active_sitesReal <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/asites.csv")
active_sites_param <- active_sitesReal  
sites <- unique(active_sitesReal$Station.Name)
Parameter <- unique(active_sitesReal$Parameter)

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

