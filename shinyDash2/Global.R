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
#library(shinydashboardPlus)
library(purrr)
library(ggfortify)
library(ggplot2)

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
con <- read.csv('www/synopticDataContext.csv')
#Server
jonnyData <- read.csv('www/synopticData.csv')
bc_data3 <- jonnyData
jonnyData$DATE = as.Date(jonnyData$DATE, format = "%Y-%m-%d")
jonnyData = jonnyData[order(jonnyData$DATE, decreasing = TRUE), ]
jonnyData$SUBBA <- as.integer(trunc(jonnyData$SITE))
extraVars <- read.csv('www/extraVars.csv')
jonnyData <- merge(jonnyData, extraVars, by="SUBBA")
jonnyData <- jonnyData %>% filter(SITE != 27.3) #remove waste water treatment plant 
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
var = jonnyData$DATE# as.Date(c("2/14/2022","6/10/2022"), format = "%m/%d/%Y")
jonnyData$Season = paste(getApproximateSeason(var),format(var,"%Y"))
synopticSeasons = unique(jonnyData$Season)

#Plotly
emptyFig <- plot_ly()
#Corr plot
# newCol = con$Feature[con$Type!="None"]
# jonnyData1<-jonnyData[,(names(jonnyData) %in% newCol)]
# corr <- cor(na.omit(jonnyData1))
# p.mat <- cor_pmat(jonnyData1)
# options(repr.plot.width = 14, repr.plot.height = 14)
# corr.plot <- ggcorrplot(corr, hc.order = TRUE, type = "upper", outline.col = "white",colors = c("navy", "white", "#08d8b2"), p.mat = p.mat)
#UI
checkIfEmpty = function(x){return(all(is.na(x)))}
unusableVars =   aggregate(jonnyData,by=list(jonnyData$Season),FUN=checkIfEmpty)
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

#1. leafletminicharts param3map
#durham_stations <- read.csv(file= "durham_station_filtered.csv", header = TRUE, sep = ";")
durham_contaminants <- na.omit(read.csv(file= "www/durham_contaminants.csv", header = TRUE, sep = ";"))
durham_contaminants$Date.Time = as.Date (durham_contaminants$Date.Time, format = "%d/%m/%Y")

huc14 <- get_spatial_layer(url = "https://services1.arcgis.com/XBhYkoXKJCRHbe7M/arcgis/rest/services/Ellerbe_Creek_CatchmentsWMIP_view/FeatureServer/0") 
huc15 <- mapview(huc14)

unit <- read.csv('www/units.csv', header = TRUE, sep = ";")

param3variables = c("NH4.N.mg.L","Cl.mg.L","SO4.mg.L")

jonnyLoc<- read.csv('www/jonnySites.csv')
  
tracerData = jonnyData[c("DATE","SITE",param3variables)]
tracerData = merge(tracerData, jonnyLoc, by="SITE")
tracerData <- na.omit(tracerData)
tracerData[param3variables] <- scale(tracerData[param3variables])
tracerData[param3variables] <- logistic(tracerData[param3variables])
tracerData[param3variables] <- round(tracerData[param3variables],2)

#adding some color
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", bins = bins) #domain = durham_contaminants$Value, )

#Redlining
redlining <- read_sf('www/NCDurham1937.geojson')
facil<-na.omit(read.csv('www/ECIndustrialFacilities.csv'))
holcVals <- unique(redlining$holc_grade)
redliningCol<-rev(brewer.pal(length(holcVals),"RdYlGn"))
redliningCols <- colorFactor(palette=redliningCol,levels=holcVals)
facil$permitIcon <- ifelse(facil$NPDES.Permit.Type=="No Permit Required","industry", "industry-windows")
icons <- makeAwesomeIcon(
  icon = facil$permitIcon,
  iconColor = 'black',
  library = 'ion',
)

#Choro data
#colnames(jonnyData)[1] = "SUBBA" 
choroData = merge(jonnyData,huc14,by = "SUBBA")
emptyMap = leaflet()%>% addTiles()%>%addPolygons(data = huc14, weight = 1, opacity = 1, color = "black", fillOpacity = 0)

##############BARDATATABLE######
bardatatable <- na.omit (read.csv(file = 'www/durham_data_bar.csv', header= TRUE, sep= ","))

bardatatable$Regulation.compliance <- as.factor(bardatatable$Regulation.compliance)

# CONVERT Character to a factor with ordered level
#bardatatable$Regulation.compliance <- factor(bardatatable$Regulation.compliance, order=TRUE, levels = c("Acceptable level","80% or more of the acceptable level","Exceed the acceptable level",">200% of the acceptable level"))
bardatatable$Regulation.compliance <- factor(bardatatable$Regulation.compliance, order=TRUE, levels = c(">200% of the acceptable level","100% - 200% the acceptable level","80% - 99% of the acceptable level","Acceptable level"))
bardatatable$Year <- factor(bardatatable$Year, order=TRUE, levels = c("2016", "2017", "2018", "2019", "2020", "2021", "2022"))

#barchart
bardata <- na.omit(bardatatable %>% count(Year, Regulation.compliance, Station.Name, vars = bardatatable$Parameter))
bardata
# bardata_percent <- bardata %>%
bardata_percent <- aggregate(bardata$n, by=list(bardata$vars,bardata$Year,bardata$Station.Name), FUN=sum) 
#cahngin the name of the columns for the next step
colnames(bardata_percent) <- c('vars','Year','Station.Name','x')

# merge according to conditions (same col names)
bla <- merge(bardata, bardata_percent,all=TRUE)

bardata_percent1 <- bla %>% mutate (Percentage = bla$n*100 / bla$x) 
bardata_percent1
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

#Boxplot
matchedparams <- as.list(con$displayedTitle[con$boxplotMatch=="yes"])

wider_durham <- active_sitesReal[, c(2, 4:6)]

#PCA Plot Extras#####################
dates <- c("2021-09-25","2022-02-26", "2022-06-16") 
#bc_data3 <- bc_data3[!duplicated(bc_data3$DOC..mg.L.), ]

#change september 2022 to 2021

bc_data3[35:64, "DATE"] <- "2021-09-25"

#Fill in NA values with half of the detection limit
#df %>% mutate_at(vars(c("AAA", "BAA":"BBA", "BBB")), ~replace_na(.,0))
contams1 <- c("Cl.mg.L", "SO4.mg.L", "Br.mg.L", "NO3.N.mg.L", "Na.mg.L", 
              "K.mg.L", "Mg.mg.L", "Ca.mg.L", "NH4.N.mg.L", "PO4.P", "DOC.mg.L", "TDN.mg.L")
bc_dataRepNA <- bc_data3 %>% mutate_at(vars(contams1), ~replace_na(., .005))
#bc_data <- bc_data %>% mutate_at(vars("E.Coli..CFU.per.100mL."), ~replace_na(5))

#remove column at bottom with NAs
bc_dataRepNA <- bc_dataRepNA[-(99),]

#filter to only columns with non-NA values and relevant params
#check which columns have NA
colSums(is.na(jonnyData))

extraCols <- jonnyData[, c(2,25, 28, 30,36, 40)]

bc_dataRepNA <- left_join(bc_dataRepNA, extraCols)
bc_dataRepNA <- bc_dataRepNA[!duplicated(bc_dataRepNA$X), ]

pca_params <- c("roadDensity_kmkm2", "pipeDensity_kmkm2", "Impervious", "Developed", "MedianHHIn")

wider_durham <- wider_durham %>% pivot_wider(id_cols = c("Date.Time", "Station.Name"), names_from = "Parameter", values_from = "Value", values_fn = list("Value" = mean))

wider_select <- wider_durham


wider_select <- wider_select[, c(1:2, 6, 10, 12, 14:16, 18, 20, 21)]

#for box plot
wider_select_2 <- wider_durham[, c(1, 2, 4, 7, 20:21, 33, 37:39)]
wider_select_2[, c(5,6,8,9)] <- wider_select_2[, c(5,6,8,9)]*.001
immutable_ws <- wider_select_2




alt_bc_data3 <- bc_data3[, c(3,8, 9, 12:15,20:21)]
#eliminate all null values
wider_select <- na.omit(wider_select)


#Sigmoid curve
xVal = seq(from = -5, to = 5, by =.1)
yVal = logistic(xVal)
logisticFig <- plot_ly(x=xVal,y=yVal) %>% layout(title = "Logistic Function",
                                                 xaxis = list(title = "Z Score"),
                                                 yaxis = list(title = "Value used in plot"),
                                                 showlegend = F)




