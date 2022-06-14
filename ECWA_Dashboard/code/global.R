c = 5

#libraries and objects used for Over-Time Graphs
library(shiny)
library(dplyr)
library(plotly)
library(stringr)  
active_sitesReal <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/asites.csv")
active_sites_param <- active_sitesReal  
sites <- unique(active_sitesReal$Station.Name)
Parameter <- unique(active_sitesReal$Parameter)
Parameter1 <- as.data.frame(Parameter)
shortlist <- active_sites_param[, c("Parameter", "Unit")] 
units_set <- left_join(Parameter1, shortlist)
#list of 38 params with corresponding unit
units_set <- units_set[!duplicated(units_set$Parameter), ]