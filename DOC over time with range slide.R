library(plotly)
library(quantmod)

res <-read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/02-2022_SS_MERGED_DATA.csv",header=TRUE)
listOfSites <- unique (res$SITE)

figg <- plot_ly()%>%layout(title = 'DOC Over Time', plot_bgcolor = "#FFFFFF", 
                           xaxis = list(title = 'Date',
                             rangeselector = list(
                             buttons = list(
                               list(
                                 count = 1,
                                 label = "1 mo",
                                 step = "month",
                                 stepmode = "backward"),
                               list(
                                 count = 6,
                                 label = "6 mo",
                                 step = "month",
                                 stepmode = "backward"),
                               list(
                                 count = 1,
                                 label = "1 yr",
                                 step = "year",
                                 stepmode = "backward"),
                               list(
                                 count = 1,
                                 label = "YTD",
                                 step = "year",
                                 stepmode = "todate"),
                               list(step = "all"))),
                             
                             rangeslider = list(type = "date")),
                           
                           yaxis = list(title = 'Dissolved Organic Carbon (mg/L)'), 
                        legend = list(title=list(text='<b> Site </b>')
                                      
                           


for (site in listOfSites){
  if(site>0){
    filteredRes = res[res$SITE == site,2:10]
    figg <- figg %>% add_trace(x = filteredRes$DATE, y = filteredRes$DOC..mg.L., type = 'scatter', mode = 'lines+markers', name = site)
  }
}

figg
