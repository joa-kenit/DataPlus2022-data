library(plotly)

res <-read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/02-2022_SS_MERGED_DATA.csv",header=TRUE)
listOfSites <- unique (res$SITE)

fig <- plot_ly()%>%layout(title = 'DOC Over Time', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Date'),
                          yaxis = list(title = 'Dissolved Organic Carbon (mg/L)'), legend = list(title=list(text='<b> Site </b>')))
for (site in listOfSites){
  if(site>0){
    filteredRes = res[res$SITE == site,2:10]
    fig <- fig %>% add_trace(x = filteredRes$DATE, y = filteredRes$DOC..mg.L., type = 'scatter', mode = 'lines+markers', name = site)
  }
}
fig
