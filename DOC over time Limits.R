library(plotly)
library(quantmod)

res <-read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/02-2022_SS_MERGED_DATA.csv",header=TRUE)
listOfSites <- unique (res$SITE)

hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 8,
    xref = "paper",
    y0 = 6,
    y1 = 6,
    line = list(color = color, dash="dot")
  )
}

fig1 <- plot_ly()%>%layout(title = 'DOC Over Time', plot_bgcolor = "#FFFFFF", 
                          xaxis = list(title = 'Date'),
                          yaxis = list(title = 'Dissolved Organic Carbon (mg/L)'), 
                          legend = list(title=list(text='<b> Site </b>')),
                          shapes = list(
                            
                            list(type = "rect",
                                 fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.3,
                                 x0 = -0.5, x1 = 8.5, xref = "x",
                                 y0 = 6.02, y1 = 13.5, yref = "y"),
                            list(type = "rect",
                                 fillcolor = "green", line = list(color = "green"), opacity = 0.3,
                                 x0 = -0.5, x1 = 8.5, xref = "x",
                                 y0 = 2, y1 = 6, yref = "y")))
                          legend = list(title=list(text='<b> Site </b>'))




for (site in listOfSites){
  if(site>0){
    filteredRes = res[res$SITE == site,2:10]
    fig1 <- fig1 %>% add_trace(x = filteredRes$DATE, y = filteredRes$DOC..mg.L., type = 'scatter', mode = 'lines+markers', name = site)
  }
}
fig1
