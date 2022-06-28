library(plotly)
library(lubridate)
#test commit

#Attempt to Make Graph Including All Sites

active_sites_zinc <- active_sitesReal %>% filter(Parameter == "Total Phosphorus")
SitesList <- unique(active_sitesReal$Station.Name)
hh

fig4 <- plot_ly()%>% 
  layout(title = 'Zinc Levels Over Time',
         plot_bgcolor='#e5ecf6',  
         xaxis = list(  
           title = 'Date',
           zerolinecolor = '#ffff',  
           zerolinewidth = 2,  
           gridcolor = 'ffff'),  
         yaxis = list(  
           title = 'Zinc (ug/L)',
           zerolinecolor = '#ffff',  
           zerolinewidth = 2,  
           gridcolor = 'ffff'),
         shapes = list(
           list(type = "rect",
                fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.3,
                x0 = as.Date("2004-01-20 09:15:00"), x1 = as.Date("2022-03-01 09:15:00"), xref = "x",
                y0 = 6, y1 = 14, yref = "y"),
           list(type = "rect",
                fillcolor = "green", line = list(color = "green"), opacity = 0.3,
                x0 = -0.5, x1 = 8.5, xref = "x",
                y0 = 2, y1 = 6, yref = "y")),
         showlegend = TRUE, width = 1100)

for(s in SitesList) {
  #reset asz to original just zinc a.s (a.s is the constant)
  #active_sitesReal sorted first for date
  active_sites_zinc <- active_sitesReal %>% filter(Parameter == "Total Phosphorus") %>% 
      filter(Station.Name == s)
  print("Ran")

  # active_sites_zinc <- active_sites_zinc %>% filter(Station.Name == s) %>%
  #   order((Date.Time))
    #filter for station name and then sort by date
  fig4 <- fig4 %>% add_trace(
    x = (as.Date(active_sites_zinc$Date.Time)), 
    y = active_sites_zinc$Value, name = s, type = 'scatter', mode = 'lines + markers')
}

fig4
