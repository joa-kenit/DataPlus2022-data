library(plotly)
library(lubridate)


#Attempt to Make Graph Including All Sites

active_sites_zinc <- active_sitesReal %>% filter(Parameter == "Total Phosphorus")
SitesList <- unique(active_sitesReal$Station.Name)


fig3 <- plot_ly()%>% 
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
  fig3 <- fig3 %>% add_trace(
    x = (as.Date(active_sites_zinc$Date.Time)), 
    y = active_sites_zinc$Value, name = s, type = 'scatter', mode = 'lines + markers')
}

fig3
