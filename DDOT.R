library(plotly)
library(lubridate)

#trial for EL1.9EC Alkalinity


# labeling of the plot
# fig_OT <- plot_ly()%>%
#   layout(title = 'DOC Over Time', plot_bgcolor = "#e5ecf6",
#          xaxis = list(title = 'Date'),
#          yaxis = list(title = 'Dissolved Organic Carbon (mg/L)'),
#          legend = list(title=list(text='<b> Site </b>')))
# 
# #for (site in listOfSites){
# 
#   #if(site>0){
#     #filteredRes = res[res$SITE == site,2:10]
# 
# #newly filtered set
# EL1.9EC_alk <- EL1.9EC %>% filter(Parameter == "Alkalinity") %>%
#   arrange(mdy_hm(Date.Time))
# 
#     fig_OT <- fig_OT %>%
#       add_trace(x = El1.9EC_alk$Date.Time, y = El1.9EC_alk$Value,
#             type = 'scatter', mode = 'lines+markers', name = site)
#   #}
# 
# #}
# 
# fig_OT

EL8.1GC <- active_sites %>% 
  filter(Station.Name == "EL8.1GC") %>%
  filter(Parameter == "Zinc") %>% 
  arrange(mdy_hm(Date.Time))



fig <- plot_ly(x = (as.Date(EL8.1GC$Date.Time)), 
               y = EL8.1GC$Value, type = 'scatter', mode = 'lines + markers'
               , name = 'Zinc Over Time')%>% 
  layout(title = 'Zinc OVer Time',
         plot_bgcolor='#e5ecf6',  
         xaxis = list(  
           title = 'Date',
           zerolinecolor = '#ffff',  
           zerolinewidth = 2,  
           gridcolor = 'ffff'),  
         yaxis = list(  
           title = 'Zinc',
           zerolinecolor = '#ffff',  
           zerolinewidth = 2,  
           gridcolor = 'ffff'),
         showlegend = TRUE, width = 1100)
fig


