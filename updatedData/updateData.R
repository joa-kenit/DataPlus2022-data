#Read in web scrapping library
library(rvest)

updatedData <- function(){
  tryCatch(
    expr = {
      #Generate station portion of URL
      stations = c("EL5.5GC","EL1.9EC","EL5.6EC","EL8.1GC","EL8.5SEC","EL7.1SEC","EL8.6SECUT","EL7.1EC")
      stationsSTR = ""
      for(station in stations){
        stationsSTR = paste0(stationsSTR,"&station%5B%5D=",station)
      }
      
      #Generate parameter portion of URL
      oldData <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/oldData/parameterDurhamData.csv")
      parameters = unique(oldData$Parameter)
      parametersSTR = ""
      for(parameter in parameters){
        parameter = gsub(" ", "+", parameter)
        parametersSTR = paste0(parametersSTR,"&parameter%5B%5D=",parameter)
      }
      
      #Generate full URL
      paramURL = paste0('http://durhamwaterquality.org/stationQuery1.php?category=name',stationsSTR,'&medium=Water','&parameter%5B%5D=all','&project%5B%5D=Ambient&beginDate=2003-01-01&endDate=',Sys.Date(),'&p=station&logged_in=1&logged_in=1&qa%5B%5D=3&submit=View+and+Download')
      WQI_URL  = paste0('http://durhamwaterquality.org/wqiQuery1.php?category=name',stationsSTR,'&beginDate=2003-01-01&endDate=',Sys.Date(),'&p=wqi&statistics=year&submit=View+and+Download')
      
      #Get website from URL
      durhamCityWebsite_param <- read_html(paramURL)
      durhamCityWebsite_WQI <- read_html(WQI_URL)
      
      df_param = durhamCityWebsite_param%>% html_node("table.sortable") %>% html_table(header=TRUE)
      df_WQI = durhamCityWebsite_WQI%>% html_node("table.sortable") %>% html_table(header=TRUE)
      message("Successfully pulled data from the Durham city website.")
      return(list(df_param, df_WQI))
    },
    error = function(e){
      message('Attempted to pull new data from the city of Durham and failed.')
      print(e)
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
    },
    finally = {
      message('All done, quitting.')
    }
  )    
}

#Run function to get data by parameter and water quality index data
data_list = updatedData()
param_data = data_list[1]
WQI_data = data_list[2]

#Save data
write.csv(param_data,'updatedData/updatedParameterDurhamData.csv')  
write.csv(WQI_data,'updatedData/updatedWQIDurhamData.csv')  

      
