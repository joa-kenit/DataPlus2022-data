#Read in web scrapping library
library(rvest)
#Open website
# durhamCityWebsite <- read_html("http://durhamwaterquality.org/index.php")
# print(durhamCityWebsite %>% html_elements("p") %>% html_text())
# 
# #Log into website 
# search <- html_form(durhamCityWebsite)[[1]]
# search <- search %>% html_form_set(uname = "jtsenane", passwd = "data+")
# resp <- html_form_submit(search)
# durhamCityWebsiteLoggedIn <- read_html(resp)
# 
# parameterData <- durhamCityWebsiteLoggedIn%>% html_node("img")
# parameterData <- durhamCityWebsiteLoggedIn%>% html_node("a")
# parameterData <- durhamCityWebsiteLoggedIn%>% html_node("option:nth-child(14)")
# print(parameterData %>% html_text())



updatedData <- function(){
  tryCatch(
    expr = {
      #Read data updated May 2022 from github
      oldData <- read.csv("https://raw.githubusercontent.com/joa-kenit/DataPlus2022-data/main/asites.csv")

      #Read 
      stations = unique(oldData$Station.Name)
      stationsSTR = ""
      for(station in stations){
        stationsSTR = paste0(stationsSTR,"&station%5B%5D=",station)
      }
      
      parameters = unique(oldData$Parameter)
      parameters = c("pH","Total Kjeldahl Nitrogen")
      parametersSTR = ""
      for(parameter in parameters){
        parameter = gsub(" ", "+", parameter)
        parametersSTR = paste0(parametersSTR,"&parameter%5B%5D=",parameter)
      }
      
      durhamCityURL = paste0('http://durhamwaterquality.org/stationQuery1.php?category=name',stationsSTR,'&medium=Water',parametersSTR,'&project%5B%5D=Ambient&beginDate=2003-01-01&endDate=',Sys.Date(),'&p=station&logged_in=1&logged_in=1&qa%5B%5D=3&submit=View+and+Download')
      durhamCityWebsite <- read_html(durhamCityURL)
      dataframe = durhamCityWebsite%>% html_node("table.sortable") %>% html_table(header=TRUE)
      rm(oldData)
      message("Successfully pulled data from the Durham city website.")
      return(dataframe)
    },
    error = function(e){
      return(oldData)
      message('Attempted to pull new data from the city of Durham and failed. Now using old data')
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

my_data = updatedData()

write_csv(my_data,paste0('data/updatedDurhamData.csv'))  



      
