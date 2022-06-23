############################################
# ECWA Data Visualization ITERATION TWO    #
# by Jack Tsenane, Ryan Yu, Joanna Huertas #
# server.R file                            #
############################################
source("./global.R")
shinyServer(function(input, output, session) {
  #######################
  #Explore relations tab########################################################
  #######################
  
    # Compute the linear regression 
    
  ############
  #Downloader###################################################################
  ############
  data <- read.csv(file = 'www/merged_data.csv')
  output$downloadData0 <- downloadHandler(
    filename = function() {
      paste0("Site summary data", ".csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  data <- read.csv(file = 'www/merged_data.csv')
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("Variable data", ".csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  ################
  #Data Over Time###############################################################
  ################
  
  #######################
  #WQI leaflet over time#########################################################
  #######################
  
  
}) #End of server


