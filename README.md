# DataPlus2022-data
Welcome to our Data Plus project for summer 2022! The goal for this project was to create a dashboard to display and explore water quality data that would otherwise be difficult to access. Our focus is on the Ellerbe Creek Watershed, with our data coming from our corresponding Bass Connections team at Duke and the City of Durham. Below is a description of all of the files we used and how they contribute to our final product.

## shinyDashboard
### ui.R file
The UI file is the framework for the front end of the website, controlling the organization of the different tabs and features we implemented it. This is the front-end of the different interactive plots and maps we include.

### server.R file
The server file enables the back-end functionality of the website, including the functions and code that are used to produce our vizualitations. 

### Global.R file
The Global file allows us to reduce redundant code and increase efficiency for our app by reading in data sets and loading packages in the Global scope of the R environment. The objects we store within this file can be accessed from the server and UI files in designing the website.  

### www folder
The www folder within the shinyDash2 folder contains some of the different files used in our shiny app. These files are read in from the Global.R file.

## .github/workflows folder
This folder contains the main.yml file, which contains code that scrapes the website that our ambient data is pushed too. When the ambient data set is updated by Durham, that same data set is updated within the updatedData folder (see below section for more). When the Shiny App is run, it then requests data from GitHub which contains the updated data. 

## updatedData
This folder contains the data sets that the Shiny App pulls from while it is being run. Because the water quality data being used is pulled from git, this allows further updates with the website whether it's through the bass connections team adding their new results or the automatic updating of ambient data. 

## asites.csv
Default data in the case that Durham alters or abandons their system of updating data to their water quality website.

## Tips and Guidance
### Publishing the Website with new Changes
Code as an alternative to using the publish Button option in RStudio:

install.packages("rsconnect")

rsconnect::setAccountInfo(name='dukerivercenter',
                          token='9CA757C8DF1F78F7C8C0DC1D4FFE1DEB',
                          secret='<SECRET>')
  
#change "path/to/your/app"
  
rsconnect::deployApp('path/to/your/app')
  
#if you want to access to the library:
  
library(rsconnect)
  
### To Keep in Mind when Running on Local Machine

  - The server has access to all of the R Environment, including objects not in the Shiny App; Make sure the site is able to run with a clear enviornment in RStudio (command: rm(list = ls()))
  - To run the Global file again, closing the app and re-running it is necessary. Reloading the app does not suffice.
  - Open the app in browser to get the full functionality of the site.
### Vizual of the Website
For those looking for a glimpse of the site without running it, below are some screenshots of what it looks like:
  
<img width="1417" alt="Screen Shot 2022-07-28 at 11 10 00 AM" src="https://user-images.githubusercontent.com/87829872/181573171-69e09831-79a2-4a59-b8d3-9b24cf08a9e0.png">
  
<img width="1025" alt="Screen Shot 2022-07-28 at 11 14 03 AM" src="https://user-images.githubusercontent.com/87829872/181574057-c6abb692-ba46-4465-b850-17e7eb67f2c4.png">

<img width="1093" alt="Screen Shot 2022-07-28 at 11 16 18 AM" src="https://user-images.githubusercontent.com/87829872/181574668-c2825abc-49e9-45d8-9f16-7a06da1b7b36.png">

<img width="1092" alt="Screen Shot 2022-07-28 at 11 16 56 AM" src="https://user-images.githubusercontent.com/87829872/181574736-15a1f3bf-d79a-4aa8-b8c5-96590f0a2812.png">


