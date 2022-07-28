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

