# DataPlus2022-data
Welcome to our Data Plus project for summer 2022! The goal for this project was to create a dashboard to display and explore water quality data that would otherwise be difficult to access. Our focus is on the Ellerbe Creek Watershed, with our data coming from our corresponding Bass Connections team at Duke and the City of Durham. Below is a description of all of the files we used and how they contribute to our final product.

## shinyDash2
### ui.R file
The UI file is the framework for the front end of the website, controlling the organization of the different tabs and features we implemented it. This is the front-end of the different interactive plots and maps we include.

### server.R file
The server file enables the back-end functionality of the website, including the functions and code that are used to produce our vizualitations. 

### Global.R file
The Global file allows us to reduce redundant code and increase efficiency for our app by loading our data sets and packages in the Global scope of the R environment. The objects we store within this file can be accessed from the server and UI files in designing the website.  

### www folder
the www folder within the shinyDash2 folder contains all of theh different files used in our shiny app. These files range from data frames to image files. 
