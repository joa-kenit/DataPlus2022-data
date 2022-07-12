#Extra text to be saved and re-added after resolving merge conflicts
#Global.R
Parameter2 <- c("NH4.N.mg.L","PO4.P", "DOC.mg.L", "TDN.mg.L", "Cl.mg.L", "SO4.mg.L"
                , "Br.mg.L", "NO3.N.mg.L", "Na.mg.L", "K.mg.L", "Mg.mg.L","Ca.mg.L")
#second boxplot data
bc_data3 <- read.csv(file = "www/SS1-SS2-SS3_MERGED_DATA.csv")
as_holder2 <- bc_data3
#operation for this specific dataset
bc_data3[40, "SITE"] <- 6.1
#change dates of "2022-09-25" to "2021-09-25:
bc_data3[35:65, "DATE"] <- "2021-09-25"
dates <- c("2021-09-25","2022-02-26", "2022-06-16") 

#server.R
#Box Plot 2
output$Boxplots2 <- renderPlotly({
  
  units_label <- units_set %>% filter(Parameter == input$Param2)
  units_label <- units_label$Unit
  
  box2 <- plot_ly() %>% layout(title = paste("Box Plots of Ellerbe Creek Samplings"),
                               xaxis = list(  
                                 title = 'Date'),
                               yaxis = list(  
                                 title = paste(input$Param2))) 
  
  select_param <- input$Param2
  
  for(d in dates){
    bc_data3 <- as_holder2
    bc_data3 <- bc_data3 %>% filter(DATE == d) 
    box2 <- var2 %>% add_trace(y = bc_data3$select_param, type = "box", name = d)
  }
  
  box2
})

#ui.R

tabBox(id = "tabsetBox", width="940px", height = "940px",
       tabPanel("Variability of Sampling Sites",
                fluidRow(
                  column(3, align="left",
                         #Insert trend tool from Durham city data 
                         
                         selectInput("Param", "Select Parameter", Parameter),
                         #outPut for Plot
                  ),
                  #ploty graph
                  column(9, align = "center", plotlyOutput("Boxplots")))
       ),
       tabPanel("Variability of Sampling Dates", 
                fluidRow(
                  column(3, align="left",
                         #Insert trend tool from Durham city data 
                         
                         selectInput("Param2", "Select Parameter", Parameter2),
                         #outPut for Plot
                  ),
                  #ploty graph
                  column(9, align = "center", plotlyOutput("Boxplots2")))
       )
),
