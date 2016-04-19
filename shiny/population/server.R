#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

load(file = "data.Rdata")
load(file = "data_tmp.Rdata")


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({

    library(ggplot2)
 
    
    ggplot(data = tmp.m[tmp.m$Communes==input$select,], aes(x = variable,  y =value)) +
      geom_point() +geom_smooth(method = "lm")    + labs(title=input$select)
  })
  
})
