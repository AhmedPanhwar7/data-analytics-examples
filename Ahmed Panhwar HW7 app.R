#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)      
library(dplyr)      
library(ggplot2)  
library(janitor) 
data(mtcars)
data.table::data.table(mtcars)
library(tidyverse)
options(rsconnect.max.bundle.size = 30000000000)
##-------------------------------------#loading packages 



# Define UI for application

vars <- setdiff(names(mtcars), c("abc","def"))#selecting drop down columns

ui <- pageWithSidebar(
  headerPanel('Data Visualization using Clustering with K-means method'),
  
  sidebarPanel(
    h4(tags$a(href = "https://shiny.rstudio.com/gallery/", "Learn the usage of this Visualization.")),
    h4("Choose different X and Y variables using the drop down option, and select the numbers of clusters manually or by putting cursor on up or down."),
    selectInput('x', 'X Variable', vars),
    selectInput('y', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('clusters', 'Cluster count', 2, min = 1, max = 100)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)



# Define server logic 
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    mtcars[, c(input$x, input$y)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#4393c3", "#c7442f", "#2166ac", "#f4a582",
              "#fddbc7", "#eff113", "#A65628", "#d1e5f0", "#92c5de"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}
# Run the APP 
shinyApp(ui = ui, server = server)
