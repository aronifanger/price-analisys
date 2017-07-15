#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("D:/projects/PriceAnalisys/PriceAnalisys")
source("priceAnalisys.R")
source("formatData.R")
source("plotData.R")

library(shiny)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Price Analisys"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      
     sidebarPanel(
       selectInput("file", "File", 
                   choices=list.files(dataDir)),
       sliderInput("days", "Days in history", 
                   min = 0,  max = 200, value = 60, step = 10),
       sliderInput("perc", "Percentiles", 
                   min = 0,  max = 100, value = 20, step = 1),
       checkboxInput("reg", "Use Regression", 
                     value = FALSE)
      ),
     
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("history")  
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$history <- renderPlot({
      # generate bins based on input$bins from ui.R
      data = LoadData(input$file)

      goodPrice = GoodPrice(data,input$days,input$perc, input$reg)
      
      PlotHistory(data)
      PlotGoodPrices(data, goodPrice)
   })
})

# Run the application 
shinyApp(ui = ui, server = server)


