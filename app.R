library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)

library(readxl)


ui <- fluidPage(
  DTOutput("main"),
  plotlyOutput('plot')
)

server <- function(input, output, session) {
  df<-read_xlsx("data/SalesData.xlsx")
  
  
  dt<-reactive({
    df
  })
  
  
  output$main<-renderDT({
    dt()
  })
  
  
}

shinyApp(ui, server)