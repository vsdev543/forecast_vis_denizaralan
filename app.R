library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(corrplot)
library(car)
library(readxl) # read excel
library(pander) # table
library(modelr) # add_prediction()
library(tseries)
library(forecast)
library(fable)
library(writexl)

source("modules/vizMod.R")

ui <- tags$div(
  viz_UI("viz")
)

server <- function(input, output, session) {
  df<-read_xlsx("data/SalesData.xlsx")
  
  # Date input to filter test data set range
  
  # Select input to select the SKU Codes
  
  # remove i_val
  
  # Add forecast horizon
  
  # Functionality of file input
  
  
  
  
  callModule(viz,id = 'viz',df = df)
  
  
}

shinyApp(ui, server)