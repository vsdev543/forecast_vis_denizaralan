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

library(shinyWidgets)

modals<-c('es','arima','hw')


getForecastOut<-function(data,hor,modal){
  fit<-switch(modal,
            "es" = {
              ets(data)
            },
            'arima' = {
              auto.arima(data)
            },
            'hw' = {
              holt(data, type = c("additive", "multiplicative"), alpha = NULL,
                   beta = NULL, lead = NULL, damped = FALSE, phi = NULL, plot = TRUE)
            }
  )
  
  if(modal=='hw'){
    if(hor>10){
      q=10
    }else{
      q<-hor
    }
    
    fore<-forecast(fit,h=q)
  }else{
    fore<-forecast(fit,h=hor)
  }
  
  n<-min(length(fore$mean),length(data))
  
  mse_sum <-sum((fore$mean[1:n]-data[1:n])^2)
  
  p<-autoplot(fore)
  
  list(fit,fore,p,mse_sum)
}


ui <- fluidPage(tags$head(tags$link(type='text/css',href="styles.css",rel="stylesheet")),
                tags$div(style="display:flex; justify-content:space-between; padding: 15px 20px 0px 20px;margin-bottom:10px; background-color:rgba(255,255,255,0.2);",
                         h2("Title text here"),
                         fileInput('data','Data')
                         ),
                fluidRow(
                  column(9,
                         tabsetPanel(type = 'pills',
                           tabPanel(title = "Exponential Smoothing",
                             div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);",
                             plotOutput('plot-es'),hr(),
                                    DTOutput('table-es'))
                             ),
                           tabPanel(title = "ARIMA",
                                    div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);",
                                        plotOutput('plot-arima'),
                                        hr(),
                                        DTOutput('table-arima'))
                                    ),
                           tabPanel(title = "Holt Winters",
                             div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);",
                             plotOutput('plot-hw'),
                             hr(),
                                    DTOutput('table-hw'))
                             ),
                           tabPanel(title = "Modal comparison",
                                    div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);", class='grid-3',
                                        uiOutput('card-es',style='text-align:center;'),
                                        uiOutput('card-arima',style='text-align:center;'),
                                        uiOutput('card-hw',style='text-align:center;')
                                    )
                                    )
                         ),
                         hr(),
                         tags$div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);",
                                  h3("Best modal"),
                                  uiOutput('bestmodname'),
                                  plotOutput('best_plot'),
                                  hr(),
                                  DTOutput('best_forecast')
                                  )
                         ),
                  column(3,
                         tags$div(
                         uiOutput('inputsUI'),br(),br(),
                         uiOutput('subDT')
                         )
                         )
                  
                )
                
)

server <- function(input, output, session) {
  
  r<-reactiveValues(trig=TRUE,bestModal=NULL)
  
  data <- reactive({
    if (!is.null(input$data)) {
      df <- read_excel(input$data$datapath)
      # df<-read_excel("data/SalesData.xlsx")
      df<-separate(data = df,col = 1,sep = "[.]",into = c("month","year"))
      df$month<-str_sub(string = df$month,-3,-1)
      df$month<-ymd(paste(df$year,"-",df$month,"-",1))
      df$year<-NULL
      names(df)[1]<-"date"
      df
    } else{
    NULL
    }
  })
  
  sampleD<-reactive({
    if (!is.null(input$data)) {
    k<-data()%>%
      select(date,input$sku)
    }
    else{k<-NULL}
    
    k
  })
  
  sampleDTS<-reactive({
    if(length(names(sampleD()))==2){
        k<-ts(sampleD()[,2],
              start=c(
                as.integer(year(min(sampleD()$date))),
                as.integer(month((min(sampleD()$date))))
                ),
              end=c(
                as.integer(year((max(sampleD()$date)))),
                as.integer(month((max(sampleD()$date))))
                ),
              frequency=12)
    }else{k<-c()}
    
    
    if(input$sim){
      l<-readRDS("klm.Rds")
      l<-l[[2]]$model
      k<-simulate(l, nsim=input$nSim, seed= NULL)
    }

    
    k
    })
  
  # observe({
  #   if(!is.null(data())){
  #   print(
  #     min(sampleD()$date)
  #     )}
  # })
  
  output$subDT <- renderUI({
    if (!is_null(data())) {
      tags$div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);",
               h4("Subset to train"),
               renderDT({sampleD()},options = list(dom = 'tp'))
      )
    }else{
      NULL
    }
  })
  

  
  
  output$inputsUI<-renderUI({
    
    if(!is.null(data())){
      div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);",
      dateRangeInput("sampleRange",label = "Sample date range",width = "100%",start = min(data()$date),end = max(data()$date),min = min(data()$date),max = max(data()$date)),
      selectInput(inputId = 'sku',label = "SKU",choices = names(data())[-1],width = '100%'),
      numericInput(
        "hor",
        "Forecast horizon",
        value = 10,
        min = 1,
        width = "100%"
      ),
        tagList(
        materialSwitch(
          inputId = "sim",
          label = "Show Simmulation", 
          value = F,
          status = "success"
        ),
        numericInput('nSim',"Number of simulation observations",100)
        )
      )
    }
  })
  
  
  observe({
    print(r$bestModal)
    if(!is.null(data()) & length(names(sampleD()))==2){
    lapply(modals,function(i){
      all<-getForecastOut(data = sampleDTS(),hor = input$hor,modal=i)
      
      output[[paste0('plot-',i)]]<-renderPlot(all[3])
      output[[paste0('table-',i)]]<-renderDT(data.frame(all[2]))
      output[[paste0('card-',i)]]<-renderUI({
        l<-switch(i,
                  "es" = "Exponential smoothing",
                  "arima" = "ARIMA",
                  "hw" = "Holt-Winter"
                  )
        tagList(
          tags$div(style="padding:10px; border-radius:10px; background-color:rgba(255,255,255,0.2);height:150px;",
          h3(paste(l,"MSE Sum")),br(),
          all[4]
          )
        )
        })
    })
      
      # output$best_plot
      # output$best_forecast
      }
  })
  
  
  miniTbl <- reactive({
    if(!is.null(data()) & length(names(sampleD()))==2){
    k <- sapply(modals, function(i) {
      all <- getForecastOut(data = sampleDTS(),
                            hor = input$hor,
                            modal = i)
      all[4]
    })
    
    
    

    
    l<-data.frame(do.call(rbind,k))
    
    names(l)<-c('mse')
    l$model<-modals
    
    l
      }else{NULL}
    
  })
  
  output$bestmodname<-renderUI({
    if(!is.null(data()) & length(names(sampleD()))==2){
      


      k <- miniTbl()$model[miniTbl()$mse == min(miniTbl()$mse)]
      
      k<-switch(k,
                "es" = 'Exponential Smoothing',
                "arima" = "ARIMA",
                "hw" = "Holt Winters"
                )
      
    l<-paste("Best modal is:",k)
      
    h3(l)
    }
  })
  
  BestModal<-reactive({
    if(!is.null(data()) & length(names(sampleD()))==2){
    i<-miniTbl()$model[miniTbl()$mse == min(miniTbl()$mse)]
    all <- getForecastOut(data = sampleDTS(),
                          hor = input$hor,
                          modal = i)
    all
    }
    
  })
  
  
  observe({
    saveRDS(BestModal(),"klm.Rds")
  })

  
  output$best_plot<-renderPlot(BestModal()[3])
  output$best_forecast<-renderDT(data.frame(BestModal()[2]))

}

shinyApp(ui, server)