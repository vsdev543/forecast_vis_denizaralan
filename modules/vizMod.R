viz_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    wellPanel(
    fluidRow(
      column(6,h2("Multi method forecast")),
      column(3,fileInput(ns("main_data"),"Sales Data",accept = '.xlsx')),
      column(3,
             numericInput(ns("i_val"),label = "i value",min = 1,value = 10)
             )
      )
    ),
    
    wellPanel(
      fluidRow(
      column(3,textOutput(ns("text1"))),
      column(3,textOutput(ns("text2"))),
      column(3,textOutput(ns("text3"))),
      column(3,textOutput(ns("text4")))
      ),
      textOutput(ns("text5"))
      ),
    
    
    fluidRow(
      column(4,
             wellPanel(
              h3("Exponential smoothing"),
              verbatimTextOutput(ns('es_summary')),
              plotOutput(ns("es_plot")),
              downloadButton(ns("es1_down"))
             )
             ),
      column(4,
             wellPanel(
             h3("ARIMA"),
             verbatimTextOutput(ns('arima_summary')),
             plotOutput(ns("arima_plot")),
             downloadButton(ns("arima1_down"))
             )
             ),
      column(4,
             wellPanel(
             h3("Holt winters"),
             verbatimTextOutput(ns('hw_summary')),
             plotOutput(ns("hw_plot")),
             downloadButton(ns("hw1_down"))
             )
             )
    ),
    hr(),
    wellPanel(
      wellPanel(
        h3("Best Modal"),
        DTOutput(ns("mse_dt")),
      ),
        h4("Hence the best modal is:"),
        textOutput(ns('best_modal_1')),
        
        h4("Forecast Output"),
        DTOutput(ns('best_1_dt')),
        plotOutput(ns("best_1_plot")),
        downloadButton(ns("best_down1"))
        
    ),
    
    hr(),hr(),
      h3("SİMULATION Modal"),
    fluidRow(
      column(4,
             wellPanel(
               h3("Exponential smoothing"),
               verbatimTextOutput(ns('es_summary1')),
               plotOutput(ns("es_plot1")),
               downloadButton(ns("es2_down"))
             )
      ),
      column(4,
             wellPanel(
               h3("ARIMA"),
               verbatimTextOutput(ns('arima_summary1')),
               plotOutput(ns("arima_plot1")),
               downloadButton(ns("arima2_down"))
             )
      ),
      column(4,
             wellPanel(
               h3("Holt winters"),
               verbatimTextOutput(ns('hw_summary1')),
               plotOutput(ns("hw_plot1")),
               downloadButton(ns("hw2_down"))
             )
      )
    ),
    wellPanel(
      wellPanel(
        h3("Best Modal"),
        DTOutput(ns("mse_dt1")),
      ),
      h4("Hence the best modal is:"),
      textOutput(ns('best_modal_11')),
      
      h4("Forecast Output"),
      DTOutput(ns('best_1_dt1')),
      plotOutput(ns("best_1_plot1")),
      downloadButton(ns("best_down2"))
      
    )

  )
}

viz <- function(input, output, session,df) {
  dt<-reactive({
    df
  })
  
  info1<-reactive({paste(("The models group:"), dt()[1,input$i_val+1])})
  info2<-reactive({paste(("The models screen:"), dt()[2,input$i_val+1])})
  info3<-reactive({paste(("The models country:"), dt()[3,input$i_val+1])})
  info4<-reactive({paste(("The models brand:"), dt()[4,input$i_val+1])})
  info5<-reactive({paste(("Train data:"), dt()[6:29,input$i_val+1])})
  
  output$text1<-renderText(info1())
  output$text2<-renderText(info2())
  output$text3<-renderText(info3())
  output$text4<-renderText(info4())
  output$text5<-renderText(info5())
  
  sales<-reactive({
    sales <- dt()[6:29,input$i_val+1]
    colnames(sales)<-c("a")
    sales<-sales %>%mutate(a=as.numeric(a))
    sales
  })
  
  train <- reactive({ts(sales()$a,start=c(2019,05), end=c(2021,4), frequency=12)})
  
  
  #TEST DATA
  test<- reactive({
    test<-dt()[30:35,input$i_val+1]
    colnames(test)<-c("b")
    test<-test %>%mutate(b=as.numeric(b))
    test <- ts(test$b,start=c(2021,05), end= c(2021,10),frequency=12)
    test
    })
  

  
  
  #EXPONENTİAL SMOOTHİNG ===============================================================================>
  expsmooth <- reactive({ets(train())})
  forecastexpsmooth <- reactive({forecast(expsmooth(), h=24)})
  plotexp <- reactive({autoplot(forecastexpsmooth())})
  
  
  ## EXPONENTİAL SMOOTHİNG MSE
  expsmoth_mse <- reactive({(forecastexpsmooth()$mean[1:5]-test()[1:5])^2})
  exp_sum<- reactive({sum(expsmoth_mse())})
  
  
  ## RENDER
  output$es_summary<-renderPrint({
    summary(expsmooth())
  })
  
  output$es_plot<-renderPlot({
    plotexp()
  })
  
  output$es1_down<-downloadHandler("ES_FORE.csv",function(file){
    write.csv(data.frame(forecastexpsmooth()),file,row.names = F)
  })
  
  #ARIMA ===============================================================================================>
  arima1 <-reactive({auto.arima(train())})
  arima_forecast<- reactive({forecast(arima1())})
  arima_plot <- reactive({autoplot(arima_forecast())})
  
  ## ARIMA MSE
  arima_mse <- reactive({(arima_forecast()$mean[1:5]-test()[1:5])^2})
  arima_mse_sum <- reactive({sum(arima_mse())})
  
  
  ## Render
  output$arima_summary<-renderPrint({
    summary(arima1())
  })
  
  output$arima_plot<-renderPlot({
    arima_plot()
  })
  
  output$arima1_down<-downloadHandler("ARIMA_FORE.csv",function(file){
    write.csv(data.frame(arima_forecast()),file,row.names = F)
  })
  
  #HOLT ===============================================================================================>
  Holt <- reactive({holt(train(), type = c("additive", "multiplicative"), alpha = NULL,
               beta = NULL, lead = NULL, damped = FALSE, phi = NULL, plot = TRUE)})
  
  Holtforecast <- reactive({forecast(Holt())})
  Holt_plot <- reactive({autoplot(Holtforecast())})
  
  #HOLT MSE
  Holt_mse <- reactive({(Holtforecast()$mean[1:5]- test()[1:5])^2})
  Holt_mse_sum <- reactive({sum(Holt_mse())})
  
  
  ## Render
  output$hw_summary<-renderPrint({
    summary(Holt())
  })
  
  output$hw_plot<-renderPlot({
    Holt_plot()
  })
  
  output$hw1_down<-downloadHandler(filename = "HoltWinters_FORE.csv",content = function(file){
    write.csv(data.frame(Holtforecast()),file,row.names = F)
  })
  
  
  #### Modal selection
  minimum <- reactive({min(sum(Holt_mse()), sum(arima_mse()), sum(expsmoth_mse()))})
  
  output$mse_dt<-renderDT({
    k<-data.frame(stringsAsFactors = F,
      ES_MSE = sum(expsmoth_mse()),
      ARIMA_MSE = sum(arima_mse()),
      Holt_MSE = sum(Holt_mse())
      )
    
    print(k)
    k
  })
  
  output$best_modal_1<-renderText({
    if (exp_sum() == minimum() ){k <- "Exponential Smoothing"}
    if(arima_mse_sum() == minimum()){k <- "ARIMA Modal"}
    if(Holt_mse_sum() == minimum()) {k <- "Holt Winters"}
    k
  })
  
  
  
  ## Show output
  result<-reactive({
    if (exp_sum() == minimum() ){result <- forecastexpsmooth()}
    if(arima_mse_sum() == minimum()){result <- arima_forecast()}
    if(Holt_mse_sum() == minimum()) {result <- Holtforecast()}
    result
  })
  
  output$best_1_dt<-renderDT({
    data.frame(result())
  })
  
  resultPlot<-reactive({
    autoplot(result())
  })
  
  output$best_1_plot<-renderPlot({
    resultPlot()
  })
  
  
  output$best_down1<-downloadHandler("Best_Modal_FORE.csv",function(file){
    if (exp_sum() == minimum() ){k <- "Exponential Smoothing"}
    if(arima_mse_sum() == minimum()){k <- "ARIMA Modal"}
    if(Holt_mse_sum() == minimum()) {k <- "Holt Winters"}
    
    write.csv(data.frame(Holtforecast()),file,row.names = F)
  })
  
  
  #### SİMULATION ===========================================================================>
  #### ======================================================================================>
  
  data2 <- reactive({simulate(result()$model, nsim=100, seed= NULL)})
  
  #EXPONENTİAL SMOOTHİNG ===========================================================================>
  expsmooth2 <- reactive({ets(data2())})
  forecastexpsmooth2 <- reactive({forecast(expsmooth2(), h=24)})
  plotexp2 <- reactive({autoplot(forecastexpsmooth2())})
  
  
  #EXPONENTİAL SMOOTHİNG MSE
  expsmoth_mse2 <- reactive({(forecastexpsmooth2()$mean[1:5]-test()[1:5])^2})
  exp_sum2<- reactive({sum(expsmoth_mse2())})
  
  
  ## RENDER
  output$es_summary1<-renderPrint({
    summary(expsmooth2())
  })
  
  output$es_plot1<-renderPlot({
    plotexp2()
  })
  
  output$es2_down<-downloadHandler("ES_FORE_SIMULATION.csv",function(file){
    write.csv(data.frame(forecastexpsmooth2()),file,row.names = F)
  })
  
  #ARIMA ===========================================================================>
  arima2 <-reactive({auto.arima(data2())})
  arima_forecast2<- reactive({forecast(arima2())})
  arima_plot2 <- reactive({autoplot(arima_forecast2())})
  
  #ARIMA MSE
  arima_mse2 <-reactive({(arima_forecast2()$mean[1:5]-test()[1:5])^2})
  arima_mse_sum2 <- reactive({sum(arima_mse2())})
  
  ## Render
  output$arima_summary1<-renderPrint({
    summary(arima2())
  })
  
  output$arima_plot1<-renderPlot({
    arima_plot2()
  })
  
  output$arima2_down<-downloadHandler("ARIMA_FORE_SIMULATION.csv",function(file){
    write.csv(data.frame(arima_forecast2()),file,row.names = F)
  })
  
  
  #HOLT ===========================================================================>
  Holt2 <- reactive({holt(data2(), type = c("additive", "multiplicative"), alpha = NULL,
                beta = NULL, lead = NULL, damped = FALSE, phi = NULL, plot = TRUE)})
  Holtforecast2 <- reactive({forecast(Holt2())})
  Holt_plot2 <- reactive({autoplot(Holtforecast2())})
  
  #HOLT MSE
  Holt_mse2 <- reactive({(Holtforecast2()$mean[1:5]- test()[1:5])^2})
  Holt_mse_sum2 <- reactive({sum(Holt_mse2())})
  
  ## Render
  output$hw_summary1<-renderPrint({
    summary(Holt2())
  })
  
  output$hw_plot1<-renderPlot({
    Holt_plot2()
  })
  
  output$hw2_down<-downloadHandler("HW_FORE_SIMULATION.csv",function(file){
    write.csv(data.frame(Holtforecast2()),file,row.names = F)
  })
  
  
  
  ### Modal selection
  
  minimum2 <- reactive({min(sum(Holt_mse2()), sum(arima_mse2()), sum(expsmoth_mse2()))})
  
  output$mse_dt1<-renderDT({
    k<-data.frame(stringsAsFactors = F,
                  ES_MSE = sum(expsmoth_mse2()),
                  ARIMA_MSE = sum(arima_mse2()),
                  Holt_MSE = sum(Holt_mse2())
    )
    
    print(k)
    k
  })
  
  output$best_modal_11<-renderText({
    if (exp_sum2() == minimum2() ){k <- "Exponential Smoothing"}
    if(arima_mse_sum2() == minimum2()){k <- "ARIMA Modal"}
    if(Holt_mse_sum2() == minimum2()) {k <- "Holt Winters"}
    k
  })
  
  ## Show output
  result2<-reactive({
  if (exp_sum2() == minimum2() ){result2 <- forecastexpsmooth2()}
  if(arima_mse_sum2() == minimum2()){result2 <- arima_forecast2()}
  if(Holt_mse_sum2() == minimum2()) {result2 <- Holtforecast2()}
    result2
  })
  
  
  
  
  output$best_1_dt1<-renderDT({
    data.frame(result2())
  })
  
  result2Plot1<-reactive({autoplot(result2())})
  
  output$best_1_plot1<-renderPlot({
    result2Plot1()
  })
  
  output$best_down2<-downloadHandler("Simmulation_Best_Modal_FORE.csv",function(file){
    write.csv(data.frame(result2()),file,row.names = F)
  })
  
}