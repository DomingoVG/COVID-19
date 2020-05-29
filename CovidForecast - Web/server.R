## ---------------------------
##
## Script name: server.R
##
## Purpose of script: Specifies server interface for coronaRisk app
##
## Author: Domingo Velazquez
##
## Date Created: 2020-03-20
##
## ---------------------------
##
## load up the packages we will need 

library(shiny)
library(plotly)
library(shinyWidgets)
library(repmis)

## ---------------------------

## source files

## ---------------------------

options(scipen = 9)

# Define server logic 

shinyServer(function(input, output, session) {
  
  please_select_a_country <- 'Please select a country or region...'
  clrDark   <- "#726C79"
  clrLight  <- "#3F2A55"
  clrOrange <- "#FF7F0E" 

  # Make global data available to session
  
  list2env(dataList[["Global"]], envir = environment())
  
  # Observer function -- Global or Country level
  # If we observe that global_or_country is changing, then update the choices in countryFinder
  
  observe({
    # change data inputs
    list2env(dataList[[input$global_or_country]], envir = parent.env(environment()))
    output$asOfDate <- renderText(format(dates[length(dates)], "%d %B %Y"))
    
    if (input$global_or_country == 'Global') {
      updateSelectizeInput(session, "countryFinder",     selected = "Mexico", choices = ddReg)
      updateSelectizeInput(session, "countryGrowthRate", selected = c("China", "Mexico", "US"), choices = ddReg)
      updateSelectizeInput(session, "countryCFI", selected = c("China", "Mexico", "US"), choices = ddReg)
    } else {
      if (input$global_or_country == 'Mexico') {
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("Baja California", "Cdmx", "Jalisco", "Quintana Roo"), choices = ddReg)
        updateSelectizeInput(session, "countryCFI", selected = c("Baja California", "Cdmx", "Jalisco", "Quintana Roo"), choices = ddReg)
      } else if (input$global_or_country == 'China') {
        dReg = ddReg[! ddReg %in% c('Anhui', 'Guangxi', 'Guizhou', 'Hainan', 'Ningxia', 'Qinghai', 'Tibet', 'Xinjiang')] # for some reason, these states do not work.  TOFIX.
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("Beijing","Hubei","Shanghai"), choices = ddReg)
        updateSelectizeInput(session, "countryCFI", selected = c("Beijing","Hubei","Shanghai"), choices = ddReg)
      } else if (input$global_or_country == 'US') {
        ddReg = ddReg[! ddReg %in% c('American Samoa')] # for some reason, these states do not work.  TOFIX.
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("California","New Jersey","New York"), choices = ddReg)
        updateSelectizeInput(session, "countryCFI", selected = c("California","New Jersey","New York"), choices = ddReg)
      } else {
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", choices = ddReg)
        updateSelectizeInput(session, "countryCFI", choices = ddReg)
      }
    }
  })
  
  # Reactive expressions for forecast page
  
    yfCast <-reactive({ # subset country for forecast page
    yI <- tsSub(timeSeriesInfections,timeSeriesInfections$Region %in% input$countryFinder)  
    yD <- tsSub(timeSeriesDeaths,timeSeriesDeaths$Region %in% input$countryFinder) 
    yR <- tsSub(timeSeriesRecoveries,timeSeriesRecoveries$Region %in% input$countryFinder)  
    yA <- tsSub(timeSeriesActive,timeSeriesActive$Region %in% input$countryFinder)
    list(yI = yI, yD = yD, yR = yR, yA = yA)
  })
  
    
  projfCast <- reactive({ # projection for forecast confirmed
    projSimple(yfCast()$yI, dates, inWindow = input$fitWinSlider, extWindow = input$fitWinSliderout)
  })
  
  projfCast2 <- reactive({ # projection for forecast death
    projSimple(yfCast()$yD, dates, inWindow = input$fitWinSlider, extWindow = input$fitWinSliderout)
  })
  
  projfCast3 <- reactive({ # projection for forecast active
    projSimple(yfCast()$yA, dates, inWindow = input$fitWinSlider, extWindow = input$fitWinSliderout)
  })
  
  
  # adjust slide input given model type
  
  observe({
    if (input$modelType){
      updateSliderInput(session, "fitWinSlider", value = 21, min = 15, max = 30)
    } else {
      updateSliderInput(session, "fitWinSlider", value = 10, min = 1, max = 15)
    }
  })
  
  plotRange <- reactive({ # get date range to plot
    yI <- yfCast()$yI
    dFrame <- data.frame(dates = as.Date(names(yI), format = "%m.%d.%y"), yI)
    dFrame_without_na <- na.omit(dFrame$yI)
    if (max(dFrame_without_na)>200) {minDate <- min(dFrame$dates[dFrame_without_na>20]); maxDate <- max(dFrame$dates)+input$fitWinSliderout} else {
      minDate <- min(dFrame$dates); maxDate <- max(dFrame$dates)+input$fitWinSliderout
    }
    list(minDate, maxDate)
  })
  
  plotRange2 <- reactive({ # get date range to plot
    yD <- yfCast()$yD
    dFrame <- data.frame(dates = as.Date(names(yD), format = "%m.%d.%y"), yD)
    dFrame_without_na <- na.omit(dFrame$yD)
    if (max(dFrame_without_na)>200) {minDate <- min(dFrame$dates[dFrame_without_na>20]); maxDate <- max(dFrame$dates)+input$fitWinSliderout} else {
      minDate <- min(dFrame$dates); maxDate <- max(dFrame$dates)+input$fitWinSliderout
    }
    list(minDate, maxDate)
  })
  
  plotRange3 <- reactive({ # get date range to plot
    yD <- yfCast()$yA
    dFrame <- data.frame(dates = as.Date(names(yD), format = "%m.%d.%y"), yA)
    dFrame_without_na <- na.omit(dFrame$yA)
    if (max(dFrame_without_na)>200) {minDate <- min(dFrame$dates[dFrame_without_na>20]); maxDate <- max(dFrame$dates)+input$fitWinSliderout} else {
      minDate <- min(dFrame$dates); maxDate <- max(dFrame$dates)+input$fitWinSliderout
    }
    list(minDate, maxDate)
  })
   
  # Prediction table confirmed
  
  output$confirmedStats <- renderTable({
    yI <- yfCast()$yI
    nn <-length(yI)
    lDat <- projfCast()$lDat
    nowThen <- format(as.integer(c(yI[nn], tail(lDat$lwr,1), tail(lDat$upr,1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Confirmed", paste("In", input$fitWinSliderout,  "days (min-max)"))
    nowThen
  }, rownames = FALSE)

  # Prediction table death 
  
  output$deathStats <- renderTable({
    yD <- yfCast()$yD
    nn <-length(yD)
    lDat <- projfCast2()$lDat
    nowThen <- format(as.integer(c(yD[nn], tail(lDat$lwr,1), tail(lDat$upr,1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Death", paste("In", input$fitWinSliderout,  "days (min-max)"))
    nowThen
  }, rownames = FALSE)
  
  # Prediction table active 
  
  output$activeStats <- renderTable({
    yA <- yfCast()$yA
    nn <-length(yA)
    lDat <- projfCast3()$lDat
    nowThen <- format(as.integer(c(yA[nn], tail(lDat$lwr,1), tail(lDat$upr,1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Active", paste("In", input$fitWinSliderout,  "days (min-max)"))
    nowThen
  }, rownames = FALSE)
  

  # Confirmed plot
  
  output$confirmedPlot <- renderPlotly({
    if (input$countryFinder != '') {
      yI <- yfCast()$yI
      yI <- data.frame(dates = as.Date(names(yI), format = "%m.%d.%y"), yI)
      lDat <- projfCast()$lDat
      date_at_peak <- projfCast()$date_at_peak
      value_at_peak <- projfCast()$value_at_peak
      pDat <- merge(yI, lDat, all = TRUE)
      yMax <- max(c(lDat$fit, yI$yI), na.rm = TRUE)*1.05
      #yTxt <- "Confirmed active cases"
      fig <- plot_ly(pDat, type = "scatter", mode = "none") %>%
        add_trace(y = ~fit,
                  x = ~dates, 
                  mode = "lines", 
                  line = list(color = clrDark), 
                  name = "Best fit", 
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$fit, 0), big.mark = ","))) %>%
        add_trace(y = ~lwr,
                  x = ~dates,
                  mode = "lines", 
                  line = list(color = "green", dash = "dash"), 
                  name = "CI lower bound",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$lwr, 0), big.mark = ","))) %>%
        add_trace(y = ~upr, 
                  x = ~dates,
                  mode = "lines", 
                  line = list(color = "red", dash = "dash"), 
                  name = "CI upper bound",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$upr, 0), big.mark = ","))) %>%
        add_trace(y = ~yI, 
                  x = ~dates,
                  mode = "markers", 
                  marker = list(color = clrLight), 
                  name = "Active cases",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$yI, 0), big.mark = ","))) %>%
        layout(showlegend = FALSE, 
               yaxis = list(range = list(0, yMax),
                            title = list(text = "Confirmed Cases"),
                            fixedrange = TRUE),
               xaxis = list(range = plotRange(),
                            title = list(text = ""),
                            fixedrange = TRUE)
        ) %>% layout(showlegend = FALSE, hovermode = "compare") %>%
                                                               config(displayModeBar = TRUE)
      if (!is.null(value_at_peak)){
        fig %>% add_trace(y = c(0,value_at_peak), 
                          x = c(date_at_peak,date_at_peak),
                          mode = "lines", 
                          line = list(color = clrLight),
                          name = "Estimated peak",
                          hoverinfo = "text+name",
                          text = format(date_at_peak, "%b, %d"))
      } else {fig}
    }
  })
  
  # Death plot
  
  output$deathPlot <- renderPlotly({
    if (input$countryFinder != '') {
      yD <- yfCast()$yD
      yD <- data.frame(dates = as.Date(names(yD), format = "%m.%d.%y"), yD)
      lDat <- projfCast2()$lDat
      date_at_peak <- projfCast2()$date_at_peak
      value_at_peak <- projfCast2()$value_at_peak
      pDat <- merge(yD, lDat, all = TRUE)
      yMax <- max(c(lDat$fit, yD$yD), na.rm = TRUE)*1.05
      #yTxt <- "Confirmed active cases"
      fig <- plot_ly(pDat, type = "scatter", mode = "none") %>%
        add_trace(y = ~fit,
                  x = ~dates, 
                  mode = "lines", 
                  line = list(color = clrDark), 
                  name = "Best fit", 
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$fit, 0), big.mark = ","))) %>%
        add_trace(y = ~lwr,
                  x = ~dates,
                  mode = "lines", 
                  line = list(color = "green", dash = "dash"), 
                  name = "CI lower bound",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$lwr, 0), big.mark = ","))) %>%
        add_trace(y = ~upr, 
                  x = ~dates,
                  mode = "lines", 
                  line = list(color = "red", dash = "dash"), 
                  name = "CI upper bound",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$upr, 0), big.mark = ","))) %>%
        add_trace(y = ~yD, 
                  x = ~dates,
                  mode = "markers", 
                  marker = list(color = clrLight), 
                  name = "Active cases",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$yD, 0), big.mark = ","))) %>%
        layout(showlegend = FALSE, 
               yaxis = list(range = list(0, yMax),
                            title = list(text = "Death Cases"),
                            fixedrange = TRUE),
               xaxis = list(range = plotRange(),
                            title = list(text = ""),
                            fixedrange = TRUE)
        )  %>% layout(showlegend = FALSE, hovermode = "compare") %>%
                                                                  config(displayModeBar = TRUE)
      if (!is.null(value_at_peak)){
        fig %>% add_trace(y = c(0,value_at_peak), 
                          x = c(date_at_peak,date_at_peak),
                          mode = "lines", 
                          line = list(color = clrLight),
                          name = "Estimated peak",
                          hoverinfo = "text+name",
                          text = format(date_at_peak, "%b, %d"))
      } else {fig}
    }
  })
  
  # Active plot
  
  output$activePlot <- renderPlotly({
    if (input$countryFinder != '') {
      yA <- yfCast()$yA
      yA <- data.frame(dates = as.Date(names(yA), format = "%m.%d.%y"), yA)
      lDat <- projfCast3()$lDat
      date_at_peak <- projfCast3()$date_at_peak
      value_at_peak <- projfCast3()$value_at_peak
      pDat <- merge(yA, lDat, all = TRUE)
      yMax <- max(c(lDat$fit, yA$yA), na.rm = TRUE)*1.05
      #yTxt <- "Confirmed active cases"
      fig <- plot_ly(pDat, type = "scatter", mode = "none") %>%
        add_trace(y = ~fit,
                  x = ~dates, 
                  mode = "lines", 
                  line = list(color = clrDark), 
                  name = "Best fit", 
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$fit, 0), big.mark = ","))) %>%
        add_trace(y = ~lwr,
                  x = ~dates,
                  mode = "lines", 
                  line = list(color = "green", dash = "dash"), 
                  name = "CI lower bound",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$lwr, 0), big.mark = ","))) %>%
        add_trace(y = ~upr, 
                  x = ~dates,
                  mode = "lines", 
                  line = list(color = "red", dash = "dash"), 
                  name = "CI upper bound",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$upr, 0), big.mark = ","))) %>%
        add_trace(y = ~yA, 
                  x = ~dates,
                  mode = "markers", 
                  marker = list(color = clrLight), 
                  name = "Active cases",
                  hoverinfo = "text+name", 
                  text = paste(format(pDat$dates, "%b %d"), format(round(pDat$yA, 0), big.mark = ","))) %>%
        layout(showlegend = FALSE, 
               yaxis = list(range = list(0, yMax),
                            title = list(text = "Active Cases"),
                            fixedrange = TRUE),
               xaxis = list(range = plotRange(),
                            title = list(text = ""),
                            fixedrange = TRUE)
        ) %>% layout(showlegend = FALSE, hovermode = "compare") %>%
                                                                 config(displayModeBar = TRUE)
      if (!is.null(value_at_peak)){
        fig %>% add_trace(y = c(0,value_at_peak), 
                          x = c(date_at_peak,date_at_peak),
                          mode = "lines", 
                          line = list(color = clrLight),
                          name = "Estimated peak",
                          hoverinfo = "text+name",
                          text = format(date_at_peak, "%b, %d"))
      } else {fig}
    }
  })
  
  # New cases confirmed
  
  output$newCasesc <- renderPlotly({
    yI <- yfCast()$yI
    newCases <- diff(yI)
    newCases <- data.frame(dates = as.Date(names(newCases), format = "%m.%d.%y"), newCases)
    fig <- plot_ly(newCases, 
                   x = ~dates, 
                   y = ~newCases, 
                   marker = list(color = "#FF3094"),
                   type = "bar", 
                   showlegend = FALSE, 
                   name = "New cases",
                   hoverinfo = "text+name", 
                   text = paste(format(newCases$dates, "%b %d"), format(round(newCases$newCases, 0), big.mark = ",")))
    fig <- fig %>% layout(xaxis = list(range = plotRange(),
                                       title = list(text = "Date")),
                          yaxis = list(title = list(text = "New Cases"))
    )  %>% layout(showlegend = FALSE, hovermode = "compare") %>%
                                                              config(displayModeBar = TRUE)
  })
  
  # New cases death
  
  output$newCasesd <- renderPlotly({
    yD <- yfCast()$yD
    newCases <- diff(yD)
    newCases <- data.frame(dates = as.Date(names(newCases), format = "%m.%d.%y"), newCases)
    fig <- plot_ly(newCases, 
                   x = ~dates, 
                   y = ~newCases, 
                   marker = list(color = "#FF3094"),
                   type = "bar", 
                   showlegend = FALSE, 
                   name = "New cases",
                   hoverinfo = "text+name", 
                   text = paste(format(newCases$dates, "%b %d"), format(round(newCases$newCases, 0), big.mark = ",")))
    fig <- fig %>% layout(xaxis = list(range = plotRange(),
                                       title = list(text = "Date")),
                          yaxis = list(title = list(text = "New cases"))
    ) %>% layout(showlegend = FALSE, hovermode = "compare") %>%
                                                             config(displayModeBar = TRUE)
  })
  
  # New cases active
  
  output$newCasesa <- renderPlotly({
    yA <- yfCast()$yA
    newCases <- diff(yA)
    newCases <- data.frame(dates = as.Date(names(newCases), format = "%m.%d.%y"), newCases)
    fig <- plot_ly(newCases, 
                   x = ~dates, 
                   y = ~newCases, 
                   marker = list(color = "#FF3094"),
                   type = "bar", 
                   showlegend = FALSE, 
                   name = "New cases",
                   hoverinfo = "text+name", 
                   text = paste(format(newCases$dates, "%b %d"), format(round(newCases$newCases, 0), big.mark = ",")))
    fig <- fig %>% layout(xaxis = list(range = plotRange(),
                                       title = list(text = "Date")),
                          yaxis = list(title = list(text = "New Cases"))
    )  %>% layout(showlegend = FALSE, hovermode = "compare") %>%
                                                              config(displayModeBar = TRUE)
  })
  
  ##### Forecast metrics ##### 
  
  output$forecastMetrics <- renderText({
    if (input$countryFinder == '') {
      please_select_a_country
    } else {
      if (input$modelType) {
        if (is.null(projfCast3()$value_at_peak)) {
          "Active cases estimated to peak beyond the forecast horizon"
        } else if (is.null(projfCast3()$date_at_peak)){
          "Active cases peaked in the past"
        } else {
          paste("Active cases estimated to peak at", format(as.integer(projfCast3()$value_at_peak), big.mark=","),"cases on", format(projfCas3t()$date_at_peak, "%d %B"))
        }
      } else {
        doubTime <- round(projfCast3()$doubling_time, 1)
        if (doubTime > 0) {
          dTime <- paste("Doubling time is", doubTime, 'days')
        } else {
          dTime <- paste("Halving time is", -doubTime, 'days')
        }
      }
    }
  })
  
  # Detection rate  
  
  output$detRate <- renderText({
    if (input$countryFinder == '') {
      please_select_a_country
    } else {
      yI <- yfCast()$yI
      yD <- yfCast()$yD
      dR<-round(detRate(yI, yD, caseFatalityRatio = input$fatalityRatioSlider), 4)*100
      if (is.na(dR)) "Insufficient data for estimation" else paste(dR,'%')
    }
  })
  

  
  ##### Prediction table true #####
  
  output$tablePredTrue <- renderTable({
    if (input$countryFinder != '') {
      yA <- yfCast()$yA
      yD <- yfCast()$yD
      yI <- yfCast()$yI
      dRate <- detRate(yI, yD, caseFatalityRatio = input$fatalityRatioSlider)
      nowDiag <- tail(yA[!is.na(yA)], 1)
      nowUndet <- nowDiag/dRate - nowDiag
      nowUndiag <- undiagnosed.infections[undiagnosed.infections$Region==input$countryFinder, ncol(undiagnosed.infections)]
      if (nowUndiag<0) nowUndiag <- 0
      nowTotal <- nowDiag+nowUndiag+nowUndet
      nowTable <- format(round(c(nowDiag, nowUndiag, nowUndet, nowTotal), 0), big.mark = ",")
      dim(nowTable) <- c(4, 1)
      rownames(nowTable)<-c("Diagnosed", "Undiagnosed", "Undetected", "Total")
      nowTable
    }
  }, rownames = TRUE, colnames = FALSE)
  
  # Reactive expressions for growth page   
  
  growthSubc <- reactive({
    subset(timeSeriesInfections, timeSeriesInfections$Region %in% input$countryGrowthRate)
  })
  
  growthSubd <- reactive({
    subset(timeSeriesDeaths, timeSeriesDeaths$Region %in% input$countryGrowthRate)
  })
  
  growthSuba <- reactive({
    subset(timeSeriesActive, timeSeriesActive$Region %in% input$countryGrowthRate)
  })
  
  cfiSub <- reactive({
    subset(timeSeriesInfections, timeSeriesInfections$Region %in% input$countryCFI)
  })
  
  # Curve-flattening confirmed
  
  output$cfi <- renderPlotly({
    colores <- c("#FF3094", "#D8D8D8", "#3F2A55", "#00C7C5", "#F9AA23")
    pDat <- cfiSub()
    pMat <- as.matrix(log(pDat[,-1]))
    row.names(pMat) <- pDat$Region
    cfiDat <- apply(pMat, MARGIN = 1, FUN = "cfi")
    cfiDat[!is.finite(cfiDat)] <- 0
    dateSub <- 3:length(dates) # date subset
    for (cc in 1:ncol(cfiDat)){
      cfiSmooth<-loess(cfiDat[,cc]~as.numeric(dates[dateSub]))
      cfiDat[,cc] <- predict(cfiSmooth, newdata = as.numeric(dates[dateSub]))
    }
    yRange <- as.list(range(cfiDat)*1.05)
    cfiDat <- data.frame(dates = dates[dateSub], cfiDat)
    fig <- plot_ly(type = "scatter", mode = "none", name = "")
    for (cc in 2:ncol(cfiDat)){
      fig <- fig %>% add_trace(y = cfiDat[,cc],
                               x = dates[dateSub],
                               mode = "lines",
                               line = list(color = colores[[cc-1]]),
                               name = colnames(cfiDat)[cc],
                               hoverinfo = "text+name", 
                               text = paste(format(cfiDat$dates, "%b %d"), round(cfiDat[,cc], 2)))
    }
    fig <- fig %>% layout(xaxis = list(title = list(text = "Date")),
                          yaxis = list(title = list(text = "Curve-Flattening Index"),
                                       range = yRange)
    )  %>% layout(showlegend = TRUE, hovermode = "compare") %>%
                                                              config(displayModeBar = TRUE)
    
  })
  
  # Growth rate confirmed
  
  output$growthRatec <- renderPlotly({
    colores <- c("#FF3094", "#D8D8D8", "#3F2A55", "#00C7C5", "#F9AA23")
    pDat <<- growthSubc()
    gRate <- as.matrix(growthRate(pDat[,-1], inWindow = ncol(pDat)-2)) #daily growth over all time
    gRateMA <- apply(gRate, # get moving average (three day window)
                     MARGIN = 1, 
                     FUN = function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 1)})
    gRateMA[is.infinite(gRateMA)] <- NA #remove Infs
    # reorganise for plotting
    gRateMA <- data.frame(dates = as.Date(colnames(gRate), format = "%m.%d.%y"), gRateMA)
    if (nrow(gRateMA)>20) gRateMA <-tail(gRateMA, 20)
    colnames(gRateMA)[-1] <- pDat$Region
    # and plot
    fig <- plot_ly(gRateMA, type = "scatter", mode = "none")
    for (cc in 2:ncol(gRateMA)){
      fig <- fig %>% add_trace(y = gRateMA[,cc],
                               x = ~dates,
                               mode = "lines", 
                               line = list(color = colores[[cc-1]]),
                               name = colnames(gRateMA)[cc],
                               hoverinfo = "text+name", 
                               text = paste(format(gRateMA$dates, "%b %d"), round(gRateMA[,cc], 1), "%"))
    }
    fig <- fig %>% layout(xaxis = list(title = list(text = "Date")),
                          yaxis = list(title = list(text = "Growth Rate Confirmed (% per day)"))
    )  %>% layout(showlegend = TRUE, hovermode = "compare") %>%
                                                              config(displayModeBar = TRUE)
  })
  
  # Growth rate death
  
  output$growthRated <- renderPlotly({
    colores <- c("#FF3094", "#D8D8D8", "#3F2A55", "#00C7C5", "#F9AA23")
    pDat <<- growthSubd()
    gRate <- as.matrix(growthRate(pDat[,-1], inWindow = ncol(pDat)-2)) #daily growth over all time
    gRateMA <- apply(gRate, # get moving average (three day window)
                     MARGIN = 1, 
                     FUN = function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 1)})
    gRateMA[is.infinite(gRateMA)] <- NA #remove Infs
    # reorganise for plotting
    gRateMA <- data.frame(dates = as.Date(colnames(gRate), format = "%m.%d.%y"), gRateMA)
    if (nrow(gRateMA)>20) gRateMA <-tail(gRateMA, 20)
    colnames(gRateMA)[-1] <- pDat$Region
    # and plot
    fig <- plot_ly(gRateMA, type = "scatter", mode = "none")
    for (cc in 2:ncol(gRateMA)){
      fig <- fig %>% add_trace(y = gRateMA[,cc],
                               x = ~dates,
                               mode = "lines", 
                               line = list(color = colores[[cc-1]]),
                               name = colnames(gRateMA)[cc],
                               hoverinfo = "text+name", 
                               text = paste(format(gRateMA$dates, "%b %d"), round(gRateMA[,cc], 1), "%"))
    }
    fig <- fig %>% layout(xaxis = list(title = list(text = "Date")),
                          yaxis = list(title = list(text = "Growth Rate Death (% per day)"))
    )  %>% layout(showlegend = TRUE, hovermode = "compare") %>%
                                                             config(displayModeBar = TRUE)
  })
  
  
  # Growth rate Active
  
  output$growthRatea <- renderPlotly({
    colores <- c("#FF3094", "#D8D8D8", "#3F2A55", "#00C7C5", "#F9AA23")
    pDat <<- growthSuba()
    gRate <- as.matrix(growthRate(pDat[,-1], inWindow = ncol(pDat)-2)) #daily growth over all time
    gRateMA <- apply(gRate, # get moving average (three day window)
                     MARGIN = 1, 
                     FUN = function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 1)})
    gRateMA[is.infinite(gRateMA)] <- NA #remove Infs
    # reorganise for plotting
    gRateMA <- data.frame(dates = as.Date(colnames(gRate), format = "%m.%d.%y"), gRateMA)
    if (nrow(gRateMA)>20) gRateMA <-tail(gRateMA, 20)
    colnames(gRateMA)[-1] <- pDat$Region
    # and plot
    fig <- plot_ly(gRateMA, type = "scatter", mode = "none")
    for (cc in 2:ncol(gRateMA)){
      fig <- fig %>% add_trace(y = gRateMA[,cc],
                               x = ~dates,
                               mode = "lines", 
                               line = list(color = colores[[cc-1]]),
                               name = colnames(gRateMA)[cc],
                               hoverinfo = "text+name", 
                               text = paste(format(gRateMA$dates, "%b %d"), round(gRateMA[,cc], 1), "%"))
    }
    fig <- fig %>% layout(xaxis = list(title = list(text = "Date")),
                          yaxis = list(title = list(text = "Growth Rate Active (% per day)"))
    )  %>% layout(showlegend = TRUE, hovermode = "compare") %>%
                                                              config(displayModeBar = TRUE)
  })
  
})
