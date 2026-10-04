# ============================================================
# BDA400 - Assignment 6
# Dynamic Technical Analysis Dashboard
# Student: Ali Beheshtinejad
# ============================================================

library(shiny)
library(quantmod)
library(TTR)
library(ggplot2)

# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  titlePanel("BDA400 - Dynamic Technical Analysis Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        "symbol",
        "Stock Symbol:",
        value = "AAPL"
      ),
      
      dateRangeInput(
        "dates",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-06-30"
      ),
      
      checkboxGroupInput(
        "indicators",
        "Technical Indicators:",
        choices = c(
          "SMA (20)" = "SMA",
          "EMA (20)" = "EMA",
          "RSI (14)" = "RSI",
          "MACD" = "MACD"
        ),
        selected = c("SMA", "EMA", "RSI", "MACD")
      ),
      
      sliderInput(
        "sma_period",
        "SMA Period:",
        min = 5,
        max = 50,
        value = 20,
        step = 1
      ),
      
      sliderInput(
        "ema_period",
        "EMA Period:",
        min = 5,
        max = 50,
        value = 20,
        step = 1
      ),
      
      sliderInput(
        "rsi_buy",
        "RSI Buy Threshold:",
        min = 10,
        max = 50,
        value = 30,
        step = 1
      ),
      
      sliderInput(
        "rsi_sell",
        "RSI Sell Threshold:",
        min = 50,
        max = 90,
        value = 70,
        step = 1
      ),
      
      actionButton(
        "update",
        "Update Dashboard"
      )
    ),
    
    mainPanel(
      
      h3("Stock Price and Technical Indicators"),
      
      plotOutput(
        "stockPlot",
        height = "650px"
      ),
      
      hr(),
      
      h3("Latest Trading Signal"),
      
      verbatimTextOutput("signal"),
      
      h3("Latest Market Information"),
      
      tableOutput("latestData")
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # DOWNLOAD STOCK DATA FROM YAHOO FINANCE
  # ----------------------------------------------------------
  
  stockData <- eventReactive(input$update, {
    
    req(input$symbol)
    req(input$dates)
    
    tryCatch({
      
      getSymbols(
        toupper(input$symbol),
        src = "yahoo",
        from = input$dates[1],
        to = input$dates[2],
        auto.assign = FALSE
      )
      
    }, error = function(e) {
      
      showNotification(
        "Unable to download stock data. Check the symbol and dates.",
        type = "error"
      )
      
      return(NULL)
    })
  })
  
  
  # ----------------------------------------------------------
  # CREATE TECHNICAL INDICATORS
  # ----------------------------------------------------------
  
  technicalData <- reactive({
    
    data <- stockData()
    
    req(data)
    
    close_price <- Cl(data)
    
    sma_value <- SMA(
      close_price,
      n = input$sma_period
    )
    
    ema_value <- EMA(
      close_price,
      n = input$ema_period
    )
    
    rsi_value <- RSI(
      close_price,
      n = 14
    )
    
    macd_value <- MACD(
      close_price,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      percent = FALSE
    )
    
    result <- merge(
      data,
      sma_value,
      ema_value,
      rsi_value,
      macd_value
    )
    
    colnames(result)[
      (ncol(result) - 4):ncol(result)
    ] <- c(
      "SMA",
      "EMA",
      "RSI",
      "MACD",
      "Signal"
    )
    
    result
  })
  
  
  # ----------------------------------------------------------
  # STOCK CHART
  # ----------------------------------------------------------
  
  output$stockPlot <- renderPlot({
    
    data <- stockData()
    
    req(data)
    
    chartSeries(
      data,
      name = paste(
        toupper(input$symbol),
        "Technical Analysis"
      ),
      theme = chartTheme("white")
    )
    
    if ("SMA" %in% input$indicators) {
      
      addSMA(
        n = input$sma_period,
        col = "blue"
      )
    }
    
    if ("EMA" %in% input$indicators) {
      
      addEMA(
        n = input$ema_period,
        col = "red"
      )
    }
    
    if ("RSI" %in% input$indicators) {
      
      addRSI(
        n = 14
      )
    }
    
    if ("MACD" %in% input$indicators) {
      
      addMACD()
    }
  })
  
  
  # ----------------------------------------------------------
  # BUY / SELL / HOLD SIGNAL
  # ----------------------------------------------------------
  
  tradingSignal <- reactive({
    
    data <- technicalData()
    
    req(data)
    
    valid_data <- na.omit(data)
    
    if (nrow(valid_data) == 0) {
      
      return("Not enough data to calculate a trading signal.")
    }
    
    latest <- tail(valid_data, 1)
    
    latest_close <- as.numeric(Cl(latest))
    latest_sma <- as.numeric(latest$SMA)
    latest_rsi <- as.numeric(latest$RSI)
    latest_macd <- as.numeric(latest$MACD)
    latest_macd_signal <- as.numeric(latest$Signal)
    
    if (
      latest_close > latest_sma &&
      latest_rsi < input$rsi_buy &&
      latest_macd > latest_macd_signal
    ) {
      
      signal <- "BUY"
      
    } else if (
      latest_close < latest_sma &&
      latest_rsi > input$rsi_sell &&
      latest_macd < latest_macd_signal
    ) {
      
      signal <- "SELL"
      
    } else {
      
      signal <- "HOLD"
    }
    
    paste(
      "Stock:", toupper(input$symbol),
      "\nSignal:", signal,
      "\nClosing Price:", round(latest_close, 2),
      "\nSMA:", round(latest_sma, 2),
      "\nRSI:", round(latest_rsi, 2),
      "\nMACD:", round(latest_macd, 2)
    )
  })
  
  
  # ----------------------------------------------------------
  # DISPLAY SIGNAL
  # ----------------------------------------------------------
  
  output$signal <- renderText({
    
    tradingSignal()
    
  })
  
  
  # ----------------------------------------------------------
  # DISPLAY LATEST DATA
  # ----------------------------------------------------------
  
  output$latestData <- renderTable({
    
    data <- technicalData()
    
    req(data)
    
    valid_data <- na.omit(data)
    
    req(nrow(valid_data) > 0)
    
    latest <- tail(valid_data, 1)
    
    data.frame(
      
      Stock = toupper(input$symbol),
      
      Date = as.character(index(latest)),
      
      Close = round(
        as.numeric(Cl(latest)),
        2
      ),
      
      SMA = round(
        as.numeric(latest$SMA),
        2
      ),
      
      EMA = round(
        as.numeric(latest$EMA),
        2
      ),
      
      RSI = round(
        as.numeric(latest$RSI),
        2
      ),
      
      MACD = round(
        as.numeric(latest$MACD),
        2
      )
    )
  })
}


# ============================================================
# RUN SHINY APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)