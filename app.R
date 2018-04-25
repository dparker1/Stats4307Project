if(!require("shiny")){
  install.packages("shiny")
  library("shiny")
}
if(!require("OneR")){
  install.packages("OneR")
  library("OneR")
}

stockFileNames <- paste("StockData/", list.files("./StockData"), sep = "");
stockSymbols <- sub("StockData/", "", sub(".csv", "", stockFileNames));
stockIndices <- 1:length(stockSymbols);

comFileNames <- paste("ComData/", list.files("./ComData"), sep = "");
comSymbols <- sub("ComData/", "", sub(".csv", "", comFileNames));
comIndices <- 1:length(comSymbols);


names(stockIndices) <- stockSymbols;
names(comIndices) <- comSymbols;

stockFileData <- vector(mode = "list", length = length(stockFileNames));
comFileData <- vector(mode = "list", length = length(comFileNames));

for(i in 1:length(stockFileNames)){
  stockFileData[[i]] <- read.csv(stockFileNames[i]);
  stockFileData[[i]]$logReturns <- log(stockFileData[[i]]$Price/stockFileData[[i]]$Open);
  stockFileData[[i]]$Number <- 1:length(stockFileData[[i]]$Price);
}

for(i in 1:length(comFileNames)){
  comFileData[[i]] <- read.csv(comFileNames[i]);
  comFileData[[i]]$logReturns <- log(comFileData[[i]]$Price/comFileData[[i]]$Open);
  comFileData[[i]]$Number <- 1:length(comFileData[[i]]$Price);
}

ui <- fluidPage(
  theme = "style.css",
  titlePanel("Stock Analytic Viewer"),
  do.call(tabsetPanel, lapply(0:12, function(i){
    if(i == 0){
      tabPanel("Home",
        htmlOutput("Names")
      )
    }
    else if(i == 11){
      tabPanel("Stocks",
         sidebarLayout(
           sidebarPanel(
             selectInput("stock1", "Stock 1 (Independent):", stockIndices),
             selectInput("stock2", "Stock 2 (Dependent):", stockIndices, selected = 2)
           ),
           mainPanel(
             plotOutput("StockCompScatter"),
             htmlOutput("StockCompTest"),
             plotOutput("ResidualComp")
           )
         )
      )
    }
    else if(i == 12){
      tabPanel("Commodities",
        sidebarLayout(
          sidebarPanel(
            selectInput("com", "Commodity (Independent):", comIndices),
            selectInput("stock", "Stock (Dependent):", stockIndices)
          ),

          mainPanel(
            plotOutput("StockComReg"),
            htmlOutput("StockComTest"),
            plotOutput("StockComResidual"),
            tableOutput("MultiLinearReg"),
            plotOutput("StudentResidual")
          )
        )
  )
    }
    else{
      tabPanel(stockSymbols[i],
        sidebarLayout(
          sidebarPanel(
            numericInput(paste0("ConfInputMean", i), "Mean Confidence Level", 0.95, 0, 1, step = 0.001),
            htmlOutput(paste0("ConfIntMean", i)),
            numericInput(paste0("ConfInputVar", i), "Variance Confidence Level", 0.95, 0, 1, step = 0.001),
            htmlOutput(paste0("ConfIntVar", i)),
            htmlOutput(paste0("Normality", i))
          ),
          mainPanel(
            plotOutput(paste0("Hist", i)),
            plotOutput(paste0("Norm", i)),
            plotOutput(paste0("RegSelf", i)),
            htmlOutput(paste0("RegSelfInfo", i)),
            plotOutput(paste0("RegularResidual",i))
          )
        )
      )
    }
  }))
)

server <- function(input, output){
  output$Names <- renderText("Kyle Fram, kdf2118<br/>Joshua Lederer, jsl2255<br/>Dean Parker, dap2180")
  output$StockCompScatter <- renderPlot({
    stock1 <- stockFileData[[as.numeric(input$stock1)]]$logReturns
    stock2 <- stockFileData[[as.numeric(input$stock2)]]$logReturns
    stock1Symbol <- stockSymbols[as.numeric(input$stock1)]
    stock2Symbol <- stockSymbols[as.numeric(input$stock2)]
    plot(stock1, stock2, main = paste(stock2Symbol, "vs.", stock1Symbol), xlab = stock1Symbol, ylab = stock2Symbol)
    regression <- lm(stock2 ~ stock1)
    abline(regression)
  })
  output$StockCompTest <- reactive({
    stock1 <- stockFileData[[as.numeric(input$stock1)]]$logReturns
    stock2 <- stockFileData[[as.numeric(input$stock2)]]$logReturns
    t <- t.test(stock1, stock2)
    c <- chisqIndependence(stock1, stock2)
    regression <- lm(stock2 ~ stock1)
    sum <- summary(regression)
    
    paste("<h2 style=\"text-align:center\">Test for Population Mean Difference = 0</br>P-Value =", round(t$p.value,6),
          "</br>Test for Independence</br>P-Value =", round(c$p.value,6),
          "</br>Linear Regression Coefficients:</br>Slope (&Beta;<sub>1</sub>) =", round(regression$coefficients[2], 6),
          "</br> Intercept (&Beta;<sub>0</sub>) =", round(regression$coefficients[1], 6), 
          "</br>R<sup>2</sup>=", round(sum$r.squared, 6))
  })
  output$ResidualComp <- renderPlot({
    stock1 <- stockFileData[[as.numeric(input$stock1)]]$logReturns
    stock2 <- stockFileData[[as.numeric(input$stock2)]]$logReturns
    stock1Symbol <- stockSymbols[as.numeric(input$stock1)]
    stock2Symbol <- stockSymbols[as.numeric(input$stock2)]
    regression <- lm(stock2 ~ stock1)
    res <- resid(regression)
    plot(res, main = paste(stock2Symbol, "vs.", stock1Symbol, "Residuals"), xlab = "Index", ylab = "Residual")
  })
  output$StockComReg <- renderPlot({
      stock <- stockFileData[[as.numeric(input$stock)]]$logReturns
      com <- comFileData[[as.numeric(input$com)]]$logReturns
      stockSymbol <- stockSymbols[as.numeric(input$stock)]
      comSymbol <- comSymbols[as.numeric(input$com)]
      plot(com, stock, main = paste(stockSymbol, "vs.", comSymbol), xlab = comSymbol, ylab = stockSymbol)
      regression <- lm(stock ~ com)
      abline(regression)
  })
  output$StockComTest <- reactive({
    stock <- stockFileData[[as.numeric(input$stock)]]$logReturns
    com <- comFileData[[as.numeric(input$com)]]$logReturns
    regression <- lm(stock ~ com)
    sum <- summary(regression)
    c <- chisqIndependence(com, stock)
    paste("<h2 style=\"text-align:center\">Test for Independence </br>P-Value =", round(c$p.value,6),
          "</br>Linear Regression Coefficients: </br>Slope (&Beta;<sub>1</sub>) =", round(regression$coefficients[2], 6),
          "</br> Intercept (&Beta;<sub>0</sub>) =", round(regression$coefficients[1], 6),
          "</br>R<sup>2</sup>=", round(sum$r.squared, 6))
  })
  output$StockComResidual <- renderPlot({
    stock <- stockFileData[[as.numeric(input$stock)]]$logReturns
    com <- comFileData[[as.numeric(input$com)]]$logReturns
    regression <- lm(stock ~ com)
    res<-resid(regression)
    plot(res, main = paste(stockSymbols[as.numeric(input$stock)], "Fitted Residuals"), xlab = "Index", ylab = "Residual")
  })
  output$MultiLinearReg <- renderTable({
    reg <- multiLinearRegression(input, stockFileData, comSymbols, comFileData)
    sumMultiRegression <- summary(reg)
    ta = data.frame(matrix(vector(), length(comSymbols) + 1, 0))
    ta[["Variable"]] <- c("Intercept", comSymbols)
    ta[["Estimate"]] <- sumMultiRegression$coefficients[,"Estimate"]
    ta[["Standard Error"]] <- sumMultiRegression$coefficients[, "Std. Error"]
    ta[["t-Value"]] <- sumMultiRegression$coefficients[,"t value"]
    ta[["P-Value"]] <- sumMultiRegression$coefficients[,"Pr(>|t|)"]
    ta
  })
  output$StudentResidual <- renderPlot({
    reg <- multiLinearRegression(input, stockFileData, comSymbols, comFileData)
    plot(rstudent(reg), main = paste("Studentized", stockSymbols[as.numeric(input$stock)], "Multilinear Residuals"), xlab = "Index", ylab = "Residual")
  })
  lapply(1:10, function(i){
    output[[paste0("Hist", i)]] <- renderPlot({
      data <- stockFileData[[i]]$logReturns;
      hist(data, col = "#FF0000", main = paste(stockSymbols[i], "Histogram"), xlab = "Log Returns")
    })
    output[[paste0("Norm", i)]] <- renderPlot({
      data <- stockFileData[[i]]$logReturns;
      qqnorm(data, main = paste(stockSymbols[i], "Normality Plot"));
      qqline(data);
    })
    output[[paste0("RegSelf", i)]] <- renderPlot({
      plot(stockFileData[[i]]$Number, stockFileData[[i]]$logReturns, main = paste(stockSymbols[i], "Regression"), xlab = "Weekdays into year", ylab = "Log Returns")
      regression <- lm(logReturns ~ Number, stockFileData[[i]])
      abline(regression)
    })
    output[[paste0("RegSelfInfo", i)]] <- reactive({
      regression <- lm(logReturns ~ Number, stockFileData[[i]])
      paste("<h2 style=\"text-align:center\"> Linear Regression Coefficients: </br>Slope (&Beta;<sub>1</sub>) =",
            round(regression$coefficients[2], 6), "</br> Intercept (&Beta;<sub>0</sub>) =",
            round(regression$coefficients[1], 6), "</br> R<sup>2</sup>=" ,round(summary(regression)$r.squared,6))
    })
    output[[paste0("ConfIntMean", i)]] <- reactive({
      s <- t.test(stockFileData[[i]]$logReturns, conf.level = input[[paste0("ConfInputMean", i)]]);
      paste("<h3 style=\"text-align:center\">",input[[paste0("ConfInputMean", i)]]*100,"% Confidence Interval: </br>",
            round(s$conf.int[1],6), " < &mu; < ", round(s$conf.int[2],6), "</h3>", sep="")
    })
    output[[paste0("ConfIntVar", i)]] <- reactive({
      s <- chisqInterval(stockFileData[[i]]$logReturns, input[[paste0("ConfInputVar", i)]]);
      paste("<h3 style=\"text-align:center\">",input[[paste0("ConfInputVar", i)]]*100,"% Confidence Interval: </br>",
            round(s[1],6), " < &sigma;<sup>2</sup> < ", round(s[2],6), "</h3>", sep="")
    })
    output[[paste0("Normality", i)]] <- reactive({
      m <- mean(stockFileData[[i]]$logReturns);
      s <- sqrt(var(stockFileData[[i]]$logReturns));
      n <- rnorm(length(stockFileData[[i]]$logReturns), m, s);
      ks <- ks.test(stockFileData[[1]]$logReturns, n);
      paste("<h3 style=\"text-align:center\"> Kolmogorov-Smirnov run vs. Normal with parameters:</br>&mu;' =", round(m,6),
            "</br>&sigma;' =", round(s,6), "</br>with results:</br>P-Value =", round(ks$p.value, 6))
    })
    output[[paste0("RegularResidual", i)]] <- renderPlot({
      regression <- lm(logReturns ~ Number, stockFileData[[i]])
      res <- resid(regression)
      plot(res, main = paste(stockSymbols[i], "Fitted Residuals"), xlab = "Index", ylab = "Residual")
    })
  })
}

chisqInterval <- function(x, cf){
  r <- rep(0, 2);
  df <- length(x) - 1;
  r[1] <- df*var(x)/qchisq(1-(1-cf)/2, df);
  r[2] <- df*var(x)/qchisq((1-cf)/2, df);
  r
}

chisqIndependence <- function(x, y){
  tabl <- table(bin(data.frame(x, y)))
  c <- chisq.test(tabl)
}

multiLinearRegression <- function(input, stockFileData, comSymbols, comFileData){
  df = data.frame(matrix(vector(), 252, length(comSymbols)+1, dimnames =list(c(),c("stock", comSymbols))))
  df[["stock"]] <- stockFileData[[as.numeric(input$stock)]]$logReturns
  s <- ""
  for (i in 1:length(comFileData)){
    s <- paste0(s, comSymbols[i], "+")
    df[[i+1]] <- comFileData[[i]]$logReturns
  }
  s <- sub("\\+$", "", s)
  f <- as.formula(paste("stock ~", s))
  lm(f, df)
}

shinyApp(ui, server)