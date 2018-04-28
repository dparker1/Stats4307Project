# Install required dependencies if not already installed, then load them
# Shiny for webapp
# OneR for binning data
if(!require("shiny")){
  install.packages("shiny")
  library("shiny")
}
if(!require("OneR")){
  install.packages("OneR")
  library("OneR")
}

# Read in all file names from StockData directory and store names
stockFileNames <- paste("StockData/", list.files("./StockData"), sep = "");
stockSymbols <- sub("StockData/", "", sub(".csv", "", stockFileNames));
stockIndices <- 1:length(stockSymbols);

# Read in all file names from ComData directory
comFileNames <- paste("ComData/", list.files("./ComData"), sep = "");
comSymbols <- sub("ComData/", "", sub(".csv", "", comFileNames));
comIndices <- 1:length(comSymbols);

# Associate names with indices for input selections
names(stockIndices) <- stockSymbols;
names(comIndices) <- comSymbols;

# Allocate data storage
stockFileData <- vector(mode = "list", length = length(stockFileNames));
comFileData <- vector(mode = "list", length = length(comFileNames));

# Read in data from all stock files and calculate logReturns
for(i in 1:length(stockFileNames)){
  stockFileData[[i]] <- read.csv(stockFileNames[i]);
  stockFileData[[i]]$logReturns <- log(stockFileData[[i]]$Price/stockFileData[[i]]$Open);
  stockFileData[[i]]$Number <- 1:length(stockFileData[[i]]$Price);
}

# Read in data from all commodities files and calculate logReturns
for(i in 1:length(comFileNames)){
  comFileData[[i]] <- read.csv(comFileNames[i]);
  comFileData[[i]]$logReturns <- log(comFileData[[i]]$Price/comFileData[[i]]$Open);
  comFileData[[i]]$Number <- 1:length(comFileData[[i]]$Price);
}

# Define ui element
ui <- fluidPage(
  
  # CSS styling sourced from www/style.css
  theme = "style.css",
  
  # Name the app
  titlePanel("Stock Analytic Viewer"),
  
  # Call loop-generated functions to generalize tab creation
  do.call(tabsetPanel, lapply(0:12, function(i){
    
    # Home page tab
    if(i == 0){
      tabPanel("Home",
        htmlOutput("Names"),
        htmlOutput("Explanation")
      )
    }
    
    # Stock vs. Stock comparison tab 
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
    
    # Stock vs. Commodity comparison tab
    else if(i == 12){
      tabPanel("Commodities",
        sidebarLayout(
          sidebarPanel(
            selectInput("com", "Commodity (Independent):", comIndices, selected = 2),
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
    
    # The 10 individual solitary stock tabs
    else{
      tabPanel(stockSymbols[i],
        sidebarLayout(
          sidebarPanel(
            htmlOutput(paste0("Statistics", i)),
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

# Define server outputs
server <- function(input, output){
  
  # Names output on home page
  output$Names <- renderText("Kyle Fram, kdf2118<br/>Joshua Lederer, jsl2255<br/>Dean Parker, dap2180")
  
  # Scatter plot and regression for stock vs. stock tab
  output$StockCompScatter <- renderPlot({
    stock1 <- stockFileData[[as.numeric(input$stock1)]]$logReturns
    stock2 <- stockFileData[[as.numeric(input$stock2)]]$logReturns
    stock1Symbol <- stockSymbols[as.numeric(input$stock1)]
    stock2Symbol <- stockSymbols[as.numeric(input$stock2)]
    plot(stock1, stock2, main = paste(stock2Symbol, "vs.", stock1Symbol), xlab = stock1Symbol, ylab = stock2Symbol, col = "#36baff")
    regression <- lm(stock2 ~ stock1)
    abline(regression, col = "#ff3656")
  })
  
  # Test outputs for stock vs. stock tab
  output$StockCompTest <- reactive({
    stock1 <- stockFileData[[as.numeric(input$stock1)]]$logReturns
    stock2 <- stockFileData[[as.numeric(input$stock2)]]$logReturns
    t <- t.test(stock1, stock2)
    c <- chisqIndependence(stock1, stock2)
    regression <- lm(stock2 ~ stock1)
    sum <- summary(regression)
    paste("<h2>Test for Population Mean Difference = 0</br>P-Value =", round(t$p.value,6),
          "</br>Test for Independence</br>P-Value =", round(c$p.value,6),
          "</br>Linear Regression Coefficients:</br>Slope (&Beta;<sub>1</sub>) =", round(regression$coefficients[2], 6),
          "</br> Intercept (&Beta;<sub>0</sub>) =", round(regression$coefficients[1], 6), 
          "</br>R<sup>2</sup>=", round(sum$r.squared, 6))
  })
  
  # Residual plot for stock vs. stock tab
  output$ResidualComp <- renderPlot({
    stock1 <- stockFileData[[as.numeric(input$stock1)]]$logReturns
    stock2 <- stockFileData[[as.numeric(input$stock2)]]$logReturns
    stock1Symbol <- stockSymbols[as.numeric(input$stock1)]
    stock2Symbol <- stockSymbols[as.numeric(input$stock2)]
    regression <- lm(stock2 ~ stock1)
    res <- resid(regression)
    plot(res, main = paste(stock2Symbol, "vs.", stock1Symbol, "Residuals"), xlab = "Index", ylab = "Residual", col = "#36baff")
  })
  
  # Scatter plot and regression for stock vs. commodity tab
  output$StockComReg <- renderPlot({
      stock <- stockFileData[[as.numeric(input$stock)]]$logReturns
      com <- comFileData[[as.numeric(input$com)]]$logReturns
      stockSymbol <- stockSymbols[as.numeric(input$stock)]
      comSymbol <- comSymbols[as.numeric(input$com)]
      plot(com, stock, main = paste(stockSymbol, "vs.", comSymbol), xlab = comSymbol, ylab = stockSymbol, col = "#36baff")
      regression <- lm(stock ~ com)
      abline(regression, col = "#ff3656")
  })
  
  # Test output for stock vs. commodity tab
  output$StockComTest <- reactive({
    stock <- stockFileData[[as.numeric(input$stock)]]$logReturns
    com <- comFileData[[as.numeric(input$com)]]$logReturns
    regression <- lm(stock ~ com)
    sum <- summary(regression)
    c <- chisqIndependence(com, stock)
    paste("<h2>Test for Independence </br>P-Value =", round(c$p.value,6),
          "</br>Linear Regression Coefficients: </br>Slope (&Beta;<sub>1</sub>) =", round(regression$coefficients[2], 6),
          "</br> P-Value =", round(summary(regression)$coefficients[8], 6),
          "</br> Intercept (&Beta;<sub>0</sub>) =", round(regression$coefficients[1], 6),
          "</br> P-Value =", round(summary(regression)$coefficients[7], 6),
          "</br>R<sup>2</sup>=", round(sum$r.squared, 6))
  })
  
  # Residual plot for stock vs. scommodity tab
  output$StockComResidual <- renderPlot({
    stock <- stockFileData[[as.numeric(input$stock)]]$logReturns
    com <- comFileData[[as.numeric(input$com)]]$logReturns
    regression <- lm(stock ~ com)
    res<-resid(regression)
    plot(res, main = paste(stockSymbols[as.numeric(input$stock)], "Fitted Residuals"),
         xlab = "Index", ylab = "Residual",col = "#36baff")
  })
  
  # Multilinear regression of stock vs. all available commodities, output table
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
  
  # Residuals plot for multilinear regression
  output$StudentResidual <- renderPlot({
    reg <- multiLinearRegression(input, stockFileData, comSymbols, comFileData)
    plot(rstudent(reg), main = paste("Studentized", stockSymbols[as.numeric(input$stock)], "Multilinear Residuals"),
         xlab = "Index", ylab = "Residual",col = "#36baff")
  })
  
  # Generalized outputs for solitary stock tabs
  lapply(1:10, function(i){
    
    # Histogram
    output[[paste0("Hist", i)]] <- renderPlot({
      data <- stockFileData[[i]]$logReturns;
      hist(data, col = "#36baff", main = paste(stockSymbols[i], "Histogram"), xlab = "Log Returns")
    })
    
    # Normality plot
    output[[paste0("Norm", i)]] <- renderPlot({
      data <- stockFileData[[i]]$logReturns;
      qqnorm(data, main = paste(stockSymbols[i], "Normality Plot"), col = "#36baff");
      qqline(data, col = "#ff3656");
    })
    
    # Regression vs. Time
    output[[paste0("RegSelf", i)]] <- renderPlot({
      plot(stockFileData[[i]]$Number, stockFileData[[i]]$logReturns, main = paste(stockSymbols[i], "Regression"),
           xlab = "Weekdays into year", ylab = "Log Returns", col = "#36baff")
      regression <- lm(logReturns ~ Number, stockFileData[[i]])
      abline(regression, col = "#ff3656")
    })
    
    # Regression statistics output
    output[[paste0("RegSelfInfo", i)]] <- reactive({
      regression <- lm(logReturns ~ Number, stockFileData[[i]])
      paste("<h2> Linear Regression Coefficients: </br>Slope (&Beta;<sub>1</sub>) =",
            round(regression$coefficients[2], 6),"</br> P-Value =", round(summary(regression)$coefficients[8],6),
            "</br> Intercept (&Beta;<sub>0</sub>) =", round(regression$coefficients[1], 6), "</br> P-Value =",
            round(summary(regression)$coefficients[7],6),"</br> R<sup>2</sup>=" ,round(summary(regression)$r.squared,6), "</h2>")
    })
    
    # Confidence interval for population mean
    output[[paste0("ConfIntMean", i)]] <- reactive({
      s <- t.test(stockFileData[[i]]$logReturns, conf.level = input[[paste0("ConfInputMean", i)]]);
      paste("<h3>",input[[paste0("ConfInputMean", i)]]*100,"% Confidence Interval: </br>",
            round(s$conf.int[1],6), " < &mu; < ", round(s$conf.int[2],6), "</h3>", sep="")
    })
    
    # Confidence interval for population variance
    output[[paste0("ConfIntVar", i)]] <- reactive({
      s <- chisqInterval(stockFileData[[i]]$logReturns, input[[paste0("ConfInputVar", i)]]);
      paste("<h3>",input[[paste0("ConfInputVar", i)]]*100,"% Confidence Interval: </br>",
            round(s[1],6), " < &sigma;<sup>2</sup> < ", round(s[2],6), "</h3>", sep="")
    })
    
    # Sample mean and variance
    output[[paste0("Statistics", i)]] <- reactive({
      m <- mean(stockFileData[[i]]$logReturns);
      v <- var(stockFileData[[i]]$logReturns);
      paste("<h3>", stockSymbols[i], "Sample Mean and Variance:</br>x&#772; =", round(m, 6), "</br>s<sup>2</sup> =", round(v, 6))
    })
    
    # Kolmogorov-Smirnov Test for normality
    output[[paste0("Normality", i)]] <- reactive({
      m <- mean(stockFileData[[i]]$logReturns);
      s <- sqrt(var(stockFileData[[i]]$logReturns));
      n <- rnorm(length(stockFileData[[i]]$logReturns), m, s);
      ks <- ks.test(stockFileData[[1]]$logReturns, n);
      paste("<h3> Kolmogorov-Smirnov run vs. Normal with parameters:</br>&mu;&#770;' =", round(m,6),
            "</br>&sigma;&#770;' =", round(s,6), "</br>with results:</br>P-Value =", round(ks$p.value, 6), "</h3>")
    })
    
    # Residual plot
    output[[paste0("RegularResidual", i)]] <- renderPlot({
      regression <- lm(logReturns ~ Number, stockFileData[[i]])
      res <- resid(regression)
      plot(res, main = paste(stockSymbols[i], "Fitted Residuals"), xlab = "Index", ylab = "Residual",col = "#36baff")
    })
  })
}

# Function to generate a chi-squared based interval for population variance
chisqInterval <- function(x, cf){
  r <- rep(0, 2);
  df <- length(x) - 1;
  r[1] <- df*var(x)/qchisq(1-(1-cf)/2, df);
  r[2] <- df*var(x)/qchisq((1-cf)/2, df);
  r
}

# Function to bin data and run a chi-squared independence test
chisqIndependence <- function(x, y){
  tabl <- table(bin(data.frame(x, y)))
  c <- chisq.test(tabl)
}

# Function to construct a linear formula based on commodity file names and then perform a multilinear regression on it
multiLinearRegression <- function(input, stockFileData, comSymbols, comFileData){
  # Prepare the data frame with appropriate names and size, replace spaces in names with underscore
  df = data.frame(matrix(vector(), 252, length(comSymbols)+1, dimnames =list(c(),c("stock", sub(" ", "_", comSymbols)))))
  df[["stock"]] <- stockFileData[[as.numeric(input$stock)]]$logReturns
  s <- ""
  # Append name to a string with a plus sign
  for (i in 1:length(comFileData)){
    s <- paste0(s, sub(" ", "_", comSymbols[i]), "+")
    df[[i+1]] <- comFileData[[i]]$logReturns
  }
  # Remove trailing plus sign, and add the response variable to the beginning
  s <- sub("\\+$", "", s)
  f <- as.formula(paste("stock ~", s))
  # Return model
  lm(f, df)
}

# Call shiny runtime
shinyApp(ui, server)