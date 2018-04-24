library(shiny);

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
      tabPanel("Stocks")
    }
    else if(i == 12){
      tabPanel("Commodities")
    }
    else{
      tabPanel(stockSymbols[i],
        sidebarLayout(
          sidebarPanel(
            numericInput(paste0("ConfInputMean", i), "Mean Confidence Level", 0.95, 0, 1, step = 0.001),
            htmlOutput(paste0("ConfIntMean", i)),
            numericInput(paste0("ConfInputVar", i), "Variance Confidence Level", 0.95, 0, 1, step = 0.001),
            htmlOutput(paste0("ConfIntVar", i)),
            htmlOutput(paste0("KS", i))
          ),
          mainPanel(
            plotOutput(paste0("Hist", i)),
            plotOutput(paste0("Norm", i)),
            plotOutput(paste0("RegSelf", i)),
            htmlOutput(paste0("RegSelfInfo", i))
          )
        )
      )
    }
  }))
)

server <- function(input, output){
  output$Names <- renderText("Kyle Fram, kdf2118<br/>Joshua Lederer, jsl2255<br/>Dean Parker, dap2180")
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
      analysis <- anova(regression)
      paste("<h2 style=\"text-align:center\"> Linear Regression Coefficients: </br>Slope =", round(regression$coefficients[2], 6), ", Intercept =", round(regression$coefficients[1], 6), "</br> P-Value for Slope =", round(analysis$`Pr(>F)`[1], 6),", R<sup>2</sup>=" ,round(summary(regression)$r.squared,6))
    })
    output[[paste0("ConfIntMean", i)]] <- reactive({
      s <- t.test(stockFileData[[i]]$logReturns, conf.level = input[[paste0("ConfInputMean", i)]]);
      paste("<h3 style=\"text-align:center\">",input[[paste0("ConfInputMean", i)]]*100,"% Confidence Interval: </br>", round(s$conf.int[1],6), " < &mu; < ", round(s$conf.int[2],6), "</h3>", sep="")
    })
    output[[paste0("ConfIntVar", i)]] <- reactive({
      s <- chisqInterval(stockFileData[[i]]$logReturns, input[[paste0("ConfInputVar", i)]]);
      paste("<h3 style=\"text-align:center\">",input[[paste0("ConfInputVar", i)]]*100,"% Confidence Interval: </br>", round(s[1],6), " < &sigma;<sup>2</sup> < ", round(s[2],6), "</h3>", sep="")
    })
    output[[paste0("KS", i)]] <- reactive({
      m <- mean(stockFileData[[i]]$logReturns);
      s <- sqrt(var(stockFileData[[i]]$logReturns));
      n <- rnorm(length(stockFileData[[i]]$logReturns), m, s);
      ks <- ks.test(stockFileData[[1]]$logReturns, n);
      paste("<h3 style=\"text-align:center\"> Kolmogorov-Smirnov run vs. Normal with parameters:</br>&mu;' =", round(m,6), "</br>&sigma;' =", round(s,6), "</br>with results:</br>P-Value =", round(ks$p.value, 6))
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

shinyApp(ui, server)