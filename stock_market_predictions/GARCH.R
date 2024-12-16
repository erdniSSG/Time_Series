library(readxl)
library(dplyr)
library(ggplot2)
library(tseries)
install.packages("rugarch")
library(rugarch)

#load data
corticeira <- read_excel("CORTICEIRA_AMORIM.xls", skip = 3)
ctt <- read_excel("CTT_CORREIOS_PORT.xls", skip = 3)
galp <- read_excel("GALP_ENERGIA_NOM.xls", skip = 3)
martifer <- read_excel("MARTIFER.xls", skip = 3)

datasets <- list(corticeira, ctt, galp, martifer)
dataset_names <- list("corticeira", "ctt", "galp", "martifer")

#calculate log-returns for each dataset
for (i in seq_along(datasets)) {
  #reverse order of dataset
  datasets[[i]] <- datasets[[i]] %>% arrange(desc(row_number()))
  
  datasets[[i]]$Close <- as.numeric(gsub(",", ".", datasets[[i]]$Close))
  
  log_returns <- c(diff(log(datasets[[i]]$Close)), NA)
  datasets[[i]]$LogReturns <- log_returns
  
  #reverse the dataset back to the original order
  datasets[[i]] <- datasets[[i]] %>% arrange(row_number())
}

#function to fit the best GARCH model and plot results
fit_garch_model <- function(log_returns, dataset_name) {
  # Remove NA values
  log_returns <- na.omit(log_returns)
  
  #step 2: Check for stationarity
  adf_test <- adf.test(log_returns)
  cat("\nADF Test for", dataset_name, ":\n")
  print(adf_test)
  
  #step 3: Fit the GARCH model
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                     distribution.model = "std")
  garch_fit <- ugarchfit(spec = spec, data = log_returns)
  
  #step 4: Evaluate the model
  cat("\nGARCH Model Summary for", dataset_name, ":\n")
  print(garch_fit)
  
  #plot the log-returns and fitted volatility
  plot_data <- data.frame(Date = as.Date(datasets[[i]]$Date[!is.na(datasets[[i]]$LogReturns)]), 
                          LogReturns = log_returns,
                          FittedVolatility = sigma(garch_fit))
  
  p <- ggplot(plot_data, aes(x = Date)) +
    geom_line(aes(y = LogReturns), color = "blue") +
    geom_line(aes(y = FittedVolatility), color = "red") +
    ggtitle(paste("Log Returns and Fitted Volatility for", dataset_name)) +
    xlab("Date") +
    ylab("Value") +
    theme_minimal()
  
  print(p)
}

#apply the function to each dataset
for (i in seq_along(datasets)) {
  cat("\nProcessing", dataset_names[[i]], "...\n")
  fit_garch_model(datasets[[i]]$LogReturns, dataset_names[[i]])
}

