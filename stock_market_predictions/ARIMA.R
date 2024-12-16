# Load necessary libraries
install.packages("readxl")
library(forecast)
library(tseries) 
library(readxl)

#load data
corticeira <- read_excel("CORTICEIRA_AMORIM.xls", skip = 3)
ctt <- read_excel("CTT_CORREIOS_PORT.xls", skip = 3)
galp <- read_excel("GALP_ENERGIA_NOM.xls", skip = 3)
martifer <- read_excel("MARTIFER.xls", skip = 3)

#initialize list to store the models and forecasts
models <- list()
forecasts <- list()

datasets <- list(corticeira, ctt, galp, martifer)
dataset_names <- list("corticeira", "ctt", "galp", "martifer")

for (i in seq_along(datasets)) {
  #ensuring that 'Close' is numeric and no commas are present
  datasets[[i]]$Close <- as.numeric(gsub(",", ".", datasets[[i]]$Close))
  
  #calculate log returns and fit with arima
  log_returns <- c(NA, diff(log(datasets[[i]]$Close)))
  datasets[[i]]$LogReturns <- log_returns
  datasets[[i]] <- na.omit(datasets[[i]])
  models[[dataset_names[[i]]]] <- auto.arima(datasets[[i]]$LogReturns)
  
  #forecast 5 periods ahead
  forecasts[[dataset_names[[i]]]] <- forecast(models[[dataset_names[[i]]]], h = 5, level = c(95))
  
  #plot
  plot(forecasts[[dataset_names[[i]]]])
}

