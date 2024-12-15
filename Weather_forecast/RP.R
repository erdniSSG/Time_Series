# Load necessary libraries
install.packages("forecast")
library("forecast")
library("tseries")

getwd()
setwd("Documents")

# a)

#load data
soi_data_raw <- read.csv("Time_Series_SOI.txt", header = FALSE, col.names = c("YearMonth", "SOI"))

#separate and  convert the year and month
soi_data_raw$Year <- substr(soi_data_raw$YearMonth, 1, 4)
soi_data_raw$Month <- substr(soi_data_raw$YearMonth, 5, 6)
soi_data_raw$Year <- as.numeric(soi_data_raw$Year)
soi_data_raw$Month <- as.numeric(soi_data_raw$Month)
#remove the original YearMonth column
soi_data <- soi_data_raw[, -1]

#filter to only include January 2010 to January 2024
soi_data <- subset(soi_data, Year >= 2010 & (Year < 2024 | (Year == 2024 & Month == 1)))

#convert to ts object
soi_ts <- ts(soi_data$SOI, start=c(2010, 1), frequency=12)

#plot
plot(soi_ts, main="SARIMA model for SOI Time Series (January 2010 - January 2024)", xlab="Year", ylab="SOI Value")

adf_test <- adf.test(soi_ts)
print(adf_test)

#check if differencing is needed
if(adf_test$p.value > 0.05) {
  cat("The data is non-stationary, differencing is needed.\n")
  #differencing
  soi_ts_diff <- diff(soi_ts, differences = 1)
  #plot differenced ts
  plot(soi_ts_diff, main="Differenced SARIMA model for SOI Time Series (January 2010 - January 2024)", xlab="Year", ylab="Differenced SOI Value")
  
  #perform ADF test 
  adf_test_diff <- adf.test(soi_ts_diff)
  print(adf_test_diff)
}

#initial examination of the data
summary(soi_ts_diff)

#ACF and PACF to identify potential AR and MA orders
acf(soi_ts_diff)
pacf(soi_ts_diff)

# Fit SARIMA model using auto.arima 
sarima_model <- auto.arima(soi_ts)

#residual diagnostics
checkresiduals(sarima_model)

#model summary
summary(sarima_model)


# b)

#forecast the next 5 time periods (months)
forecasted_values <- forecast(sarima_model, h = 6, level = c(95))

#create a new plot for the forecasted values
plot(forecasted_values, xlim=c(2024, 2024 + (5/12)), ylim=range(forecasted_values$lower, forecasted_values$upper))

#create time series object with just forecasted values
forecasted_ts <- ts(forecasted_values$mean, start=c(2024, 2), frequency=12)

#plot
plot(forecasted_ts, type="n", xlim=c(2024.1, 2024 + (6/12)), ylim=range(c(forecasted_values$lower, forecasted_values$upper)), xlab="Year", ylab="SOI", main="Forecasted SOI Values")
lines(forecasted_ts, col="blue")

#add 95% prediction interval
lines(ts(forecasted_values$lower, start=c(2024, 2), frequency=12), col="red", lty=2)
lines(ts(forecasted_values$upper, start=c(2024, 2), frequency=12), col="red", lty=2)

legend("topleft", legend=c("Forecast", "95% Prediction Interval"), col=c("blue", "red"), lty=c(1, 2), cex=0.8)


