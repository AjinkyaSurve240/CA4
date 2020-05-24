# Loading the dataset
New_data <- read.csv("new property price.csv")

# Filling the NA values
New_data$Cork[New_data$Cork=="n/a"] <- NA
New_data$Galway[New_data$Galway=="n/a"] <- NA
New_data$Limerick[New_data$Limerick=="n/a"] <- NA
New_data$Waterford[New_data$Waterford=="n/a"] <- NA
New_data$Other.Areas[New_data$Other.Areas=="n/a"] <- NA


# Creating a copy of dataset
Change_Data <- New_data


# Omit The Na values
New_data <- na.omit(New_data)
# Selecting only one column for time series
New_data <- subset(New_data, select = c(3))

# Using Time series for Property Prices
Dub_Price <- ts(New_data,start = c(1970), frequency = 1)
summary(Dub_Price)
plot(Dub_Price)


# Displaying the data with the help of time series functions
start(Dub_Price)
end(Dub_Price)
frequency(Dub_Price)
class(Dub_Price)
summary(Dub_Price)

# using library 
library(forecast)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

ylim <- c(min(Dub_Price), max(Dub_Price))
plot(Dub_Price, main="Raw time series")
plot(ma(Dub_Price, 2), main="Dublin Data (k=2)", ylim=ylim)
plot(ma(Dub_Price, 4), main="Dublin Data (k=4)", ylim=ylim)
plot(ma(Dub_Price, 12), main="Dublin Data (k=12)", ylim=ylim)
par(opar)

#
library(tseries)
adf.test(diff(diff(log(Dub_Price))))
frequency(Dub_Price)
cycle(Dub_Price)

#Plotting the data 
plot(Dub_Price,
     xlab="Date", 
     ylab = "Property Prices",
     main= "Property price of Dublin from 1970 to 2015")
# Add a straight line shwing the linear relationship
# between passenger numbers and time
abline(reg=lm(Dub_Price~time(Dub_Price)))

plot(aggregate(Dub_Price,FUN=mean))


boxplot(Dub_Price ~ cycle(Dub_Price),
        xlab="Date", 
        ylab = "Property Price",
        main ="Yearly Property Prices Boxplot from 1970 to 2014")


# Checking the data for Auto Correlation  or Partial Correlation
library(forecast)
acf(Dub_Price)
pacf(Dub_Price)


# Fiiting an ARIMA model
fit <- arima(Dub_Price, 
             c(0,2,1), 
             seasonal = list(order = c(1,0,0), 
                             period = 1))
fit


# Generate Forecasts 
prediction <- predict(fit, n.ahead = 5)
prediction

# Forecasting the values with confidence level of 80
forecast_Dub_Price <- forecast(fit, level = c(80), h = 5)
forecast_Dub_Price

# Displaying the forecasted values
autoplot(forecast_Dub_Price)


plot(forecast(forecast_Dub_Price, 5), xlab = "Year", ylab = "Prices ")

# Using  the Auto Arima model 
auto_arima_model <- auto.arima(Dub_Price)
auto_arima_model
# Check the accuracy of model
accuracy(auto_arima_model)
accuracy(fit)

plot(forecast(auto_arima_model, 5), xlab = "Year", ylab = "Property Prices")

plot(forecast(fit, 5), xlab = "Year", ylab = "Property Prices")


# Choosing training and testing on the dataset 
Dub_Price_train <- window(x = Dub_Price, start=c(1970), end=c(2009))
Dub_Price_test <- window(x = Dub_Price, start=c(2010))
# Displaying the train data 
Dub_Price_train

# displaying the test data
Dub_Price_test

# Fit the training data in the model
fit1 <- arima(Dub_Price_train, 
             c(0,2,1), 
             seasonal = list(order = c(1,0,0), 
                             period = 1))
fit1
rm(arima_model)
#Uisng the auto arima on training data
auto_arima_model1 <- auto.arima(Dub_Price_train)
auto_arima_model1
accuracy(auto_arima_model1)

# Predicting the data using auto arima
predict_auto_ARIMA <- forecast(auto_arima_model, 10)
predict_auto_ARIMA

# Using the fit data
predict_manual_ARIMA <- forecast(fit1, 10)
predict_manual_ARIMA

# Displaying the data which is forcasted
plot(forecast(predict_auto_ARIMA, 5), xlab = "Year", ylab = "Prices ")
plot(forecast(predict_manual_ARIMA, 10), xlab = "Year", ylab = "Prices ")


Price_test <- arima(Dub_Price_test, model=auto_arima_model)
accuracy(Price_test)





