data <- read.csv("D:/Analytics/R/Projects/2_ucanalytics/Tractor-Sales.csv")
View(data)
str(data)
summary(data)

#plotting tractor sales data as time series 
data <- ts(data[,2],start=c(2003,1),frequency=12)
plot (data, xlab='Years', ylab='Tractor Sales')

#removing trend, differencing
plot (diff(data),ylab="Differenced tractor sales")

#log transformation of data for stationarity
plot(log10(data), ylab = "log tractor sales")

#differencing log transformed data to make the data stationary in both mean and variance
plot(diff(log10(data)), ylab='Differenced log tractor sales')

#checking ACF and PCF
par(mfrow=c(1,2))
acf(ts(diff(log10(data))), main='ACF Tractor sales')
pacf(ts(diff(log10(data))), main='PACF Tractor sales')

#identifying best fit ARIMA model
require(forecast)
Arimafit <- auto.arima(log10(data), approximation =FALSE, trace = FALSE)
summary(Arimafit)

#forecasting using ARIMA model
par(mfrow=c(1,1))
pred = predict(Arimafit, n.ahead = 36)
pred
plot(data,type='l', 
     xlim=c(2004,2018), ylim=c(1,1600), xlab='Year',ylab='Tractor sales')
lines(10^(pred$pred),col='red')
lines(10^(pred$pred+2*pred$se),col='green')
lines(10^(pred$pred-2*pred$se),col='blue')

#plotting the residuals
par(mfrow=c(1,2))
acf(ts(Arimafit$residuals),main='ACF Residual')
pacf(ts(Arimafit$residuals),main='PACF Residual')

