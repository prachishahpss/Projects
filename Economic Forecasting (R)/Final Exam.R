rm(list=ls())
#Load the data
Housing <- read.table("Housing.csv", header = TRUE, sep=",")
class(Housing)
Construct.ts <- ts(Housing$construct, start = c(2009, 7), freq = 12)
Price.ts <- ts(Housing$hprice, start = c(2009, 7), freq = 12)
#1
plot(Construct.ts, xlab="Years", ylab= "Total Construction Spending (millions)", main = "Total Residential Construction Spending over the years")
#2
library(forecast)
auto.arima(Construct.ts, d=FALSE, trace=TRUE, stepwise=FALSE, seasonal=FALSE, ic="aic", approximation=FALSE, method = "ML")
Construct.arma <- arima(Construct.ts, order = c(1,0,3), method="ML")
Construct.arma
#3
auto.arima(Construct.ts, trace=TRUE, stepwise=FALSE, seasonal=FALSE, ic="aic", approximation=FALSE, method = "ML")
Construct.arima <- arima(Construct.ts, order = c(3,1,1), method="ML")
Construct.arima
#4
plot(diff(Construct.ts, lag=4), lty=1)
title("Total Construction Residential Spending over the years")
Construct1 <- auto.arima(Construct.ts,trace=TRUE, stepwise=FALSE, ic="aic", approximation = FALSE, method="ML")
Construct1 <- arima(Construct.ts, order=c(3,1,0), seas=list(order = c(0,1,0), 12))
Construct1
#5
acf(Construct.arma$residuals)
acf(Construct.arima$residuals)
acf(Construct1$residuals)
AIC(Construct.arma)
AIC(Construct.arima)
AIC(Construct1)
#6
install.packages("forecast")
library(forecast)
Construct_forecast <- forecast(Construct1, h=12, level=c(95))     
plot(Construct_forecast)
#7
install.packages("dLagM")
library(dLagM)
ardl1 <- ardlDlm(x=as.numeric(Price.ts[1:135]), y=as.numeric(Construct.ts), p=2, q=13) 
summary(ardl1)
#8
Cpred <- predict(HoltWinters(Price.ts, beta=FALSE), n.ahead=6)
plot(Cpred, xlab="Years", ylab= "Housing Price Index", main = "Housing Price Index over the years")
#9
library(fpp2)
install.packages("vars")
library(vars)
VARselect(cbind(Price.ts,Construct.ts), lag.max=8, type = "const")$selection
var1<- VAR(cbind(Price.ts,Construct.ts), p=3, type="const")
summary(var1)
#10
library(MASS)
data("SP500")
SP <- SP500
SP.arima <- arima(SP, order=c(2,0,1))
summary(SP.arima)

SP.res <- resid(SP.arima)

library(tseries)
SP.garch <- garch(SP.res, trace = F) 
summary(SP.garch) 

SP.garch.res <- resid(SP.garch)[-1]
acf(SP.garch.res)
acf(SP.garch.res^2)
