rm(list=ls())
#Load the data
Exrate <- read.table("Exrate.csv", header = TRUE, sep=",")
class(Exrate)
Exrate.ts <- ts(Exrate$DEXUSEU, start = c(2012, 1), freq = 12)
#plot
plot(Exrate.ts, xlab="Years", ylab= "U.S. Dollar to Euro Exchange Rate", main = "Exchange Rate over the years")
summary(Exrate.ts)
boxplot(Exrate.ts ~ cycle(Exrate.ts))
#Holt Winters
Exrate.hw <- HoltWinters(Exrate.ts, seasonal = "mult")
plot(Exrate.hw, xlab="Years", ylab= "U.S. Dollar to Euro Exchange Rate", main = "Exchange Rate over the years")
Exrate.hw$SSE
Exrate.predict <- predict(Exrate.hw, n.ahead = 5*12)
ts.plot(Exrate.ts, Exrate.predict, lty = 1:2, xlab="Years", ylab= "U.S. Dollar to Euro Exchange Rate", main = "Exchange Rate over the years")
Exrate.hw$alpha
#First-difference
plot(diff(Exrate.ts))
#Log transformation
plot(diff(log(Exrate.ts)))
#ACF & PACF
acf(Exrate.ts)
pacf(Exrate.ts)
#Best version of ARIMA model
library(forecast)
Exrate.arima <- auto.arima(Exrate.ts, trace=TRUE, stepwise=FALSE, seasonal=FALSE, ic="aic", approximation=FALSE, method = "ML")
Exrate.arima
#ACf & PACF of the residuals
acf(Exrate.arima$res[-1])
pacf(Exrate.arima$res[-1])
#Forecast
Exrate_forecast <- forecast(Exrate.arima, h=60, level=c(80,95))     
plot(Exrate_forecast)
#ARDL
Infrate <- read.table("Inflation.csv", header = TRUE, sep=",")
class(Infrate)
Infrate.ts <- ts(Infrate$T10YIE, start = c(2012,1), freq=12)
plot(Infrate.ts)

Exrate.d <- diff(Exrate.ts)
plot(cbind(Exrate.d,Infrate.ts), main = "Change in Inflation")

model1<- lm(Exrate$DEXUSEU ~ Infrate$T10YIE)
summary(model1)

install.packages("dynlm")
library(dynlm)

ardl1 <- dynlm(Exrate.d ~ L(Exrate.d, 1) + Infrate.ts + L(Infrate.ts, 1))
summary(ardl1)
AIC(ardl1)

ardl2 <- dynlm(Exrate.d ~ L(Exrate.d, 1) + Infrate.ts + L(Infrate.ts, 1)+ L(Infrate.ts, 2))
summary(ardl2)
AIC(ardl2)

ardl3 <- dynlm(Exrate.d ~ L(Exrate.d, 1) + Infrate.ts + L(Infrate.ts, 1)+ L(Infrate.ts, 2)+ L(Infrate.ts, 3))
summary(ardl3)
AIC(ardl3)

ardl4 <- dynlm(Exrate.d ~ L(Exrate.d, 1) + Infrate.ts + L(Infrate.ts, 1)+ L(Infrate.ts, 2)+ L(Infrate.ts, 3)+ L(Infrate.ts, 4))
summary(ardl4)
AIC(ardl4)

acf(ardl2$residuals, main = "Residuals ACF")
pacf(ardl2$residuals, main = "Residuals PACF")

install.packages("dLagM")
library(dLagM)

model.ardl2 <- ardlDlm(x=as.numeric(Exrate.ts[1:121]), y=as.numeric(Infrate.ts), p=1, q=2) 
#independent variable (p), #dependent variable (q))
summary(model.ardl2)
#produce forecast 5 years ahead
Gpred <- predict(HoltWinters(Exrate.ts, beta=FALSE), n.ahead=5*12, lty=1:2)
#infrate - independent, exrate - dependent
model.ardl <- ardlDlm(x=as.numeric(Infrate.ts[1:120]), y=as.numeric(Exrate.d), p=1, q=1) 
summary(model.ardl)
model.ardl.forecast <- forecast(model=model.ardl, x=as.numeric(Gpred), h=60, interval = FALSE)
Exrate.d.pred <- ts(model.ardl.forecast$forecasts, start=c(2022,1), freq=12)
ts.plot(Exrate.d,Exrate.d.pred,lty=1:2)












