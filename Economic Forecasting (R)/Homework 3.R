rm(list=ls())
#Load the data
GDP <- read.table("realgdp.csv", header = TRUE, sep=",")
class(GDP)
GDP.ts <- ts(GDP$ND000334Q, start = c(2002, 1), freq = 4)

#1
plot(GDP.ts, xlab="Years", ylab= "US Real GDP (billions)", main = "Real GDP over the years")
#3 (AR)
acf(GDP.ts)
pacf(GDP.ts)
#4 
GDP1.ar<- arima(GDP.ts, order = c(1,0,0), method = "ML") 
GDP2.ar<- arima(GDP.ts, order = c(2,0,0), method = "ML") 
GDP3.ar<- arima(GDP.ts, order = c(3,0,0), method = "ML") 
GDP4.ar<- arima(GDP.ts, order = c(4,0,0), method = "ML") 
GDP5.ar<- arima(GDP.ts, order = c(5,0,0), method = "ML") 
GDP6.ar<- arima(GDP.ts, order = c(6,0,0), method = "ML") 
GDP1.ar
GDP2.ar
GDP3.ar
GDP4.ar
GDP5.ar
GDP6.ar
#5
acf(GDP1.ar$res[-1])
acf(GDP2.ar$res[-1])
acf(GDP3.ar$res[-1])
acf(GDP4.ar$res[-1])
acf(GDP5.ar$res[-1])
acf(GDP6.ar$res[-1])
#6 (MA)
GDP1.ma <- arima(GDP.ts, order = c(0,0,1), method = "ML") 
GDP2.ma <- arima(GDP.ts, order = c(0,0,2), method = "ML") 
GDP3.ma <- arima(GDP.ts, order = c(0,0,3), method = "ML") 
GDP4.ma <- arima(GDP.ts, order = c(0,0,4), method = "ML") 
GDP5.ma <- arima(GDP.ts, order = c(0,0,5), method = "ML") 
GDP6.ma <- arima(GDP.ts, order = c(0,0,6), method = "ML") 
GDP7.ma <- arima(GDP.ts, order = c(0,0,7), method = "ML") 
GDP8.ma <- arima(GDP.ts, order = c(0,0,8), method = "ML") 
GDP9.ma <- arima(GDP.ts, order = c(0,0,9), method = "ML") 
GDP10.ma <- arima(GDP.ts, order = c(0,0,10), method = "ML") 
GDP11.ma <- arima(GDP.ts, order = c(0,0,11), method = "ML") 
GDP12.ma <- arima(GDP.ts, order = c(0,0,12), method = "ML") 
GDP1.ma
GDP2.ma
GDP3.ma
GDP4.ma
GDP5.ma
GDP6.ma
GDP7.ma
GDP8.ma
GDP9.ma
GDP10.ma
GDP11.ma
GDP12.ma
#7
acf(GDP1.ma$res[-1])
acf(GDP2.ma$res[-1])
acf(GDP3.ma$res[-1])
acf(GDP4.ma$res[-1])
acf(GDP5.ma$res[-1])
acf(GDP6.ma$res[-1])
acf(GDP7.ma$res[-1])
acf(GDP8.ma$res[-1])
acf(GDP9.ma$res[-1])
acf(GDP10.ma$res[-1])
acf(GDP11.ma$res[-1])
acf(GDP12.ma$res[-1])
#8 (ARMA) 
auto.arima(GDP.ts, d=FALSE, trace=TRUE, stepwise=FALSE, seasonal=FALSE, ic="aic", approximation=FALSE, method = "ML")
GDP.arma <- arima(GDP.ts, order = c(5,0,0), method="ML")
GDP.arma
#9
acf(GDP.arma$res[-1]) 
#10 (Non-Seasonal ARIMA)
auto.arima(GDP.ts, trace=TRUE, stepwise=FALSE, seasonal=FALSE, ic="aic", approximation=FALSE, method = "ML")
GDP.arima <- arima(GDP.ts, order = c(4,1,0), method="ML")
GDP.arima
#11
acf(GDP.arima$res[-1])
#12 (Seasonal ARIMA)
plot(diff(GDP.ts, lag=4), lty=1)
title("Real GDP over the years")
#13
GDP1 <- arima(GDP.ts, order=c(1,0,0), seas=list(order = c(2,1,0), 4))
GDP1
#14
GDPa <- auto.arima(GDP.ts,trace=TRUE, stepwise=FALSE, ic="aic", approximation = FALSE, method="ML")
GDPa
#15
acf(GDPa$res[-1])
#16
install.packages("forecast")
library(forecast)
GDP_forecast <- forecast(GDPa, h=12, level=c(95))     
plot(GDP_forecast)
#17
data("AirPassengers")
AP<-AirPassengers
plot(diff(log(AP)))
title("AirPassengers")


