rm(list=ls())
#Load the data
Phillips <- read.table("phillips.csv", header = TRUE, sep=",")
class(Phillips)
ud.ts <- ts(Phillips$u.d, start = c(1987, 2), freq = 4)
inf.ts <- ts(Phillips$inf, start = c(1987, 2), freq = 4)
UnemplomentRate <- ud.ts
InflationRate <- inf.ts
#1
plot(cbind(UnemplomentRate,InflationRate), xlab = "Years", main = "Quarterly change in Unemployment rate and Quarterly Inflation rate in Australia")
#2
install.packages("dLagM")
library(dLagM)

ardl1 <- ardlDlm(x=as.numeric(ud.ts[1:90]), y=as.numeric(inf.ts), p=1, q=1) 
ardl2 <- ardlDlm(x=as.numeric(ud.ts[1:90]), y=as.numeric(inf.ts), p=1, q=2) 
ardl3 <- ardlDlm(x=as.numeric(ud.ts[1:90]), y=as.numeric(inf.ts), p=1, q=3) 
ardl4 <- ardlDlm(x=as.numeric(ud.ts[1:90]), y=as.numeric(inf.ts), p=1, q=4) 
ardl5 <- ardlDlm(x=as.numeric(ud.ts[1:90]), y=as.numeric(inf.ts), p=1, q=5) 

summary(ardl1)  
summary(ardl2) 
summary(ardl3)  
summary(ardl4)  
summary(ardl5)

BIC(ardl1)
BIC(ardl2)
BIC(ardl3)
BIC(ardl4)
BIC(ardl5)
#ACF 
acf(resid(ardl4))
#3 - Holt Winters
#Additive Decomposition
ud.decom.add <- decompose (ud.ts, "add")
udadjust.add <- ud.ts-ud.decom.add$seasonal
plot(udadjust.add, ylab="Australian Unemployment Rate", xlab="Years", main="Australian Unemployment Rate: 1987 Q2 - 
     2009 Q3")
ud.hw <- HoltWinters(ud.ts, seasonal = "add")
plot(ud.hw, xlab="Years", ylab="Australian Unemployment Rate", main ="Australian Unemployment Rate: 1987 Q2 - 2009 Q3")
ud.predict <- predict(ud.hw, n.ahead = 12)
ts.plot(ud.ts, ud.predict, lty = 1:2, xlab="Years", ylab="Australian Unemployment Rate", main ="Australian Unemployment Rate: 1987 Q2-2009 Q3")
#4 
Gpred <- predict(HoltWinters(ud.ts, beta=FALSE), n.ahead=12)
ardl4.forecast <- forecast(model=ardl4, x=as.numeric(Gpred), h=12, interval = FALSE)
inf.d.pred <- ts(ardl4.forecast$forecasts, start=c(2009,4), freq=4)
inf.d <- diff(inf.ts)
ts.plot(inf.d,inf.d.pred,lty=1:2, xlab = "Years", ylab="Australian Inflation Rate", main ="Australian Inflation Rate: 1987 Q2-2009 Q3")
#5 - Multivariate Models
library(tseries)
adf.test(ud.ts)
adf.test(inf.ts)
#6
po.test(cbind(ud.ts, inf.ts))
#7
library(fpp2)
install.packages("vars")
library(vars)
VARselect(cbind(ud.ts,inf.ts), lag.max=8, type = "const")$selection
VARselect(cbind(ud.ts,inf.ts), lag.max=8, type = "const")
#8 
var1<- VAR(cbind(ud.ts,inf.ts), p=2, type="const")
summary(var1)
acf(var1$varresult$ud.ts$residuals)
acf(var1$varresult$inf.ts$residuals)
serial.test(var1, lags.pt=10, type="PT.asymptotic")

detach("package:dLagM", unload=TRUE)
library(forecast)
#9
var1fcast <- predict(var1, n.ahead = 15, ci = 0.95)
plot(var1fcast)
#10 - ARCH/GARCH
library(MASS)
data("SP500")
SP <- SP500
#10
SP.arima <- arima(SP, order=c(2,0,1))
summary(SP.arima)

SP.res <- resid(SP.arima)
acf(SP.res) 
acf(SP.res^2)

library(tseries)
SP.garch <- garch(SP.res, trace = F) 
summary(SP.garch) 

SP.garch.res <- resid(SP.garch)[-1]
acf(SP.garch.res)
acf(SP.garch.res^2)

