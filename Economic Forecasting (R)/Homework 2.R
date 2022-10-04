rm(list=ls())
#Download the files, load the series into R and format them as time series:
#1
SportingGoods <- read.table("SportingGoods.csv", header = TRUE, sep=",")
class(SportingGoods)
SportingGoods.ts <- ts(SportingGoods$MRTSSM45111USN, start = c(1992, 1), freq = 12)
#2
UNRATENSA <- read.table("UNRATENSA.csv", header = TRUE, sep=",")
class(UNRATENSA)
UNRATENSA.ts <- ts(UNRATENSA$UNRATENSA, start = c(2000, 1), freq = 12)
#3
UNRATE <- read.table("UNRATE.csv", header = TRUE, sep=",")
class(UNRATE)
UNRATE.ts <- ts(UNRATE$UNRATE, start = c(2000, 1), freq = 12)
#4
Inventory <- read.table("inventory.csv", header = TRUE, sep=",")
class(Inventory)
Inventory.ts <- ts(Inventory$MEDDAYONMAR39153, start = c(2016, 1), freq = 12)
#5
Prices <- read.table("prices.csv", header = TRUE, sep=",")
class(Prices)
Prices.ts <- ts(Prices$MEDLISPRI39153, start = c(2016, 1), freq = 12)

# Autocorrelation, Stationarity, and Leading Variables
#Question 1. Using the series from sportinggoods.csv:
#a.
plot(SportingGoods.ts, xlab="Years", ylab=" Retail Sales (millions)")
title("Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
#e.
acf(SportingGoods.ts, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)" )
#f.
acf(SportingGoods.ts, lag.max = 90, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
#g.
SportingGoods.ran <- decompose(SportingGoods.ts)$random
SportingGoods.ran.ts <- window(SportingGoods.ran, start = c(1992,3), end = c(2020,7))
plot(SportingGoods.ran)
plot(SportingGoods.ran, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
#j.
acf(SportingGoods.ran, na.action = na.pass, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")

#Question 2.Using the series from UNRATENSA.csv
#a.
layout(1:2)
#multiplicative decomposition
UNRATENSA.decom <- decompose(UNRATENSA.ts, "multiplicative")
UNRATENSAadjust.mult <- UNRATENSA.ts/UNRATENSA.decom$seasonal
plot(UNRATENSAadjust.mult, ylab="National Unemployment Rate", xlab="Years", main="Employment Rate: Jan 2000 - 
     Aug 2020 (Mult)")
plot(UNRATENSA.ts, ylab="National Unemployment Rate", xlab="Years", main="Employment Rate: Jan 2000 - 
     Aug 2020")

#additive decomposition
UNRATENSA.decom.add <- decompose (UNRATENSA.ts, "add")
UNRATENSAadjust.add <- UNRATENSA.ts-UNRATENSA.decom.add$seasonal
plot(UNRATENSAadjust.add, ylab="National Unemployment Rate", xlab="Years", main="Employment Rate: Jan 2000 - 
     Aug 2020 (Add)")
plot(UNRATENSA.ts, ylab="National Unemployment Rate", xlab="Years", main="Employment Rate: Jan 2000 - 
     Aug 2020")

#b
plot(UNRATE.ts, ylab="Unemployment Rate (%)",xlab = "Years", main = "National Unemployment Rate: Jan 2000 - Aug 2020")

#Question 3:Use the series from inventory.csv and prices.csv 
#b
plot(Inventory.ts, ylab="Median Days",xlab = "Years", main = "Housing Inventory: 
Median Days on Market")
plot(Prices.ts, ylab="Median Price ($)",xlab = "Years", main = "Housing Inventory: Median Listing Price")
#c
#Cross-correlation and autocorrelation
layout(1:1)
ccf(Inventory.ts,Prices.ts) 
#Question 4.Using the series from sportinggoods.csv:
#a.
SportingGoods.hw <- HoltWinters(SportingGoods.ts, beta = FALSE, gamma = FALSE)
SportingGoods.hw
plot(SportingGoods.hw, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
SportingGoods.hw$SSE

#b.
SportingGoods.hw1 <- HoltWinters(SportingGoods.ts, alpha = 0.2, beta = FALSE, gamma = FALSE)
SportingGoods.hw1
plot(SportingGoods.hw1, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
SportingGoods.hw1$SSE

SportingGoods.hw2 <- HoltWinters(SportingGoods.ts, alpha = 0.05, beta = FALSE, gamma = FALSE)
SportingGoods.hw2
plot(SportingGoods.hw2, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
SportingGoods.hw2$SSE

SportingGoods.hw3 <- HoltWinters(SportingGoods.ts, beta = FALSE, gamma = FALSE)
SportingGoods.hw3
plot(SportingGoods.hw3, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
SportingGoods.hw3$SSE

#c.
SportingGoods.hw <- HoltWinters(SportingGoods.ts, seasonal = "mult")
plot(SportingGoods.hw, xlab="Years", ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")
SportingGoods.predict <- predict(SportingGoods.hw, n.ahead = 5 * 12)
ts.plot(SportingGoods.ts, SportingGoods.predict, lty = 1:2, ylab=" Retail Sales (millions)", main ="Retail Sales: Sporting Goods Stores (Jan 1992 - Aug 2020)")

SportingGoods.hw$alpha
SportingGoods.hw$beta
SportingGoods.hw$gamma
