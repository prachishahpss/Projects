rm(list=ls())

#Question 1
x<-(((12000*45)-10)/5000)*(2^3)

#Question 2
myname<- c("Prachi","Shah")

#Question 3
UnemploymentRate <- read.table("UnemploymentRate.csv", header = TRUE, sep=",")
TotalVehicleSales <- read.table("TotalVehicleSales.csv", header = TRUE, sep=",")
ur<-UnemploymentRate
tvs<-TotalVehicleSales

#Question 4
summary(ur)

#Question 5
sapply(ur[2],sd)

#Question 6
summary(tvs)

#Question 7
sapply(tvs[2:2],sd)

#Question 8
plot(tvs$TOTALNSA, ur$UNRATENSA,xlab="Total Vehicle Sales",ylab="Unemployment Rate")
title("Unemployment Rate as compared to Total Vehicle Sales")

#Question 9
model1<- lm(ur$UNRATENSA ~ tvs$TOTALNSA)
summary(model1)

#Question 10
abline(model1)

#Question 11
cor(ur$UNRATENSA,tvs$TOTALNSA)

#Question 12

#Question 13
ConsumerPriceIndex <- read.table("ConsumerPriceIndex.csv", header = TRUE, sep=",")
cpi<-ConsumerPriceIndex
model2 <- lm(ur$UNRATENSA ~ tvs$TOTALNSA + cpi$CPALTT01USM657N)
summary(model2)

#Question 14
class(ur)
ur.ts <- ts(ur$UNRATENSA, start = c(2000, 1), freq = 12)
plot(ur.ts, xlab="Years", ylab="National Unemployment Rate(%)")
title("Unemployment Rate: Jan 2000 - Aug 2020")

class(tvs)
tvs.ts <- ts(tvs$TOTALNSA, start = c(2000, 1), freq = 12)
plot(tvs.ts, xlab="Years",ylab="Total Vehicle Sales")
title("Total Vehicle Sales: Jan 2000 - Aug 2020")

#Question 15
ur.annual.ts <- aggregate(ur.ts)/12
plot(ur.annual.ts, xlab="Years", ylab="National Unemployment Rate(%)")
title("Unemployment Rate: Jan 2000 - Aug 2020")

tvs.annual.ts <- aggregate(tvs.ts)/12
plot(tvs.annual.ts, xlab="Years",ylab="Total Vehicle Sales")
title("Total Vehicle Sales: Jan 2000 - Aug 2020")

UnemploymentRate.ts <- ur.ts
TotalVehicleSales.ts <- tvs.ts

#Question 16
boxplot(UnemploymentRate.ts~cycle(UnemploymentRate.ts))
title("Unemployment Rate")
boxplot(TotalVehicleSales.ts~cycle(TotalVehicleSales.ts))
title("Total Vehicle Sales")

#Question 17
plot(decompose(ur.ts))
plot(decompose(tvs.ts))
