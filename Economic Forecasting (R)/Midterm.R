rm(list=ls())
#Midterm Exam
#1
MonthlyEmployment18 <- read.table("PAYNSAdec18.csv", header = TRUE, sep=",")
class(MonthlyEmployment18)
MonthlyEmployment18.ts <- ts(MonthlyEmployment18$PAYNSA, start = c(2010, 1), freq = 12)
#2
MonthlyEmployment20 <- read.table("PAYNSAsep20.csv", header = TRUE, sep=",")
class(MonthlyEmployment20)
MonthlyEmployment20.ts <- ts(MonthlyEmployment20$PAYNSA, start = c(2010, 1), freq = 12)
#Using only the series PAYNSA from PAYNSAdec18.csv:
#1
plot(MonthlyEmployment18.ts, xlab="Years", ylab= "Monthly Employment (thousands)", main = "Employment over the years (Jan 2010 - Dec 2018)")
#3
boxplot(MonthlyEmployment18.ts~cycle(MonthlyEmployment18.ts))
title("Employment over the years (Jan 2010 - Dec 2018)")
#4
acf(MonthlyEmployment18.ts)
#5
MonthlyEmployment18.decom.add <- decompose (MonthlyEmployment18.ts, "add")
MonthlyEmployment18.add <- MonthlyEmployment18.ts-MonthlyEmployment18.decom.add$seasonal
plot(MonthlyEmployment18.add, ylab="Monthly Employment (thousands)", xlab="Years", main="Employment over the years (Jan 2010 - Dec 2018)")
#6a
MonthlyEmployment18.hw <- HoltWinters(MonthlyEmployment18.ts)
MonthlyEmployment18.hw
plot(MonthlyEmployment18.hw, xlab="Years", ylab="Monthly Employment (thousands)", main ="Employment over the years (Jan 2010 - Dec 2018)")
MonthlyEmployment18.hw$SSE
#6b
MonthlyEmployment18.hw$alpha
MonthlyEmployment18.hw$beta
MonthlyEmployment18.hw$gamma
#6c
MonthlyEmployment18.predict <- predict(MonthlyEmployment18.hw, n.ahead = 2*12)
ts.plot(MonthlyEmployment18.predict, MonthlyEmployment20.ts, gpars = list(lty= c(2,1)), ylab= "Monthly Employment (thousands)", main = "Employment over the years (Jan 2010 - Dec 2018)")
