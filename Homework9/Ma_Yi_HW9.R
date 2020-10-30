library(foreign)
library(lmtest)
library(systemfit)
library(sandwich)
library(survival)
install.packages("AER")
library(AER)
suppressMessages(library(systemfit))

# setting working directory
setwd('C:/Users/think/Documents/R/Homework9')
# get the data
mydata<- read.dta("bwght.dta")

#part c
mydata.ols<- lm(log(bwght)~male+parity+ log(faminc)+packs, data=mydata)
print(summary (mydata.ols))
reg.iv<-ivreg(log(bwght)~male+parity+ log(faminc)+packs | .- packs+cigprice, data=mydata)
print(summary(reg.iv))

#part d
red.iv<- lm(packs~male+parity+log(faminc)+cigprice, data=mydata)
print(summary(red.iv))