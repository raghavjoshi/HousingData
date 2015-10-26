#setup
setwd("~/Desktop/mini_proj")
rm(list=ls())
data <- read.csv("Housing Price.csv")
data <- na.omit(data)

str(data)
summary(data)

new_data <- read.csv("new_data.csv")
str(new_data)
summary(new_data)
attach(new_data)

new_data$YR_BLT <- as.factor(new_data$YR_BLT)
hist(PRICE)
pairs(~PRICE + ZIP + year + quarter + bathrooms + bedrooms + YR_BLT + SQFT)

log_price <- log(PRICE)
hist(log_price)
summary(log_price)

new_data$ZIP <- as.factor(new_data$ZIP)
new_data$year<- as.factor(new_data$year)

fit1<-lm(log_price~ZIP+year+quarter+bathrooms+bedrooms+YR_BLT+SQFT,data=new_data)
summary(fit1)
fit2<-lm(log_price~ZIP+year+quarter+bathrooms+bedrooms+SQFT,data=new_data)
summary(fit2)
anova(fit1,fit2)
library(MASS)
stepAIC(fit2)
fit3<-lm(log_price~ZIP+year+bathrooms+bedrooms+SQFT,data=new_data)
summary(fit3)
anova(fit2,fit3)

fit3$coefficients