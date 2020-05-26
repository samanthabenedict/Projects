#Set working directory
setwd()

#########  HUNTER COLLEGE: DEPARTMENT OF MATHEMATICS & STATISTICS 
#########  Bayesian Statistics
#########  Fall 2019
#########  Bayesian Regression Project
#########  Samantha Benedict

#Libraries
library(LearnBayes)
library(lattice)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(plyr)
library(readxl)
library(tidyverse)
library(read.csv)
library(ggrepel)
library(data.table)
library(baytrends)
library(lmtest)
library(qualityTools)
library(alr3)
library(gmodels)
library(stats)
library(lmtest)

#Import Data
df <- read_excel("weatherHistory.xlsx")
summary(df)

#Omit missing values
df <- na.omit(df)  
summary(df)
attach(df)

#OLS Model
mlr.model <- lm(Humidity~Temperature+Wind_Speed,x=TRUE,y=TRUE) 
summary(mlr.model)

#Scatterplots
par(mfrow=c(1,2))
plot(Humidity~Temperature, xlab="Temperature", main="Humidity Vs. Temperature", pch=20, col="darkred")
plot(Humidity~Wind_Speed, xlab="Wind Speed", main="Humidity Vs. Wind Speed", col="darkgreen", pch=20)

anova(mlr.model)
vcov(mlr.model)

#OLS Model Diagnostics
par(mfrow=c(1,2))
plot(mlr.model$residuals ~ mlr.model$fitted.values, main="Residuals vs. Fitted", xlab="Fitted Values", ylab="Residuals",pch= 20, col= "darkblue")
abline(h=mean(mlr.model$residuals), col= "sienna")
plot(mlr.model,pch=20, which=c(2), col= "darkblue")

#Bayesian Regression
theta.sample=blinreg(mlr.model$y,mlr.model$x,5000)

S=sum(mlr.model$residual^2)
shape=mlr.model$df.residual/2; rate=S/2
sigma2=rigamma(1,shape,rate)

MSE = sum(mlr.model$residuals^2)/mlr.model$df.residual
vbeta=vcov(mlr.model)/MSE
beta=rmnorm(1,mean=mlr.model$coef,varcov=vbeta*sigma2)

#Posterior Distributions
par(mfrow=c(2,2))
hist(theta.sample$beta[,1],main="Intercept",
     xlab=expression(beta[0]), col = "darkred", border="darkgrey")
hist(theta.sample$beta[,2],main="Temperature",
     xlab=expression(beta[1]),col = "darkblue", border="darkgrey")
hist(theta.sample$beta[,3],main="Wind Speed",
     xlab=expression(beta[2]), col = "darkgreen", border="darkgrey")
hist(theta.sample$sigma,main="ERROR SD",
     xlab=expression(sigma), col = "darkgrey")

apply(theta.sample$beta,2,quantile,c(.05,.5,.95))

quantile(theta.sample$sigma,c(.05,.5,.95))

S=readline(prompt="Type  <Return>   to continue : ")


summary(Temperature)
summary(Wind_Speed)

#Estimation
cov1=c(1,11,2)
cov2=c(1,11,9)
cov3=c(1,11,15)
X1=rbind(cov1,cov2,cov3)
mean.draws=blinregexpected(X1,theta.sample)
c.labels=c("A","B","C")

par(mfrow=c(1,3))
for (j in 1:3)
  hist(mean.draws[,j],
       main=paste("Covariate set",c.labels[j]),xlab="Humidity", col="darkgreen", border="darkgrey")

#Prediction
X1=rbind(cov1,cov2,cov3)
pred.draws=blinregpred(X1,theta.sample)
c.labels=c("A","B","C","D")
par(mfrow=c(1,3))
for (j in 1:3)
  hist(pred.draws[,j],
       main=paste("Covariate set",c.labels[j]),xlab="Humidity", col="darkred", border="darkgrey")

#Diagnsotics type 1
pred.draws=blinregpred(mlr.model$x,theta.sample)
pred.sum=apply(pred.draws,2,quantile,c(.05,.95))
par(mfrow=c(1,1))
ind=1:length(Humidity)
matplot(rbind(ind,ind),pred.sum,type="l",lty=1,col=1,
          xlab="INDEX",ylab="Humidity")

points(ind,Humidity,pch=19)
out=(Humidity>pred.sum[2,])
text(ind[out], Humidity[out], label=Temperature[out], pos = 4, col="red")

#Diagnsotics type 2
prob.out=bayesresiduals(mlr.model,theta.sample,2)
par(mfrow=c(1,1))
plot(Wind_Speed,prob.out)
out = (prob.out > 0.35)
text(Wind_Speed[out], prob.out[out], label=Temperature[out], pos = 4, col="red")
