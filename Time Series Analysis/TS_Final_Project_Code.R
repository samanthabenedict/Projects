#Set working directory
setwd("~/Desktop/Time Series/Project/data")

#Packages
library(ggplot2)
library(readxl)
library(tsbox)
library(TSA)
library(timeSeries)
library(zoo)
library(xts)
library(imputeTS)
library(aTSA)
library(forecast)
library(tseries)
library(sarima)


##Import Data from before
#df1 <- read_excel("water_quality_peconic.xlsx")
#df1 <- df1[-c(2:8,10:30)]
#df1 <- df1[-c(1:140, 1236:2758),]

#Rename Column
#names(df1)[2]="WT"

#df1$datetime = as.Date(df$datetime)
#time.series3 = ts(df1$WT, start =1 , end = 1095)
#time.series3

#Impute missing values
#time.series4 = na_kalman(time.series3)
#time.series4

#no_na = as.data.frame(time.series3)
#write.csv(time.series4, "wt_na.csv")

##Import Data
test = read_excel("water_quality_peconic.xlsx")
test = test[-c(2:17,19:30)]
test = test[-c(1:1235, 1267:2758),]
names(test)[2]="DO"

##Convert test data to time series
test$datetime = as.Date(test$datetime)
diss.ox.test = ts(test$DO, start = c(2016, 1) , frequency = 365)
diss.ox.test

##Import non Missing value data
df <- read_excel("2013_2015.xls")

#Convert data to time series
df$datetime = as.Date(df$datetime)
diss.ox = ts(df$DO, start = c(2013, 1) , frequency = 365)
diss.ox

wat.temp = ts(df$WT, start = c(2013, 1) , frequency = 365)
wat.temp

######## INTRODUCTION ############

#Extract trend, seasonality, and error and plot on DO
decomposedRes <- decompose(diss.ox, type="add") 
plot (decomposedRes, col="orange", xaxt = "n") 
axis(1, 2013:2016, tick=FALSE)
stlRes <- stl(diss.ox, s.window = "periodic")

#Extract trend, seasonality, and error and plot on DO
decomposedRes <- decompose(wat.temp, type="add") 
plot (decomposedRes, col="darkblue", xaxt="n") 
axis(1, 2013:2016, tick=FALSE)
stlRes <- stl(wat.temp, s.window = "periodic")

#Plot time series Dissolved Oxygen vs. Water Temp
plot(diss.ox,ylab="Average Daily Dissolved Oxygen (mg/L)", xlab='Year', type='l', pch=20, col = "darkorange", main="Water Temperature vs. Dissolved Oxygen 2013-2015", xaxt="n")
par(new=TRUE)
plot(wat.temp, axes=FALSE, ylab=NULL, xlab='Year', type='l', pch=20, col="darkblue", labels = FALSE, lty = 2)
axis(side=4)
axis(1, 2013:2016, tick=FALSE)

###LAG PLOTS###
par(mfrow=c(2,1))

#Lag 1 Plot
plot(y=diss.ox, x=zlag(diss.ox,1 ), ylab='Dissolved Oxygen', xlab='Previous Year Dissolved Oxygen',pch=20, main="Lag 1 Plot")
#Lag 2 Plot
plot(y=diss.ox, x=zlag(diss.ox,2 ), ylab='Dissolved Oxygen', xlab='Previous Year Dissolved Oxygen',pch=20, main="Lag 2 Plot")
#Lag 3 Plot
plot(y=diss.ox, x=zlag(diss.ox,3 ), ylab='Dissolved Oxygen', xlab='Previous Year Dissolved Oxygen',pch=20)

#ACF and PACF of unstransformed time series
par(mfrow=c(2,1))
acf(diss.ox, main="ACF of Daily Dissolved Oxygen")
pacf(diss.ox, main="PACF of Daily Dissolved Oxygen")

#Augmented Dickey FUller to test if stationary
adf.test(diss.ox) #P-value >.05 not stationary


par(mfrow=c(1,1))


### NUMERICAL TRANSFORMATIONS ###

#before differenced
plot(diss.ox, type="l", ylab="Dissolved Oxygen")
acf(diss.ox, main="ACF of Daily Dissolved Oxygen", lag.max=30)
pacf(diss.ox, main="PACF of Daily Dissolved Oxygen")


#1st differenced
diff1= diff(diss.ox)
plot(diff1, type="l")
acf(diff1)
pacf(diff1)
#Augmented Dickey FUller to test if stationary
adf.test(diff1) #P-value >.05 not stationary

#log differenced
diff.log= diff(log(diss.ox))
plot(diff.log, type="l", ylab="Log Difference")
acf(diff.log)
pacf(diff.log)
adf.test(diff.log) #P-value >.05 not stationary

#log
log.ox= log(diss.ox)
plot(log.ox, type="l")
acf(log.ox)
adf.test(log.ox) #P-value >.05 not stationary

#sqrt
sqrt.ox = sqrt(diss.ox)
plot(sqrt.ox)
acf(sqrt.ox)
pacf(sqrt.ox)

#sqrt differenced
diff.sqrt= diff(sqrt(diss.ox))
plot(diff.sqrt, type="l", ylab="Sqrt Difference")
acf(diff.sqrt)
pacf(diff.sqrt)

par(mfrow=c(3,1))
plot(diff.log, type="l", ylab="Log Difference",main="Time Series of Log Difference Transformation", xaxt="n")
axis(1, 2013:2016, tick=FALSE)
acf(diff.log, main="ACF of Log Difference Transformation")
axis(1, 1:30, tick=FALSE)
pacf(diff.log,main="PACF of Log Difference Transformation")

plot(diff.sqrt, type="l", ylab="Sqrt Difference", main="Time Series of Sqrt Difference Transformation", xaxt="n")
axis(1, 2013:2016, tick=FALSE)
acf(diff.sqrt, main="ACF of Sqrt Difference Transformation", lag.max=30)
pacf(diff.sqrt, main="PACF of Sqrt Difference Transformation")

####### Model Selection #######
arima.sqrt$x <- sqrt.ox

par(mfrow=c(1,1))
#ARIMA 313 no transformation
arima313 = arima(diss.ox, order=c(3,1,3))
arima313

acf(residuals(arima313))
pacf(residuals(arima313))
checkresiduals(arima313)
stdres = rstandard(arima313)
qqnorm(stdres)
qqline(stdres)


#ARIMA with seasonality
par(mfrow=c(1,1))

seas.mod = Arima(sqrt.ox,  order = c(3,1,2), seasonal = list(order=c(1,1,1), period=3))
seas.mod
plot(residuals(seas.mod), main="(3,1,3) Sqrt Model Residuals")
acf(residuals(seas.mod), main="(3,1,3) Sqrt Model ACF")
pacf(residuals(seas.mod), main="(3,1,3) Sqrt Model PACF")
stdres = rstandard(seas.mod)
qqnorm(stdres)
qqline(stdres)
checkresiduals(seas.mod)

#ARIMA 212 sqrt transformation
arima.sqrt212 = arima(sqrt.ox, order=c(2,1,2))
arima.sqrt212

acf(residuals(arima.sqrt212))
pacf(residuals(arima.sqrt212))
checkresiduals(arima.sqrt212)
stdres = rstandard(arima.sqrt212)
qqnorm(stdres)
qqline(stdres)

#ARIMA 312 sqrt transformation
arima.sqrt312 = arima(sqrt.ox, order=c(3,1,2))
arima.sqrt312

acf(residuals(arima.sqrt312))
pacf(residuals(arima.sqrt312))
checkresiduals(arima.sqrt312)
stdres = rstandard(arima.sqrt312)
qqnorm(stdres)
qqline(stdres)

#ARIMA 013 sqrt transformation
arima.sqrt013 = arima(sqrt.ox, order=c(0,1,3))
arima.sqrt013

acf(residuals(arima.sqrt013))
pacf(residuals(arima.sqrt013))
checkresiduals(arima.sqrt013)
stdres = rstandard(arima.sqrt013)
qqnorm(stdres)
qqline(stdres)

#ARIMA 112 log transformation
arima.log = arima(log.ox, order=c(1,1,2))
arima.log

acf(residuals(arima.log))
pacf(residuals(arima.log))
checkresiduals(arima.log)
stdres = rstandard(arima.log)
qqnorm(stdres)
qqline(stdres)

#ARIMA 313 log transformation
arima.log313 = arima(log.ox, order=c(3,1,3))
arima.log313

acf(residuals(arima.log313))
pacf(residuals(arima.log313))
checkresiduals(arima.log313)
stdres = rstandard(arima.log313)
qqnorm(stdres)
qqline(stdres)

#ARIMA 212 log transformation
arima.log212 = arima(log.ox, order=c(2,1,2))
arima.log212

acf(residuals(arima.log212))
pacf(residuals(arima.log212))
checkresiduals(arima.log212)
stdres = rstandard(arima.log212)
qqnorm(stdres)
qqline(stdres)

#ARIMA 012 log transformation
arima.log012 = arima(log.ox, order=c(0,1,2))
arima.log012

acf(residuals(arima.log012))
pacf(residuals(arima.log012))
checkresiduals(arima.log012)
stdres = rstandard(arima.log012)
qqnorm(stdres)
qqline(stdres)


#Auto arima log diff
log.ar = auto.arima(diff.log, seasonal=FALSE);log.ar
tsdisplay(residuals(log.ar), col= "darkgreen", lag.max=45, main='(2,1,1) Log Model Residuals') # AIC=-15.88
checkresiduals(log.ar$residuals)
summary(log.ar)

#Auto arima diff 1
diff.ar = auto.arima(diff1, seasonal=FALSE);log.ar
tsdisplay(residuals(diff.ar), col= "darkgreen", lag.max=45, main='(2,1,1) Log Model Residuals') # AIC=-15.88
checkresiduals(diff.ar$residuals)
summary(diff.ar)

#Auto arima diff 2
diff2.ar = auto.arima(diff2, seasonal=FALSE);log.ar
tsdisplay(residuals(diff2.ar), col= "darkgreen", lag.max=45, main='(2,1,1) Log Model Residuals') # AIC=-15.88
checkresiduals(diff2.ar$residuals)
summary(diff2.ar)

#### WINNING MODEL
#ARIMA 313 sqrt transformation 
arima.sqrt = arima(sqrt.ox, order=c(3,1,3))
arima.sqrt

par(mfrow=c(2,1))
acf(residuals(arima.sqrt), main="(3,1,3) Sqrt Model ACF")
pacf(residuals(arima.sqrt), main="(3,1,3) Sqrt Model PACF")

plot(residuals(arima.sqrt), main="(3,1,3) Sqrt Model Residuals", ylab="Residuals", xaxt="n")
axis(1, 2013:2016, tick=FALSE)
stdres = rstandard(arima.sqrt)
qqnorm(stdres)
qqline(stdres)
checkresiduals(arima.sqrt)

## non parsimonious model
month = season(diss.ox)
lin_model = lm(diss.ox~month-1)
summary(lin_model)
plot(lin_model$residuals~df$DO, pch=19)
abline(h=2*1.289); abline(h=-2*1.289)
acf(lin_model$resdiuals)

##Cosine Trend Model - bad acf and pacf
har=harmonic(diss.ox,1)
sin_model=lm(diss.ox ~har)
summary(sin_model)
plot(sin_model$residuals~sin_model$fitted.values, pch=19)
tsdisplay(residuals(sin_model), col= "darkgreen", lag.max=45)
checkresiduals(diff2.ar$residuals)
acf(sin_model$residuals)
pacf(sin_model$residuals)

plot(sin_model)


#De-seasonalized model
ts.stl <- stl(diss.ox,"periodic")  # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
plot(diss.ox, type="l")  # original series
plot(ts.sa, type="l")  # seasonal adjusted

acf(ts.sa)
pacf(ts.sa)
seasonplot(ts.sa, 365, col=rainbow(3) ,pch= , year.labels=TRUE, main="Seasonal plot: Dissolved Oxygen") # seasonal frequency set as 12 for monthly data.


#Arima De-seasonalized model
deseason.model = arima(ts.sa, order=c(2,1,1)); deseason.model

checkresiduals(deseason.model)
acf(residuals(deseason.model))
pacf(residuals(deseason.model))

#Autoarima De-seasonalized model
deseason.model2 = auto.arima(ts.sa, seasonal=FALSE); deseason.model2
checkresiduals(deseason.model2)
acf(residuals(deseason.model2), lag=36)
pacf(residuals(deseason.model2), lag=36)



#####FORECAST#####
par(mfrow=c(1,1))

test =  window(diss.ox.test, start=c(2016,1))
train = window(diss.ox, start=c(2013,1))
test_sqrt =  sqrt(test)
train_sqrt = sqrt(train)

arima.sqrt = arima(train_sqrt, order=c(3,1,3))
arima.sqrt

arima.sqrt$x <- sqrt.ox


first15= head(test_sqrt, 15)


fcast = forecast(arima.sqrt,h=15)

plot(fcast, main = "Forecasts for Sqrt ARIMA (3,1,3)", xlab= "Simulations", ylab="Forecasts", xlim=c(2015.8,2016.03))
points((fcast$mean),lty = 2, col = "blue", lwd = 1, pch = 4)
points(first15, col = "red", pch = 2)
legend("topleft", pch=c(2,4), col=c("red", "blue"), legend = c("Observed Values", "Predicted Values"))

forecast = (fcast$mean)^2
truth = (first15)^2

sum((head((forecast),15)-head((truth),15))^2)/15

#percent error
(1/5)*sum((head((forecast),5)-head((truth),5))/(head((truth),5)))
(1/10)*sum((head((forecast),10)-head((truth),10))/(head((truth),10)))
(1/15)*sum((head((forecast),15)-head((truth),15))/(head((truth),15)))
