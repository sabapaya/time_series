##################################
########INSTALLMENT 1 CODE########
##################################
summary(data)

#Mean of Variables
mean(unemp.ts)
mean(goods.ts)
mean(edu.ts)

#Standard Deviation of Variables
sd(unemp.ts)
sd(goods.ts)
sd(edu.ts)

#Turning Variables into Time Series 
unemp.ts <- ts(data[,2], start = 1992, fr = 12)
goods.ts <- ts(data[,3], start = 1992, fr = 12)
edu.ts <- ts(data[,4], start = 1992, fr = 12)

#Time Series Plots 
par(mfrow=c(3,1))
plot(unemp.ts, main = "Monthly Unemployment Percent in California Jan 1992-Dec 2016", type="l")
plot(goods.ts, main = "Monthly Production of Goods Jan 1992-Dec 2016",  type="l")
plot(edu.ts, main = "Monthly Employment Ratio Based on Degree Jan 1992-Dec 2016",  type="l")

#Additive Decomposition of the Data
unemp.add = decompose(unemp.ts, type = "add")
plot(unemp.add)

#Multiplicative Decomposition of the Data 
unemp.mult = decompose(unemp.ts, type = "multi")
plot(unemp.mult)

#Seasonl Plot of the Data
boxplot(unemp.ts ~ cycle(unemp.ts), main = "Rate of Unemployment by Season", xlab="Month", ylab="Percent")

#ACF of Additive Decomposition of the Data
par(mfrow=c(1,2))
acf(unemp.add$random, lag=50, main="ACF of Additive Decomposition", na.action = na.omit)

#ACF of Multiplicative Decomposition of the Data
acf(unemp.mult$random, lag=50, main="ACF of Multiplicative Decomposition", na.action = na.omit)


##################################
#######INSTALLMENT 2 CODE#########
##################################
#Working with n-12 data
data <- read.csv("C:/Users/sabap/Desktop/170 Project/PartI/Paya-104483616-Installment3.csv")
newdata<-data[1:288,-1]

unemp1.ts <- ts(newdata[,1], start = 1992, fr = 12)
acf(unemp1.ts, main="Series unemp")

#Differencing the Data 
par(mfrow=c(2,1))
diff1=diff(unemp1.ts, lag=1, differences = 1)
acf(diff1)

diff12=diff(unemp1.ts, lag=12, difference=1)
acf(diff12)

diff1diff2=diff(diff1, lag=12, differences = 1)
acf(diff1diff2)

#Partial ACF of Y**
pacf(diff1diff2)

#Identifying an ARIMA model 
m1<-arima(unemp1.ts, order=c(2,1,1), seas=list(order=c(1,1,1), 12))
acf(residuals(m1))

m1.2<-arima(unemp1.ts, order=c(1,1,2), seas=list(order=c(1,0,1), 12))
acf(residuals(m1.2))

m2<-arima(unemp1.ts, order=c(2,1,1), seas=list(order=c(1,1,0), 12))
acf(residuals(m2))

m3<-arima(unemp1.ts, order=c(1,1,2), seas=list(order=c(1,1,0), 12))
acf(residuals(m3))

m4<-arima(unemp1.ts, order=c(1,1,2), seas=list(order=c(1,1,1), 12))
acf(residuals(m4))

#Testing to see which ARIMA model is the best 
AIC(m1); AIC(m2); AIC(m3); AIC(m4) 

t.test(m1$coef)
t.test(m2$coef) 
t.test(m3$coef)
t.test(m4$coef)

#Ljung-Box test
Box.test(m1$residuals, lag=6, type="Ljung")
Box.test(m1$residuals, lag=12, type="Ljung")
Box.test(m1$residuals, lag=18, type="Ljung")
Box.test(m1$residuals, lag=24, type="Ljung")

#Best ARIMA model
m1<-arima(unemp1.ts, order=c(2,1,1), seas=list(order=c(1,1,1), 12))
par(mfrow=c(1,2))
acf(residuals(m1), main="ACF of residuals of best fitting model")
pacf(residuals(m1), main="PACF of residuals of best fitting model")
coefficients(m1)

#Forecasting 12 steps ahead 
forecast = predict(m1, n.ahead=12)
forecast

my.predict=ts(forecast$pred, st=2016, fr=12)
my.predict

#Create time object out of sample test data
real.data=ts(data$unemp[289:300], st=2016, fr=12)
real.data

#Confidence Intervals
pcil=ts(forecast$pred-1.96*forecast$se, st=2016, fr=12)
pciu=ts(forecast$pred+1.96*forecast$se, st=2016, fr=12)

#Plot of Raw and Forecasted Data
ts.plot(cbind(unemp1.ts,real.data, my.predict, pcil, pciu), lty=c(1,1,5,5,5), 
        col=c("black", "black", "red", "blue", "blue"),
        ylab="unemployment rate", main="Unemployment Data and Forecast of the Last 12 Months")

#Root Mean Square Error of Forecast
forecast.error=real.data - my.predict
mse.forecast=sqrt((sum(forecast.error^2))/12)
mse.forecast

##################################
########INSTALLMENT 3 CODE########
##################################
unemp1.ts <- ts(newdata[,1], start = 1992, fr = 12)
goods1.ts <- ts(newdata[,2], start = 1992, fr = 12)
edu1.ts <- ts(newdata[,3], start = 1992, fr = 12)

plot(unemp1.ts, main = "Monthly Unemployment Percent in California Jan 1992-Dec 2016", type="l")
plot(goods1.ts, main = "Monthly Unemployment Percent in California Jan 1992-Dec 2016",  type="l")
plot(edu1.ts, main = "Monthly Unemployment Percent in California Jan 1992-Dec 2016",  type="l")

########
###Differencing the Data
########
#Differencing Unemp Variable 
par(mfrow=c(3,1))
diff1.unemp=diff(unemp1.ts, lag=1, differences = 1)
acf(diff1.unemp)

diff12=diff(unemp1.ts, lag=12, difference=1)
acf(diff12)

diff1diff2.unemp=diff(diff1, lag=12, differences = 1)
acf(diff1diff2.unemp)

#Differencing Goods Variable
diff1.goods=diff(goods1.ts, lag=1, differences = 1)
acf(diff1.goods)

diff12=diff(goods1.ts, lag=12, difference=1)
acf(diff12)

diff1diff2.goods=diff(diff1, lag=12, differences = 1)
acf(diff1diff2.goods)

#Differencing Edu Variable 
diff1.edu=diff(edu1.ts, lag=1, differences = 1)
acf(diff1.edu)

diff12=diff(edu1.ts, lag=12, difference=1)
acf(diff12)

diff1diff2.edu=diff(diff1, lag=12, differences = 1)
acf(diff1diff2.edu)

#########
####Section V.1 Cross Correlation
#########
par(mfrow=c(1,2))
ccf(diff1diff2.unemp, diff1diff2.goods)
ccf(diff1diff2.unemp, diff1diff2.edu)

########
####Section V.2
########
#Unit Root Test
library(tseries)
adf.test(unemp1.ts)
adf.test(goods1.ts)
adf.test(edu1.ts)

#Cointegration Test
po.test(cbind(diff1.unemp, diff1.goods))
po.test(cbind(diff1.unemp, diff1.edu))

########
####Section V.3
########
#Fitting VARS model
library(vars)
AR.data <- ar(newdata, method="burg", dmean=T, intercept=F)
AR.data$order

VAR.data <- VAR(newdata, p=15, type="const")
coef(VAR.data)

#Analyzing Residuals of AR(15) 
acf(resid(VAR.data)[,1],main="Res of AR, unemp")
acf(resid(VAR.data)[,2], main="Res of AR, goods")
acf(resid(VAR.data)[,3], main="Res of AR, edu")


par(mfrow=c(1,2))
ccf(resid(VAR.data)[,1], resid(VAR.data)[,2])
ccf(resid(VAR.data)[,1], resid(VAR.data)[,3])

########
####Section V.4
########
#Impulse Response Analysis
#Shock unemp and see what happens to unemp and the other variables over time 
irf.y=irf(VAR.data, impulse = "unemp", response = "unemp", boot =
          FALSE,n.ahead=40)
irf.x=irf(VAR.data, impulse = "unemp", response = "goods", boot =
          FALSE,n.ahead=40)
irf.z=irf(VAR.data, impulse = "unemp", response = "edu", boot =
          FALSE,n.ahead=40)
#plot the three impulse response variables
plot(irf.y)
plot(irf.x)
plot(irf.z)


#Shock goods and see what happens to goods and the other variables over time 
irf.y2=irf(VAR.data, impulse = "goods", response = "unemp", boot =
          FALSE,n.ahead=40)
irf.x2=irf(VAR.data, impulse = "goods", response = "goods", boot =
          FALSE,n.ahead=40)
irf.z2=irf(VAR.data, impulse = "goods", response = "edu", boot =
          FALSE,n.ahead=40)

#plot the three impulse response variables
plot(irf.y2)
plot(irf.x2)
plot(irf.z2)

#Shock edu and see what happens to edu and the other variables over time 
irf.y3=irf(VAR.data, impulse = "edu", response = "unemp", boot =
          FALSE,n.ahead=40)
irf.x3=irf(VAR.data, impulse = "edu", response = "goods", boot =
          FALSE,n.ahead=40)
irf.z3=irf(VAR.data, impulse = "edu", response = "edu", boot =
          FALSE,n.ahead=40)

#plot the three impulse response variables
plot(irf.y3)
plot(irf.x3)
plot(irf.z3)

#######
####Section V.5
#######
#Forecast
VAR.pred <- predict(VAR.data, n.ahead=12)
VAR.pred
unemp.pred <- ts(VAR.pred$fcst$unemp[,1],st=2016,fr=12) 

#Confidence Interval
ciu<-ts(VAR.pred$fcst$unemp[,3], st=2016, frequency=12)
cil<-ts(VAR.pred$fcst$unemp[,2], st=2016, frequency=12)

#Put predicted values in the time plot 
ts.plot(cbind(window(unemp1.ts, start = 1992), unemp.pred, cil, ciu),
        lty=c(1,2,2,2),col=c("Black", "Red", "Blue", "Blue"), ylab="unemployment rate", 
        main="Unemployment Data and Forecast of the Last 12 Months")


#RMSE
real.data=ts(data$unemp[289:300], st=2016, fr=12)
real.data

var.forecast.error=real.data - unemp.pred
var.mse.forecast=sqrt((sum(var.forecast.error^2))/12)
var.mse.forecast


##################################
########INSTALLMENT 4 CODE########
##################################
data <- read.csv("C:/Users/sabap/Desktop/170 Project/PartI/ts_dat.csv")
newdata<-data[1:288,-1]

#Turning All Variables into Time Series 
unemp.ts <- ts(newdata[,1], start = 1992, fr = 12)
goods.ts <- ts(newdata[,2], start = 1992, fr = 12)
edu.ts <- ts(newdata[,3], start = 1992, fr = 12)

data.ts=cbind(unemp.ts, goods.ts, edu.ts)


#Fitting a Regression Model
model<-lm(unemp.ts~ goods.ts+edu.ts+factor(months))
par(mfrow=c(1,2))
acf(model$residuals)
pacf(model$residuals)

#Checking the Residuals and Other Assumptions
par(mfrow=c(2,2))
plot(y=rstudent(model), x=as.vector(time(unemp.ts)), xlab="time", 
     ylab="Standardized Residuals", type="o")
abline(h=0)
acf(rstudent(model))
hist(rstudent(model), xlab="Standardized Residuals")
qqnorm(rstudent(model))

dev.off()


#Finding the AR fit
acf(ts(rstudent(model)))
pacf(ts(rstudent(model)))

reg.AR1 <- arima(ts(model$residuals), order=c(13,0,0), include.mean = F)
acf(residuals(reg.AR1))


#Creating a Dummy Variable 
months=cycle(unemp.ts)
months

model1=lm(data.ts~factor(months)-1); summary(model1)
model2=lm(data.ts~factor(months)); summary(model2)

plot(ts(fitted(model2), freq=12, start=c(1992,1)), col="red", ylab="data and fitted values", type="l",
     ylim=range(c(fitted(model2), data.ts)))
lines(data.ts)

#Fitting a Seasonal Model 
times=time(unemp.ts)
model1<-lm(unemp.ts~goods.ts+edu.ts+times+times^2+times^3+factor(months))
summary(model1)
acf(residuals(model1))
pacf(residuals(model1))

arima.mod=arima(ts(rstudent(model1)), order=c(13,0,0))
acf(residuals(arima.mod))

dev.off()

###GLS Model Seasonal Dummies and Time Trend
library(nlme)
gmodel=gls(unemp.ts~goods.ts+edu.ts+factor(months)+times+times^2+times^3, correlation = corARMA(c( 0.7095,  0.2357,  -0.0229, -0.0358,
                                                                                   0.0073, 0.0090,  -0.0336, 0.0354, 
                                                                                   0.1371, -0.0069,  -0.0842,  0.2485, 
                                                                                   -0.2557), p=13))
gmodel
summary(gmodel)
acf(residuals(gmodel))

#ACF of GLS model
ystar=unemp.ts[-1]- 0.7095 *unemp.ts[-length(unemp.ts)] - 0.2357*unemp.ts[-length(unemp.ts)] 
+ 0.0229*unemp.ts[-length(unemp.ts)] + 0.0358*unemp.ts[-length(unemp.ts)] -0.0073 *unemp.ts[-length(unemp.ts)]
- 0.0090*unemp.ts[-length(unemp.ts)] + 0.0336*unemp.ts[-length(unemp.ts)] -  0.0354*unemp.ts[-length(unemp.ts)]
- 0.1371*unemp.ts[-length(unemp.ts)] + 0.0069*unemp.ts[-length(unemp.ts)]  + 0.0842*unemp.ts[-length(unemp.ts)] 
- 0.2485*unemp.ts[-length(unemp.ts)] + 0.2557*unemp.ts[-length(unemp.ts)] 

xstar=goods.ts[-1]- 0.7095 *goods.ts[-length(goods.ts)] - 0.2357*goods.ts[-length(goods.ts)]
+ 0.0229*goods.ts[-length(goods.ts)] + 0.0358*goods.ts[-length(goods.ts)] -0.0073 *goods.ts[-length(goods.ts)]
- 0.0090*goods.ts[-length(goods.ts)] + 0.0336*goods.ts[-length(goods.ts)] -  0.0354*goods.ts[-length(goods.ts)]
- 0.1371*goods.ts[-length(goods.ts)] + 0.0069*goods.ts[-length(goods.ts)]  + 0.0842*goods.ts[-length(goods.ts)] 
- 0.2485*goods.ts[-length(goods.ts)] + 0.2557*goods.ts[-length(goods.ts)]

zstar=edu.ts[-1]- 0.7095 *edu.ts[-length(edu.ts)] - 0.2357*edu.ts[-length(edu.ts)]
+ 0.0229*edu.ts[-length(edu.ts)] + 0.0358*edu.ts[-length(edu.ts)] -0.0073 *edu.ts[-length(edu.ts)]
- 0.0090*edu.ts[-length(edu.ts)] + 0.0336*edu.ts[-length(edu.ts)] -  0.0354*edu.ts[-length(edu.ts)]
- 0.1371*edu.ts[-length(edu.ts)] + 0.0069*edu.ts[-length(edu.ts)]  + 0.0842*edu.ts[-length(edu.ts)] 
- 0.2485*edu.ts[-length(edu.ts)] + 0.2557*edu.ts[-length(edu.ts)] 

mod=lm(ystar~xstar+zstar)
acf(ts(mod$residuals))

#Forecast Plot
new.t<-seq(2016, len=288, by=1/12)
new.dat2=data.frame(months=rep(1:12,1), times=new.t)
predicted2=predict(gmodel, new.dat2)[1:12]

hat.x=ts(predicted2, st=2016, fr=12)
ts.plot(unemp.ts, hat.x, lty=1:2, main="Unemployment Data and Forecast of Last 12 Months", ylab="Value of Unemp", xlab="Time", col=c("black", "red"))

#RMSE
real.data=ts(data$unemp[289:300], st=2016, fr=12)
real.data

reg.forecast.error=real.data - predicted2
reg.mse.forecast=sqrt((sum(reg.forecast.error^2))/12)
reg.mse.forecast


##################################
########INSTALLMENT 5 CODE########
##################################
unemp.ts <- ts(newdata[,1], start = 1992, fr = 12)
exp2<- HoltWinters(unemp.ts, beta = FALSE, gamma=FALSE) #without trend and without seasonal component
exp3<- HoltWinters(unemp.ts, gamma=FALSE)  #with trend and no seasoanl component
exp4<- HoltWinters(unemp.ts, seasonal = "add")
exp5<- HoltWinters(unemp.ts, seasonal = "mult")

plot(unemp.ts)
lines(fitted(exp2)[, 1], type="l", col="red")
plot(unemp.ts)
lines(fitted(exp3)[, 1], type="l", col="red")
plot(unemp.ts)
lines(fitted(exp5)[, 1], type="l", col="red")
plot(unemp.ts)
lines(fitted(exp4)[, 1], type="l", col="red")

exp.mse.forecast2=sqrt(exp2$SSE/length(data$unemp))
exp.mse.forecast2
exp.mse.forecast3=sqrt(exp3$SSE/length(data$unemp))
exp.mse.forecast3
exp.mse.forecast4=sqrt(exp4$SSE/length(data$unemp))
exp.mse.forecast4
exp.mse.forecast5=sqrt(exp5$SSE/length(data$unemp))
exp.mse.forecast5

#Forecast 12 steps ahead
exp.predict=predict(exp4, n.ahead=12)
exp.predict

#Plot with forecasted values 
ts.plot(unemp.ts, predict(exp4, n.ahead=12), main="Forecasted values (dashed line)", lty=1:2, col=c("black", "red"))

#RMSE
real.data=ts(data$unemp[289:300], st=2016, fr=12)
real.data

exp.forecast.error=real.data - exp.predict
exp.mse.forecast=sqrt((sum(exp.forecast.error^2))/12)
exp.mse.forecast


#####################
#####Conclusion######
#####################

#Calculating Average RMSE of all 4 forecasts 
real.data=ts(data$unemp[289:300], st=2016, fr=12)
real.data

avg.data=c(6.6208, 6.433,6.324,5.777,5.633,6.021,6.294,
           5.99,5.609,5.614,5.667,5.539)

forecast.error=real.data - avg.data
mse.forecast=sqrt((sum(forecast.error^2))/12)
mse.forecast

#Calculating RMSE for Short term 
real.data1 <- ts(c(5.9, 5.8), st = 2016, frequency = 12)
my.predict1 <- ts(c(6.164,5.958), st = 2016, frequency = 12)
error.forecast1 <- real.data1 - my.predict1
arima.mse.forecast1 <- sqrt((sum(error.forecast1^2))/2)
arima.mse.forecast1

real.data2 <- ts(c(5.9, 5.8), st = 2016, frequency = 12)
my.predict2 <- ts(c(6.137,5.918), st = 2016, frequency = 12)
var.forecast.error1 <- real.data2 - my.predict2
var.mse.forecast1 <- sqrt((sum(var.forecast.error1^2))/2)
var.mse.forecast1

real.data3 <- ts(c(5.9, 5.8), st = 2016, frequency = 12)
my.predict3 <- ts(c(7.991,7.858), st = 2016, frequency = 12)
reg.forecast.error1 <- real.data3 - my.predict3
reg.mse.forecast1 <- sqrt((sum(reg.forecast.error1^2))/2)
reg.mse.forecast1

real.data4 <- ts(c(5.9, 5.8), st = 2016, frequency = 12)
my.predict4 <- ts(c(6.191,5.998), st = 2016, frequency = 12)
exp.forecast.error1 <- real.data4 - my.predict4
exp.mse.forecast1 <- sqrt((sum(exp.forecast.error1^2))/2)
exp.mse.forecast1

real.data5 <- ts(c(5.9, 5.8), st = 2016, frequency = 12)
my.predict5 <- ts(c(6.621,6.433), st = 2016, frequency = 12)
avg.forecast.error1 <- real.data5 - my.predict5
avg.mse.forecast1 <- sqrt((sum(avg.forecast.error1^2))/2)
avg.mse.forecast1
