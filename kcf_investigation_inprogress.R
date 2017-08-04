
.libPaths("C:/Libraries/R_library")

list.of.packages <- c("ggplot2","readxl", "Rcpp","robustbase","entropy","spectrum","TSA","asta", "forecast")
install.packages(list.of.packages)
#if(length(new.packages)) install.packages(new.packages)
install.packages("forecast")
install.packages("fitdistrplus")
install.packages("moments")
install.packages("mclust")
library(moments)
library(mclust)
library(entropy)
library(astsa)
library(forecast)
library(TSA)
library(readxl)
library(tseries)
library(MASS)
#
## 17-01

library(fitdistrplus)
#
kcf_sum.y<-as.data.frame(kcf_sum[kcf_sum$node_acc_sensorRole=="VibrationYAxis",],na.rm=TRUE)
kcf_sum.x<-as.data.frame(kcf_sum[kcf_sum$node_acc_sensorRole=="VibrationXAxis",],na.rm=TRUE)
fleet    <-(unique(kcf_sum.x$node_acc_serialNumber))
#
#kcf_raw <- read.csv("C:/Users/dredmond/Downloads/VES003_3.0.3/VES003_3.0.3/VES003_3.0.3/VES003_3.0.3/kcf/serial_00001824_v8_expandedValues.csv",header=TRUE)
#kcf_raw.x<-as.numeric( kcf_raw[kcf_raw$node_sensorRole=="VibrationXAxis"& kcf_raw$node_serialNumber=="1824","value"] )
#kcf_raw.y<-as.numeric( kcf_raw[kcf_raw$node_sensorRole=="VibrationYAxis"& kcf_raw$node_serialNumber=="1824","value"] )
#kcf_raw.y[1:3]
#kcf_sum.y<-as.numeric(kcf_sum[kcf_sum$node_acc_sensorRole=="VibrationYAxis"& kcf_sum$node_acc_serialNumber=="i"])
#kcf_sum.x<-as.numeric(kcf_sum[kcf_sum$node_acc_sensorRole=="VibrationXAxis"& kcf_sum$node_acc_serialNumber==i])
kcf_sum.y[1:3,]



par(mfrow=c(2,1))
plot(kcf_sum.y$node_acc__datetime[-1], diff(kcf_sum.y$RMS),col="blue", pch=3, main="difference of RMS")
plot(kcf_sum.x$node_acc__datetime[-1], diff(kcf_sum.x$RMS),col="green", pch=3, main="difference of RMS")

par(mfrow=c(2,2))
hist(as.numeric(kcf_sum.y$kurtValues), freq=FALSE, 100, main="00001822 y-axis Distribution of Kurtosis", col="green")
hist(as.numeric(kcf_sum.y$skewValues), freq=FALSE, 100, main="00001822 y-axis Distribution of Skew", col="blue")
hist(as.numeric(kcf_sum.y$RMS), freq=FALSE, 100, main="00001822 y-axis Distribution of RMS", col="yellow")
hist(as.numeric(kcf_sum.y$crestFactor), freq=FALSE, 100, main="00001822 y-axis Distribution of Crestfactor", col="cyan")

par(mfrow=c(2,2))
hist(as.numeric(kcf_sum.x$kurtValues), freq=FALSE, 100, main="00001822 x-axis Distribution of Kurtosis", col="green")
hist(as.numeric(kcf_sum.x$skewValues), freq=FALSE, 100, main="00001822 x-axis Distribution of Skew", col="blue")
hist(as.numeric(kcf_sum.x$RMS), freq=FALSE, 100, main="00001822 x-axis Distribution of RMS", col="yellow")
hist(as.numeric(kcf_sum.x$crestFactor), freq=FALSE, 100, main="00001822 x-axis Distribution of Crestfactor", col="cyan")
par(mfrow=c(1,1))


# RAW data plotted for machine
par(mfrow=c(2,1))
#hist(kcf_raw.x, freq=FALSE, 100, main="00001824 x-axis Distribution of RAW data", col="green")
#hist(kcf_raw.y, freq=FALSE, 100, main="00001824 Y-axis Distribution of RAW data", col="Blue")
#
#
#
####
# Multivariate Case
# investiagte the RMS trends for X and Y sensors
# input  x_mat is 2-dim matrix
fit_mv<-function(x,y){
  fitW_x <-  fitdist(x, "weibull")
  fitg_x <-  fitdist(x, "gamma")
  fitln_x <- fitdist(x,"lnorm")
  fitW_y <-  fitdist(y, "weibull")
  fitg_y <-  fitdist(y, "gamma")
  fitln_y <- fitdist(y,"lnorm")
  par(mfrow=c(2,4))
  cdfcomp(list(fitW_x, fitg_x, fitln_x),  legendtext=c("Weibull_x", "gamma_x","lognormal_x"))
  cdfcomp(list(fitW_y, fitg_y, fitln_y ), legendtext=c("Weibull_y", "gamma_y", "lognormal_y"))
  denscomp(list(fitW_x, fitg_x, fitln_x), legendtext=c("Weibull_x", "gamma_x", "lognormal_x"))
  denscomp(list(fitW_y, fitg_y, fitln_y), legendtext=c("Weibull_y", "gamma_y", "lognormal_y"))
  qqcomp(list(fitW_x,  fitg_x, fitln_x),  legendtext=c("Weibull_x", "gamma_x", "lognormal_x"))
  qqcomp(list(fitW_y,  fitg_y, fitln_y),  legendtext=c("Weibull_y", "gamma_y", "lognormal_y"))
  ppcomp(list(fitW_x,  fitg_x, fitln_x),  legendtext=c("Weibull_x", "gamma_x", "lognormal_x"))
  ppcomp(list(fitW_y,  fitg_y, fitln_y),  legendtext=c("Weibull_y", "gamma_y", "lognormal_y"))
  gofstat(list(fitW_x, fitg_x, fitln_x),  fitnames=c("Weibull_x", "gamma_x", "lognormal_x"))
  gofstat(list(fitW_y, fitg_y, fitln_y),  fitnames=c("Weibull_y", "gamma_y", "lognormal_y"))
}
#
#   build ARIMA model for the RAW data..... and test the forecast performance against the summary ARIMA model
#
arma_plot<-function(x,y)
  # function to plot the forecast and actual points based on the last 600 points
  # using auto.arima function to calculate the p,d,q
  {
  m=nrow(x)
  n=nrow(y)
  kcf_kit.x<-auto.arima(x)
  kcf_kit.y<-auto.arima(y)
  par(mfrow=c(2,1))
  plot(forecast(x[1:(m-h)] ,h=h),main="bearing on X-axis")
  points(x,col="RED")
  plot(forecast(y[1:(n-h)],h=h),main="bearing on Y-axis")
  points(y,col="RED")
  }
#
#
# select the region for Modelling
m=nrow(kcf_sum.x)
n=nrow(kcf_sum.y)
k=2000
h=20
#
############
##
x<-as.numeric(kcf_sum.x$skewValues[(m-k):(m)])
y<-as.numeric(kcf_sum.y$skewValues[(n-k):(n)])
arma_plot(x,y)
# DONT try to PDF fit Skewness as its go neg. numbers fit_mv(x,y)
par(mfrow=c(2,1))
plot.ts(diff(x),col="red", main="X-axis RMS")
plot.ts(diff(y),col="green", main="y-axis RMS")
#
x<-as.numeric(kcf_sum.x$CrestFactor[(m-k):(m)])
y<-as.numeric(kcf_sum.y$crestFactor[(n-k):(n)])
#
par(mfrow=c(2,2))
plot.ts(diff(x),col="red", main="X-axis Crest")
plot.ts(diff(y),col="green", main="y-axis Crest")
x<-as.numeric(kcf_sum.x$kurtValues[(m-k):(m)])
y<-as.numeric(kcf_sum.y$kurtValues[(n-k):(n)])
#
plot.ts(diff(x),col="red", main="X-axis Kurt")
plot.ts(diff(y),col="green", main="y-axis Kurt")


x<-as.numeric(kcf_sum.x$kurtValues[(m-k):(m)])
y<-as.numeric(kcf_sum.y$kurtValues[(n-k):(n)])
#arma_plot(x,y)
fit_mv(x,y)
#
x<-as.numeric(kcf_sum.x$crestFactor[(m-k):(m)])
y<-as.numeric(kcf_sum.y$crestFactor[(n-k):(n)])
#arma_plot(x,y)
fit_mv(x,y)
#
#
#
## 24-jan
i=0
fleet<-(unique(kcf_sum.x$node_acc_serialNumber))
#
#
#
#
# RMS is col = 20
# Crest = 19
par(mfrow=c(3,2))
r<-length(fleet)
param_vec<-data.frame(meanlog=0,sdlog=r,bic=0)
param_vec<-cbind(fleet,param_vec)
colnames(param_vec)<-c("fleet","meanlog","sdlog","bic")
for (i in fleet)
  {
  i.ix<-as.numeric(kcf_sum.x[kcf_sum.x$node_acc_serialNumber==i,20],na.rm=TRUE)
  #ts.plot(i.ix[10:200], ylim=c(0,0.3))
  fitln_x <- fitdist(i.ix[20:200],"lnorm")
  param_vec[param_vec$fleet==i,2:4]<-c(fitln_x$estimate, fitln_x$bic)
  #param_vec[param_vec$fleet==i,2:3]<-fitln_y$estimate
  }
par(mfrow=c(1,1))
pairs(param_vec)
par(mfrow=c(2,2))
hist(param_vec$meanlog,col="blue")
hist(param_vec$sdlog, col="red")

##############################
# Skew will have some values < 0 so not suitable for the fit_mv plots
par(mfrow=c(2,1))
x<-as.numeric(kcf_sum.x$RMS[(m-k):(m)])
y<-as.numeric(kcf_sum.y$RMS[(n-k):(n)])
hist(x,90,col="blue" ,freq=FALSE)
hist(y,90,col="green",freq=FALSE)
#
# Looks like Cauchy with the fat tails.....
#
loglik<-function(theta,x)
{
  mu<-theta[1]
  sigma<-theta[2]
  sum(dlnorm(x,meanlog=mu,sdlog=sigma,log=TRUE))
}
#
#
#
#
x<-as.numeric(kcf_sum.x$RMS)
mu0   <-mean(log(x))
sigma0<-sd(log(x))
theta0<-c(mu0,sigma0)
fit   <- optim(par=theta0,loglik,method="BFGS",x=x,control=list(),hessian=TRUE)
#
#
#
apply(apply(kcf_sum.y,2,is.na),2,sum)
apply(apply(kcf_sum.x,2,is.na),2,sum)
# pair_id seems to be the only one
#  take a sample of the data
j = 200
k = j+ 2000
h=20
y.ts<-auto.arima(kcf_sum.y$RMS[j:k],seasonal = FALSE,allowdrift=TRUE,allowmean = TRUE)
x.ts<-auto.arima(kcf_sum.x$RMS[j:k],seasonal = FALSE,allowdrift=TRUE,allowmean = TRUE)
plot(x.ts$residuals,pch=3,col="red")
plot(y.ts$residuals, col="green")
hist(x.ts$residuals,60,col="green")
#
hist(y.ts$residuals,60,col="red")
#
#
plot(forecast(x.ts, h=h))
points(kcf_sum.x$RMS[j:(k+h)],pch=4,col="cyan")
plot(forecast(y.ts, h=h))
points(kcf_sum.y$RMS[j:(k+h)],pch=4,col="cyan")
#
#
ccf((kcf_sum.y$RMS),(kcf_sum.x$RMS),lag=6)
ccf(diff(kcf_sum.y$RMS),diff(kcf_sum.x$RMS),lag=6)
hist(kcf_sum.y$RMS,90,col="red",freq=FALSE,main="")

#
#
# outliers
outlier.y <- boxplot.stats(diff(kcf_sum.y$RMS))$out
outlier.x <- boxplot.stats(diff(kcf_sum.x$RMS))$out

no_outlier.y<-ifelse(diff(kcf_sum.y$RMS) %in% outlier.y,0,diff(kcf_sum.y$RMS))
no_outlier.x<-ifelse(diff(kcf_sum.x$RMS) %in% outlier.x,0,diff(kcf_sum.x$RMS))

plot(kcf_sum.y$node_acc__datetime[-1], no_outlier.y,col="red",pch=3, main="difference of RMS outlier removed")
plot(kcf_sum.x$node_acc__datetime[-1], no_outlier.x,col="green",pch=3, main="difference of RMS outlier removed")
hist(no_outlier.x,90,col="red",freq=FALSE,main="")
#
qqnorm(y.ts$residuals)
qqline(y.ts$residuals,col="RED")
#
qqnorm(x.ts$residuals)
qqline(x.ts$residuals,col="RED")
#
hist(kcf_sum.y$RMS,90,col="red")
hist(kcf_sum.x$RMS,90,col="green")
#
#
outlier.y <- boxplot.stats(diff(kcf_sum.y$RMS))$out
kcf_sum.out<-ifelse(diff(kcf_sum.y$RMS) %in% outlier.y,NA,diff(kcf_sum.y$RMS))
outlier.x <- boxplot.stats(diff(kcf_sum.x$RMS))$out
kcf_sum.out<-ifelse(diff(kcf_sum.x$RMS) %in% outlier.x,NA,diff(kcf_sum.x$RMS))
plot((kcf_sum.out),col="red")



