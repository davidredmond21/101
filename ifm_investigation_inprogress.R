
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
library(fitdistrplus)
#
ifm.de01<-read_excel("C:/Users/dredmond/Downloads/H_010.033.071.067.xlsx",
                   sheet = "(1) SCR07 M26 DE Bearings 01",
                   na = "NA")
#
ifm.de02<-read_excel("C:/Users/dredmond/Downloads/H_010.033.071.067.xlsx",
                     sheet = "(1) SCR07 M26 DE Bearings 02")
#
ifm.unbal<-read_excel("C:/Users/dredmond/Downloads/H_010.033.071.067.xlsx",
                     sheet = "(1) SCR07 M26 DE Unbalance",
                     na = "NA")
ifm.bearing01<-as.data.frame(ifm.de01[,-(3:6)],drop.na=TRUE)
ifm.bearing02<-as.data.frame(ifm.de02[,-(3:6)],drop.na=TRUE)
ifm.unbal<-as.data.frame(ifm.unbal[,-(3:6)],drop.na=TRUE)

par(mfrow=c(3,1))
plot.ts(ifm.bearing01[,2],col="blue", pch=3, main="Timeseries plot  of displacement-01")
plot.ts(ifm.bearing02[,2],col="green", pch=3, main="Timeseries plot  of displacement-02")
plot.ts(ifm.unbal[,2],col="red", pch=3, main="Timeseries plot  of unbalance")

# looking at the 1st half


par(mfrow=c(3,1))
plot.ts(ifm.bearing01[20:5000,2],col="blue", pch=3, main="Timeseries plot  of displacement-01")
plot.ts(ifm.bearing02[20:5000,2],col="green", pch=3, main="Timeseries plot  of displacement-02")
plot.ts(ifm.unbal[20:3000,2],col="red", pch=3, main="Timeseries plot  of unbalance")

x<-as.numeric(ifm.bearing01[20:4800,2])
hist(x,freq=FALSE,100, main="first 1/2 of samples RAW data-01",col="blue")
y<-as.numeric(ifm.bearing01[20:4800,2])
hist(y,freq=FALSE,100, main="first 1/2 of samples RAW data-02",col="green")
bal<-as.numeric(ifm.bearing01[20:3000,2])
hist(bal,freq=FALSE,100, main="first 1/2 of samples RAW unbal data",col="red")
fit_pdf(x)
fit_pdf(y)
fit_pdf(bal)


# PDF fitting
# input  x_mat is 1-dim matrix
fit_pdf<-function(x){
  fitW_x <-  fitdist(x, "weibull")
  fitg_x <-  fitdist(x, "gamma")
  fitln_x <- fitdist(x,"lnorm")
  par(mfrow=c(2,2))
  cdfcomp(list(fitW_x, fitg_x, fitln_x),  legendtext=c("Weibull_x", "gamma_x","lognormal_x"))
  denscomp(list(fitW_x, fitg_x, fitln_x), legendtext=c("Weibull_x", "gamma_x", "lognormal_x"))
  qqcomp(list(fitW_x,  fitg_x, fitln_x),  legendtext=c("Weibull_x", "gamma_x", "lognormal_x"))
  ppcomp(list(fitW_x,  fitg_x, fitln_x),  legendtext=c("Weibull_x", "gamma_x", "lognormal_x"))
  gofstat(list(fitW_x, fitg_x, fitln_x),  fitnames=c("Weibull_x", "gamma_x", "lognormal_x"))
}
#   build ARIMA model for the RAW data..... and test the forecast
#####
acf2(diff(x))
auto.arima(x)
sarima(x,3,1,1)
sarima(y,3,1,1)

arma_plot<-function(x,y,h=20)
  # function to plot the forecast and actual points based on the last 600 points
  # using auto.arima function to calculate the p,d,q
  {
  m=length(x)
  n=length(y)
  ifm_fit.x<-auto.arima(x)
  ifm_fit.y<-auto.arima(y)
  par(mfrow=c(2,1))
  plot(forecast(x[((m-h)-50):(m-h)],h=h),main="bearing-01")
  points(x,col="RED")
  plot(forecast(y[((n-h)-50):(n-h)],h=h),main="bearing-02")
  points(y,col="RED")
  }
####
# select the region for Modelling
#
arma_plot(x,y)
fit_mv(x,y)
#
#
# outliers
outlier.x <- boxplot.stats(x)$out
outlier.y <- boxplot.stats(y)$out
#
#
#
#
#
# outliers
outlier.x <- boxplot.stats(x)$out
outlier.y <- boxplot.stats(y)$out

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



