### MACHINE LEARNING VIA ARONSON'S SSML ###

#### TODO:
# It is worth noting that lagged variables were not considered in this analysis, 
#but that they can often prove to be useful.

####

## Libraries
library(caret);library(corrplot);library(minerva);
library(glmulti);library(Boruta);
library(ggplot2);library(reshape2);library(dplyr)

## Read in data
# myFile <- file.choose()
# vars  <- read.csv(myFile, header=TRUE, stringsAsFactors = FALSE)
#setwd("C:/Users/Kris/Dropbox/RobotWealth/Blog posts/ML")
setwd("/home/david/Downloads/wilm1/")

url='https://www.dropbox.com/sh/zmt0bgbo2lzbcnd/AAAWiawvxF7W-FKKmrDrenZga?dl=0&lst='
n4r_url=('https://www.dropbox.com/sh/zmt0bgbo2lzbcnd/AAAWiawvxF7W-FKKmrDrenZga?dl=0&lst=MCSV4AN4R.summary.csv')

n4r<-read.csv("/home/david/Downloads/wilm1/MCSV4AN4R.summary.csv")
hpx<-read.csv("/home/david/Downloads/wilm1/MCSVC1HPX.summary.csv")
ly9<-read.csv("/home/david/Downloads/wilm1/MCSVM7LY9.summary.csv")

j43<-read.csv("/home/david/Downloads/wilm1/MCSVSJ42V.summary.csv")
t82x<-read.csv("/home/david/Downloads/wilm1/MCSVVT82X.summary.csv")
hpa<-read.csv("/home/david/Downloads/wilm1/MCSVYBHPA.summary.csv")

#
mah   <-  function(xx,mah_offset=0) {
   mahalanobis(xx,colMeans(xx),var(xx))-mah_offset }

#rnd.date=as.Date("2017/11/30")+30*sort(stats::runif(30))
plot.ts(n4r[,7:12])
#axis.Date(1,at=seq(as.Date("2017/11/30"), max(rnd.date)+6,"days"))
pairs(n4r[,7:12])
plot.ts(hpx[,7:12])
pairs(hpx[,7:12])
plot.ts(ly9[,7:12])
pairs(ly9[,7:12])
plot.ts(j43[,7:12])
pairs(j43[,7:12])
plot.ts(t82x[,7:12])
pairs(t82x[,7:12])
plot.ts(hpa[,7:12])
pairs(hpa[,7:12])
#
#####MEAN and RMS are the same number....


kurt<-matrix(0, ncol=6, nrow=nrow(n4r))
kurt= cbind(n4r$kurtosis,hpx$kurtosis,ly9$kurtosis,j43$kurtosis,t82x$kurtosis,hpa$kurtosis)
glimpse(kurt)
kurt%<>%cbind(mah(kurt))
colheader<-c("n4r","hpx","ly9","j34","t82x","hpa","mah")
colnames(kurt)<-colheader
plot.ts(scale(kurt))
pairs(scale(kurt))

crest<-matrix(0, ncol=6, nrow=nrow(n4r))
crest= cbind(n4r$crest,hpx$crest,ly9$crest,j43$crest,t82x$crest,hpa$crest)
glimpse(crest)
colheader<-c("n4r","hpx","ly9","j34","t82x","hpa")
colnames(crest)<-colheader
plot.ts(scale(crest))
pairs(scale(crest))
crest%<>%cbind(mah(crest))
crest%<>%as.tibble(crest)
k=mean(crest$mah)
ggplot(crest,aes(mah)) +
   geom_density() +
   theme_classic()+
   stat_function(fun = dnorm, args=list(mean=k),colour="red",alpha=0.15)+
   ggtitle("Mahalanobis Density plot (centered) of Features with Normal density")

       

skew<-matrix(0, ncol=6, nrow=nrow(n4r))
skew= cbind(n4r$skewness,hpx$skewness,ly9$skewness,j43$skewness,t82x$skewness,hpa$skewness)
glimpse(skew)
skew%<>%cbind(mah(skew))
colheader<-c("n4r","hpx","ly9","j34","t82x","hpa","mah")
colnames(skew)<-colheader
plot.ts(scale(skew))
pairs(scale(skew))

rms<-matrix(0, ncol=6, nrow=nrow(n4r))
rms= cbind(n4r$rms,hpx$rms,ly9$rms,j43$rms,t82x$rms,hpa$rms)
glimpse(rms)
rms%<>%cbind(mah(rms))
colheader<-c("n4r","hpx","ly9","j34","t82x","hpa","mah")
colnames(rms)<-colheader
plot.ts(scale(rms))
pairs(scale(rms))

stdDev<-matrix(0, ncol=6, nrow=nrow(n4r))
stdDev= cbind(n4r$stdDev,hpx$stdDev,ly9$stdDev,j43$stdDev,t82x$stdDev,hpa$stdDev)
glimpse(stdDev)
stdDev%<>%cbind(mah(stdDev))
colheader<-c("n4r","hpx","ly9","j34","t82x","hpa","mah")
colnames(stdDev)<-colheader
plot.ts(scale(stdDev))
pairs(scale(stdDev))


