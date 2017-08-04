
.libPaths("C:/Libraries/R_library")
install.packages("jsonlite")
library(jsonlite)
library(tibble)
library(reshape2)
library(magrittr)
library(dplyr)
library(readr)
setwd("C:/Users/dredmond/Downloads/")
datax<-as.tibble(fromJSON("KCFVibrationSensorNode_0000161C_102908.json"))
datay<-as.tibble(read_csv("KCFVibrationSensorNode_0000161C_102907.json"))
#
# from KCF dashboard
datax<-read_csv("spectrum_export_Peak Acceleration.csv",skip=2)
datay<-read_csv("spectrum_export_Peak Acceleration_y.csv",skip=2)




# from
glimpse(datax)

data_x = datax %>%mutate(y=seq(1:length(datax[,2])))
#
ggplot(datax , aes('S.2',y)) +geom_line()
ts.plot(data[,4],col='blue',main='sample vibration plot',ylab="ave.g/in/sec",xlab='time steps')
hist(data[,4],50)
dim(data)
# Also could download the package in CSV format......
data<-read.csv("KCF_ADI_S3_Loop_20170621140449674.csv")
#  selecting 0000181A and break up the data to X- Y axis
x_d<-data[which( (data[,2]=="VibrationXAxis") & (data[,1]=="0000181A")),]
y_d<-data[which( (data[,2]=="VibrationYAxis") & (data[,1]=="0000181A")),]
x2_d %>% subset()

x1_d<-as.data.frame(x_d[, c("kurtValues","skewValues","crestFactor","RMS","maxAbsValues","entropyValues",'node_acc__datetime' )],na.rm=TRUE)
y1_d<-as.data.frame(y_d[, c("kurtValues","skewValues","crestFactor","RMS","maxAbsValues","entropyValues")],na.rm=TRUE)
#### Coarse plotting.....
plot.ts(x1_d,color=c(1:6))
plot.ts(y1_d)
pairs(x1_d)
#
par(mfrow=c(1,1))
# distribution of Kurtosis
library(ggplot2)
library(reshape2)
library(lubridate)
library(lvplot)
#
xyplot.ts(kurtValues ~ skewValues + crestFactor + RMS + maxAbsValues, data= y1_d )

x1_d<-cbind(x1_d,robust_mah)

x1_melt<-melt(x1_d,measure.vars =1:6 )
y1_melt<-melt(y1_d,measure.vars =1:6 )
# Generate plt object for X, and for Y
#
#
plt <- plt +  geom_boxplot() + labs(x="Box plot showing distribution for features", y="unScaled")


#pltx<-ggplot(data = x1_melt, aes(x=node_acc__datetime, y=value, color=variable))+
      geom_boxplot()
plt

plt  + geom_boxplot()+
boxplot(x1_d$kurtValues,x1_d$skewValues,x1_d$crestFactor,x1_d$RMS,x1_d$maxAbsValues)
boxplot(x1_scale$kurtValues, x1_scale$skewValues, xlab=c("kurt","skew"))







library(dplyr)
library(grid)

plot1 <- x1_d %>%
  select(node_acc__datetime, kurtValues) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = node_acc__datetime, y = kurtValues ) , size = 0.5, alpha = 0.75) +
  ylab("Kurtosis") +
  theme_bw() +
  theme(axis.title.x = element_blank())

plot2 <- x1_d %>%
  select(node_acc__datetime, skewValues) %>%
  na.omit() %>%
  ggplot() +
  geom_line(aes(x = node_acc__datetime, y = skewValues), size = 0.5, alpha = 0.75) +
  ylab("SkewValues") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot3 <- x1_d %>%
  select(node_acc__datetime, crestFactor) %>%
  na.omit() %>%
  ggplot() +
  geom_line(aes(x = node_acc__datetime, y = crestFactor), size = 0.5, alpha = 0.75) +
  ylab("CrestFactor") +
  theme_minimal() +
  theme(axis.title.x = element_blank())




grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), ggplotGrob(plot3), size = "last"))






##############BOx plots for

pltx <- ggplot(x1_melt, aes(x=factor(variable),y=value, fill=variable))+
   geom_boxplot() +
   stat_boxplot(geom='errorbar')+
   labs(x="KCF X-Axis 000181A features") +
   theme(axis.text.x=element_text(angle=0, vjust=0.4,hjust=0.5))
pltx
ggplot()
#
#
plty <- ggplot(y1_melt, aes(x=factor(variable),y=value, fill=variable))+
  geom_boxplot(alpha=0.25) +
  stat_boxplot(geom='errorbar')+
  labs(x="KCF Y-Axis 000181A features") +
  theme(axis.text.x=element_text(angle=0, vjust=0.4,hjust=0.5))
plty
#
#install.packages('qcc')
library(qcc)
qcc(data=x1_d$robust_mah[900:1000], type='xbar.one',plot=TRUE)



# shows the distribution for each feature
#
#
plty <- ggplot(y1_melt, aes(x=factor(variable),y=value, fill=variable))+
        geom_density() +
        stat_function(fun=norm, geom='line')
plty




k_mu<-  mean(x1_d$kurtValues)
k_sd<-  sd(x1_d$kurtValues)
sk_mu<-  mean(x1_d$skewValues)
sk_sd<-  sd(x1_d$skewValues)
rms_mu<-  mean(x1_d$RMS)
rms_sd<-  sd(x1_d$RMS)
cr_mu<-  mean(x1_d$crestFactor)
cr_sd<-  sd(x1_d$crestFactor)
en_mu<-  mean(x1_d$entropyValues)
en_sd<-  sd(x1_d$entropyValues)

#
plt_psd<-ggplot(x1_d, aes())+
 geom_density( aes(x=(kurtValues)),col='red')+
  stat_function(fun=dnorm,args=list(k_mu,k_sd),col='blue', lty=2) +
 geom_density( aes(x=skewValues),col='red')+
  stat_function(fun=dnorm,args=list(sk_mu,sk_sd),col='green', lty=2)+
  geom_density( aes(x=RMS),col='red')+
  stat_function(fun=dnorm,args=list(rms_mu,rms_sd),col='cyan', lty=2)+
  geom_density( aes(x=crestFactor),col='red')+
  stat_function(fun=dnorm,args=list(cr_mu,cr_sd),col='purple', lty=2)+
  geom_density( aes(x=entropyValues ),col='red')+
  stat_function(fun=dnorm,args=list(en_mu,en_sd),col='darkgreen', lty=2)
  facet_wrap( )


    facet_wrap(c("kurtValues","skewValues","RMS","crestFactor","entrogy")~, nrow=2)


#plt_psd<-ggplot(x1_d, aes(x=entropyValues), fill=variable,col='red')+ geom_density()+












k_mu<-mean(x1_d$kurtValues)
k_sd<-sd(x1_d$kurtValues)#
p + stat_function(fun = dnorm, args = list(mean = k_mu, sd = k_sd), colour = 'red')
#
# distribution of RMS
#
r_mu<-mean(x1_d$RMS)
r_sd<-sd(x1_d$RMS)
p = ggplot(x1_d, aes(x = RMS)) + geom_density()
p + stat_function(fun = dnorm, args = list(mean = r_mu, sd = r_sd), colour = 'red')
# Crest
c_mu<-mean(x1_d$crestFactor)
c_sd<-sd(x1_d$crestFactor)
p = ggplot(x1_d, aes(x = crestFactor)) + geom_density()
p + stat_function(fun = dnorm, args = list(mean = c_mu, sd = c_sd), colour = 'green')
#
# distribution of Skewness
#
s_mu<-mean(x1_d$skewValues)
s_sd<-sd(x1_d$skewValues)
p = ggplot(x1_d, aes(x = skewValues)) + geom_density()
p + stat_function(fun = dnorm, args = list(mean = s_mu, sd = s_sd), colour = 'blue')
#
# distribution of AbsMax
#
a_mu<-mean(x1_d$maxAbsValues)
a_sd<-sd(x1_d$maxAbsValues)
p = ggplot(x1_d, aes(x = maxAbsValues)) + geom_density()
p + stat_function(fun = dnorm, args = list(mean = a_mu, sd = a_sd), colour = 'purple')
#
# All together
plt =
par(mfrow=c(1,1))
#
# Calculate the Mahalanobis distance for these features
#
# install.packages("reshape")#
library(mclust)
library(reshape2)
library(robustbase)
#
big_analysis   <-functionI(x1_d)


x1_d %>% select(-node_acc__datetime) %>% covMcd() %>% Mclust()$mah


    robust_mah <- df_mahalanobis$mah
    df_clust <- Mclust(robust_mah)$classification
    df_kmean <-kmeans(robust_mah,centers = 9)$cluster
    scale_factor <- as.integer(max(robust_mah) / max(df_clust) / 2)
    scale_factor2 <- as.integer(max(robust_mah) / max(df_kmean) / 2) # we know kmeans max 5
    df_plot <- data.frame(as.integer(row.names(x1_d)), robust_mah, scale_factor*df_clust, scale_factor2*df_kmean)
    colnames(df_plot) <- c('idx', 'Mahalanobis', 'Cluster_Mclust', 'Cluster_kmeans')
    melted <- melt(df_plot, id.vars="idx")
    plt <- ggplot(melted, aes(x=idx, y=value, group=variable, colour=variable)) +
     geom_point() +
     theme_classic()+

    plt <- plt + xlab("Time") + ylab('Mahalanobis')
    plt
    bic_df <- mclustBIC(x1_d)
    model_df <- summary(bic_df, data=x1_d)
    cat("summary of Mclust models", model_df)

#scale_factor <- as.integer(max(x1_d) / max(model_df$classification) / 2)
#
    df_plot2 <- data.frame(as.integer(row.names(x1_d)), x1_d$kurtValues, x1_d$skewValues,x1_d$RMS,x1_d$crestFactor, x1_d$maxAbsValues, model_df$classification)
    colnames(df_plot2) <- c('idx', 'Kurtosis', 'Skewness', 'RMS', 'Crest','MaxAbs', 'Cluster')
#
    melted <- melt(df_plot2, id.vars="idx")
    plt <- ggplot(melted, aes(x=idx, y=value, group=variable, colour=variable))
    plt <- plt + geom_density()
    plt <- plt + theme_classic()
    plt <- plt + xlab("Time") + ylab('')
    plt




    df <- data.frame(raw=v1$x[1: n * n_obs_per_file], kurt=k)

    kurtosis_aov = aov(kurt ~ raw, data=df)
    summary(kurtosis_aov)

    skewness_aov = aov(skew ~ raw, data=df)
    summary(skewness_aov)








    install.packages(c('plyr','reshape2','plotly'))
    library(plyr)
    library(reshape2)
    library(plotly)

    set.seed(1234)
    x<- rnorm(100)
    y.1<-rnorm(100)
    y.2<-rnorm(100)
    y.3<-rnorm(100)
    y.4<-rnorm(100)

    df<- (as.data.frame(cbind(x,y.1,y.2,y.3,y.4)))

    dfmelt<-melt(df, measure.vars = 2:5)

    p <- ggplot(dfmelt, aes(x=factor(round_any(x,0.5)), y=value,fill=variable))+
      geom_boxplot()+
      facet_grid(.~variable)+
      labs(x="X (binned)")+
      theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))

    p <- ggplotly(p)












gg.gauge <- function(pos,breaks=c(0,25,75,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }

ggplot()+
  geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="forestgreen")+
  geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
  geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="red")+
  geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
  geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
            aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
  annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
  coord_fixed()+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank())
}

gg.gauge(52,breaks=c(0,35,70,100))




library(forecast)
raw<-as.data.frame(fromJSON("KCFVibrationSensorNode_0000161C_001213.json "))
#window size
w <- 250
raw %<>% select(-c(1:3,5)) %>% mutate(time=seq(from=1,to=length(raw$S.2)))
#
ggplot(raw ,aes(time,S.2)) + geom_point() + theme_classic()
## close inspection
ggplot(raw[520:600,] ,aes(time,S.2 )) + geom_point() + stat_smooth(span=0.15)


# drop the last 5% data points
# fit the ARMA model to the 95%
# compare or vlaidate using Forecast
#k<-length(raw$S.2)
#
g = 350
k = w+g
d_raw<-diff(log(raw$S.2))
ar.fit = auto.arima(d_raw[1:(length(d_raw)*.15)])
ar.pred= forecast(ar.fit,h=50)
plot(ar.pred)
points(raw$'S.2',col="red")


ggplot(d1.df ,aes(timestep,data )) + geom_point() + stat_smooth( span=0.3)




ar.fit = auto.arima(d2_raw[1:(length(d2_raw)*.25)])
ar.pred= forecast(ar.fit,h=50)
plot(ar.pred)
points(raw$'S.2',col="red")


d1.df<-cbind(d_raw,raw$time[-1])
colnames(d1.df)<-c('data', 'timestep')

ggplot(d1.df[520:550,] ,aes(timestep,data )) + geom_point() + stat_smooth()



