require(downloader)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
require(Quandl)
require(data.table)

 SPYrets <- na.omit(Return.calculate(SPY$Adj_Close))

spySims <- list()
set.seed(123)
for(i in 1:999) {
   simulatedSpy <- xts(sample(coredata(SPYrets), size = length(SPYrets), replace = FALSE), order.by=index(SPYrets))
   colnames(simulatedSpy) <- paste("sampleSPY", i, sep="_")
   spySims[[i]] <- simulatedSpy
}
spySims <- do.call(cbind, spySims)
spySims <- cbind(spySims, SPYrets)
colnames(spySims)[1000] <- "Original SPY"

dailyReturnCAGR <- function(rets) {
   return(prod(1+rets)^(252/length(rets))-1)
}
rets <- sapply(spySims, dailyReturnCAGR)
drawdowns <- maxDrawdown(spySims)
calmars <- rets/drawdowns
ranks <- rank(calmars)
plot(density(as.numeric(calmars)), main = 'Calmars of reshuffled SPY, realized reality in red')
abline(v=as.numeric(calmars[1000]), col = 'red')




#####   SMOTE """"""""""""""""""""""

hyper <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data', header=F)
names <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', header=F, sep='\t')[[1]]
names <- gsub(pattern =":|[.]",x = names, replacement="")
colnames(hyper) <- names

# fix variables and column headers
colnames(hyper) <-c("target", "age", "sex", "on_thyroxine", "query_on_thyroxine",
                    "on_antithyroid_medication", "thyroid_surgery", "query_hypothyroid",
                    "query_hyperthyroid", "pregnant", "sick", "tumor", "lithium",
                    "goitre", "TSH_measured", "TSH", "T3_measured", "T3", "TT4_measured",
                    "TT4", "T4U_measured", "T4U", "FTI_measured", "FTI", "TBG_measured",
                    "TBG")
hyper$target <- ifelse(hyper$target=='negative',0,1)
head(hyper,2)

# check balance of outcome variable
print(table(hyper$target))
print(prop.table(table(hyper$target)))

# binarize all character fields
ind <- sapply(hyper, is.factor)
hyper[ind] <- lapply(hyper[ind], as.character)

hyper[ hyper == "?" ] = NA
hyper[ hyper == "f" ] = 0
hyper[ hyper == "t" ] = 1
hyper[ hyper == "n" ] = 0
hyper[ hyper == "y" ] = 1
hyper[ hyper == "M" ] = 0
hyper[ hyper == "F" ] = 1

hyper[ind] <- lapply(hyper[ind], as.numeric)

repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
hyper <- repalceNAsWithMean(hyper)

# split data into train and test portions
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(hyper$target, p = .50,
                                  list = FALSE,
                                  times = 1)
trainSplit <- hyper[ splitIndex,]
testSplit <- hyper[-splitIndex,]

prop.table(table(trainSplit$target))
prop.table(table(testSplit$target))

# model using treebag
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(target ~ ., data = trainSplit, method = "treebag",
                 trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

# evaluate the model's performance
library(pROC)
auc <- roc(testSplit$target, pred)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

# SMOTE more positive cases
library(DMwR)
trainSplit$target <- as.factor(trainSplit$target)
trainSplit <- SMOTE(target ~ ., trainSplit, 
                    perc.over = 100, perc.under=200)
trainSplit$target <- as.numeric(trainSplit$target)

prop.table(table(trainSplit$target))

# evaluate the SMOTE performance
tbmodel <- train(target ~ ., data = trainSplit, method = "treebag",
                 trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

auc <- roc(testSplit$target, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

###############################################3










require(quantmod)
require(PerformanceAnalytics)

#get the data and fill out the MA
getSymbols('SPY', from='1950-01-01')
SPY$ma200 <- SMA(Cl(SPY), 200)
SPY$ma50 <- SMA(Cl(SPY), 50)

#lets look at it from 1990 to 2015
spy <- SPY['1990/2015']

#our baseline, unfiltered results
ret <- ROC(Cl(spy)) 

#our comparision, filtered result
ma_sig <- Lag(ifelse(SPY$ma50 > SPY$ma200, 1, 0))
ma_ret <- ROC(Cl(spy)) * ma_sig

golden<- cbind(ma_ret,ret)
colnames(golden) = c('GoldCross','Buy&Hold')

#Plot to visually see the actual moving averages
chartSeries(spy, 
            type = "line",
            name = "Moving Average : Golden Cross",
            TA= c(addSMA(50, col = 'yellow'), addSMA(200)))

# lets see what the latest signals are 1 being a buy signal
tail(ma_sig)

maxDrawdown(golden)
table.AnnualizedReturns(golden, Rf= 0.02/252)
charts.PerformanceSummary(golden, Rf = 0.02, main="Golden Cross",geometric=FALSE)













syms <- c("^GSPC", "^IXIC")
baseURL <- "http://download.finance.yahoo.com/d/quotes.csvr?e=.csv&f="
formatURL <- "snl1d1t1c1p2va2bapomwerr1dyj1x"
endURL <- "&s="
url <- paste(baseURL, formatURL, endURL, paste(syms, collapse="+"), sep="")
read.csv(url, header=FALSE)