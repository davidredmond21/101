### MACHINE LEARNING VIA ARONSON'S SSML ###

#### TODO:
# It is worth noting that lagged variables were not considered in this analysis, 
#but that they can often prove to be useful.

####

## Libraries
library(caret);library(corrplot);library(minerva);
library(glmulti);library(Boruta);
library(ggplot2);library(reshape2)

## Read in data
# myFile <- file.choose()
# vars  <- read.csv(myFile, header=TRUE, stringsAsFactors = FALSE)
setwd("C:/Users/Kris/Dropbox/RobotWealth/Blog posts/ML")
vars <- read.csv("gu_daily_midnight.csv", stringsAsFactors = FALSE)

# shorten column names to make plots more readable
colnames(vars) <- sub("Norm", "", colnames(vars))
colnames(vars) <- sub("[N]", "", colnames(vars))

# check if columns were imported as expected
str(vars) # note several columns imported as "chr" - update next line accordingly

# these columns don't get recognised as numeric due to some NaN, missing values etc
vars[c("bWdith3", "bWdith5", "deltabWidth3", "deltabWidth5", "deltabWidth10", "bWdith10")] <- sapply(vars[c("bWdith3", "bWdith5", "deltabWidth3", "deltabWidth5", "deltabWidth10", "bWdith10")], as.numeric)
vars <- vars[complete.cases(vars), ]

# set date as index and remove from data
row.names(vars) <- vars$DD.MM.YYYY
vars$DD.MM.YYYY <- NULL

## Removing highly correlated variables
require(caret);require(corrplot)
cor.mat <- cor(vars[, -c(1:4)]) # compute the correlation matrix without the target or HLC
highCor <- findCorrelation(cor.mat, 0.3) #apply correlation filter
highCor <- highCor+4 # increment by four to account for removing the target and HLC above
vars_filt <- vars[, -c(1:4, highCor)]
cor.mat.filt <- cor(vars_filt)
corrplot(cor.mat.filt, order = "hclust", type = 'lower') #plot correlation matrix

# save variables and target in new data frame vars_filt
vars_filt$target <- vars$target

## MIC
require(minerva)
mine.filt <- mine(vars_filt[, -ncol(vars_filt)], vars_filt[, ncol(vars_filt)])
mic <- as.data.frame(mine.filt$MIC)
mic.ordered <- mic[order(mic), ,drop = FALSE]

### RESULTS
# MMIFaster         0.09817869
# deltaPVR5         0.10107728
# bWidthSlow     0.10196236
# deltaATRrat10     0.10228334
# apc5              0.10346916
# deltaATRrat3      0.10473520
# mom10             0.10593616
# trend          0.10610100
# HurstMod           0.10703185
# HurstFast          0.10810217
# atrRatSlow     0.10818756
# deltaMMIFastest10 0.10863479
# bWdith3           0.11014629
# HurstFaster        0.11493763
# ATRSlow            0.12458435

## Recursive Feature Elimination

# define the control function and CV method
cntrl <- rfeControl(functions=rfFuncs, method="cv", number=5)
rfe.results <- rfe(vars_filt[, -ncol(vars_filt)], vars_filt[, ncol(vars_filt)], sizes=c(2:15), rfeControl=cntrl)
print(rfe.results)

# list final feature set
predictors(rfe.results)
plot(rfe.results, type='l')

#### Results
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (5 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables  RMSE  Rsquared RMSESD RsquaredSD Selected
# 2 69.56 0.0010739  5.176   0.001703         
# 3 68.30 0.0028735  4.271   0.002143         
# 4 68.14 0.0024810  4.265   0.002888         
# 5 67.91 0.0015594  4.470   0.002297         
# 6 68.09 0.0045451  4.558   0.009021         
# 7 67.63 0.0054969  4.450   0.008473         
# 8 67.28 0.0029462  4.771   0.005069         
# 9 67.59 0.0026974  5.068   0.002669         
# 10 67.15 0.0029288  5.317   0.002183         
# 11 66.83 0.0009411  5.392   0.001388         
# 12 66.73 0.0021375  5.567   0.002090         
# 13 66.58 0.0018327  5.420   0.001675         
# 14 66.51 0.0014323  5.349   0.001319        *
#   15 66.71 0.0021740  5.201   0.002024         
# 
# The top 5 variables (out of 14):
#   ATRSlow, trend, HurstMod, deltaATRrat10, bWidthSlow

absretSummary <- function (data, lev = NULL, model = NULL) {
  positions <-ifelse(abs(data[, "pred"]) > 5, sign(data[, "pred"]), 0)
  trades <- abs(c(1,diff(positions))) 
  profits <- positions*data[, "obs"]
  profit <- sum(profits)
  names(profit) <- 'profit'
  return(profit)
}

cntrl$functions$summary <- absretSummary

set.seed(53)
rfe.results.custSumm <- rfe(vars_filt[, -ncol(vars_filt)], vars_filt[, ncol(vars_filt)], sizes=c(2:15), rfeControl=cntrl, metric='profit', maximize=TRUE)
print(rfe.results.custSumm)
# list final feature set
predictors(rfe.results.custSumm)
plot(rfe.results.custSumm, type='l')

# Results
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (5 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables  profit profitSD Selected
# 2 -314.89    738.4         
# 3 -230.82   1202.0         
# 4 -556.15    708.3         
# 5 -400.71    862.9         
# 6 -624.55    532.9         
# 7 -544.35    649.6         
# 8 -373.58    959.1         
# 9 -603.04    977.7         
# 10 -623.67    823.0         
# 11 -589.28    965.4         
# 12 -251.84    763.3         
# 13 -274.64    678.9         
# 14  -66.19    837.3         
# 15  104.16    681.9        *
#   
#   The top 5 variables (out of 15):
#   ATRSlow, trend, HurstMod, bWidthSlow, atrRatSlow
# "ATRSlow"            "trendNorm"          "HurstMod"           "bWidthSlow"    
# "atrRatSlow"     "deltaATRrat10"     "apc5"              "deltaMMIFastest10"
# "bWdith3"           "HurstFast"          "deltaPVR5"         "MMIFaster"        
# "deltaATRrat3"      "mom10"             "HurstFaster"

## Models with in-built feature selection

# create indexes for time series cross validation windows
init = 200 #initial window
horiz = 20 #prediction horizon
wdw <- createTimeSlices(1:nrow(vars_filt), initialWindow = init, horizon = horiz, skip = horiz-1, fixedWindow = TRUE)
trainSlices <- wdw[[1]]
testSlices <- wdw[[2]]

# verify visually correct window setup:
trainSlices[[length(trainSlices)]]
testSlices[[length(testSlices)]]

# caret trainControl function
cntrl <- trainControl(summaryFunction=absretSummary, savePredictions="all", returnResamp="all", 
                      index=trainSlices, indexOut=testSlices)

# Bagged MARS
bMARS <- train(x=vars_filt[, -ncol(vars_filt)], y=vars_filt[, ncol(vars_filt)], 
               method="bagEarth", trControl=cntrl, metric="profit", 
                   maximize=TRUE, tuneGrid = data.frame(nprune = 2,
                                                        degree = 3))
predictors(bMARS)
# "mom10"             "apc5"              "deltaATRrat10"     "atrRatSlow"       
# "ATRSlow"           "deltaATRrat3"      "bWdith3"           "MMIFaster"        
# "HurstFast"         "deltaMMIFastest10" "HurstMod"          "trend"            
# "bWidthSlow"        "deltaPVR5" 

# Boosted Generalized Additive Model
bstGAM <- train(x=vars_filt[, -ncol(vars_filt)], y=vars_filt[, ncol(vars_filt)], 
               method="gamboost", trControl=cntrl, metric="profit", 
               maximize=TRUE, tuneGrid = data.frame(mstop = c(100, 200),
                                                    prune = c(TRUE, FALSE)))
predictors(bstGAM)
# "mom10"

# Lasso
lasso <- train(x=vars_filt[, -ncol(vars_filt)], y=vars_filt[, ncol(vars_filt)], 
                method="lasso", trControl=cntrl, metric="profit", 
                maximize=TRUE, tuneGrid = data.frame(fraction = c(1:10)/10))
predictors(lasso)
# "mom10"             "apc5"              "deltaPVR5"         "deltaATRrat3"     
# "deltaATRrat10"     "bWdith3"           "MMIFaster"         "deltaMMIFastest10"
# "HurstMod"          "HurstFast"         "HurstFaster"       "trend"            
# "atrRatSlow"        "bWidthSlow"        "ATRSlow" 

# Spike and Slab
ssr <- train(x=vars_filt[, -ncol(vars_filt)], y=vars_filt[, ncol(vars_filt)], 
               method="spikeslab", trControl=cntrl, metric="profit", 
               maximize=TRUE, tuneGrid = data.frame(vars = c(2,3,4,5,10,15)))
predictors(ssr)
# "bWdith3"    "trend"      "atrRatSlow"

# Model Tree
tree <- train(x=vars_filt[, -ncol(vars_filt)], y=vars_filt[, ncol(vars_filt)], 
             method="M5", trControl=cntrl, metric="profit", 
             maximize=TRUE, tuneLength=2)
predictors(tree)
# "mom10"             "apc5"              "deltaPVR5"         "deltaATRrat3"     
# "deltaATRrat10"     "bWdith3"           "MMIFaster"         "deltaMMIFastest10"
# "HurstMod"          "HurstFast"         "HurstFaster"       "trend"            
# "atrRatSlow"        "bWidthSlow"        "ATRSlow"        

# stochastic gradient boosting
sgb <- train(x=vars_filt[, -ncol(vars_filt)], y=vars_filt[, ncol(vars_filt)], 
              method="gbm", trControl=cntrl, metric="profit", 
              maximize=TRUE, tuneLength=3)
predictors(sgb)
# "mom10"             "apc5"              "deltaPVR5"         "deltaATRrat3"     
# "deltaATRrat10"     "bWdith3"           "MMIFaster"         "deltaMMIFastest10"
# "HurstMod"          "HurstFast"         "HurstFaster"       "trend"            
# "atrRatSlow"        "bWidthSlow"        "ATRSlow"

# frequency of selection in top 5
variable <- c("mom10", "bWidth3", "apc5", "trend", "deltaATRRat10", "deltaATRRat3",
               "atrRatSlow", "deltaPVR5", "ATRSlow")
count <- c(5, 1, 4, 1, 4, 3, 2, 3, 1)
freq <- data.frame(variable, count)
ggplot(freq, aes(variable, count)) +
  geom_bar(stat="identity", fill="blue", alpha=0.7)

## Model Selection Using glmulti
require(glmulti)
y <- glm(target ~ ., data = vars_filt)
L1.models <- glmulti(y, level=1, crit="aicc")
plot(L1.models, type='s', col='blue')
print(L1.models)

# glmulti.analysis
# Method: h / Fitting: glm / IC used: aicc
# Level: 1 / Marginality: FALSE
# From 100 models:
#   Best IC: 22669.8709723958
# Best model:
#   [1] "target ~ 1 + trend + atrRatSlow"
# Evidence weight: 0.0323562049748589
# Worst IC: 22673.0396363133
# 23 models within 2 IC units.
# 92 models to reach 95% of evidence weight.

# retain models with AICs less than 2 units from 'best' model
weights <- weightable(L1.models)
bst <- weights[weights$aic <= min(weights$aic) + 2,]
print(bst) 
# model     aicc    weights
# 1                                  target ~ 1 + trend + atrRatSlow 22669.87 0.03235620
# 2                           target ~ 1 + apc5 + trend + atrRatSlow 22670.72 0.02111157
# 3                      target ~ 1 + MMIFaster + trend + atrRatSlow 22670.73 0.02104022
# 4                        target ~ 1 + bWdith3 + trend + atrRatSlow 22670.87 0.01961736
# 5                     target ~ 1 + trend + atrRatSlow + bWidthSlow 22670.96 0.01878187
# 6              target ~ 1 + deltaMMIFastest10 + trend + atrRatSlow 22671.04 0.01799825
# 7                        target ~ 1 + trend + atrRatSlow + ATRSlow 22671.20 0.01667710
# 8                                          target ~ 1 + atrRatSlow 22671.23 0.01644049
# 9               target ~ 1 + apc5 + MMIFaster + trend + atrRatSlow 22671.40 0.01504698
# 10                     target ~ 1 + deltaPVR5 + trend + atrRatSlow 22671.44 0.01474030
# 11        target ~ 1 + MMIFaster + trend + atrRatSlow + bWidthSlow 22671.49 0.01439135
# 12                     target ~ 1 + HurstFast + trend + atrRatSlow 22671.55 0.01396476
# 13                      target ~ 1 + HurstMod + trend + atrRatSlow 22671.61 0.01357259
# 14                         target ~ 1 + mom10 + trend + atrRatSlow 22671.64 0.01337186
# 15                target ~ 1 + apc5 + bWdith3 + trend + atrRatSlow 22671.65 0.01329943
# 16 target ~ 1 + MMIFaster + deltaMMIFastest10 + trend + atrRatSlow 22671.66 0.01325762
# 17                  target ~ 1 + deltaATRrat3 + trend + atrRatSlow 22671.68 0.01309016
# 18                 target ~ 1 + deltaATRrat10 + trend + atrRatSlow 22671.69 0.01303342
# 19                   target ~ 1 + HurstFaster + trend + atrRatSlow 22671.70 0.01294480
# 20           target ~ 1 + bWdith3 + MMIFaster + trend + atrRatSlow 22671.72 0.01283320
# 21             target ~ 1 + apc5 + trend + atrRatSlow + bWidthSlow 22671.74 0.01268352
# 22      target ~ 1 + apc5 + deltaMMIFastest10 + trend + atrRatSlow 22671.83 0.01216110
# 23          target ~ 1 + bWdith3 + trend + atrRatSlow + bWidthSlow 22671.85 0.01205142

## generalized linear model with stepwise selection
cntrl <- trainControl(summaryFunction=absretSummary, method="timeslice", initialWindow=200, horizon=1, fixedWindow=TRUE)
glmStepAICModel <- train(vars_filt[, -ncol(vars_filt)], vars_filt[, ncol(vars_filt)], method = "glmStepAIC", 
                         trControl = cntrl, metric = "profit", maximize = TRUE)

print(glmStepAICModel$finalModel)
# Call:  NULL
# 
# Coefficients:
#   (Intercept)        trend   atrRatSlow  
# -1.907       -3.632       -5.100  
# 
# Degrees of Freedom: 2024 Total (i.e. Null);  2022 Residual
# Null Deviance:	    8625000 
# Residual Deviance: 8593000 	AIC: 22670

## Boruta all relevant feature selection
require(Boruta)
set.seed(53)
bor <- Boruta(target ~ ., data = vars_filt, maxRuns=1000)
plot(bor, las=3, xlab=NULL, colCode=c("seagreen3", "goldenrod1", "tomato2", "dodgerblue3"))
print(bor)

# Boruta performed 369 iterations in 5.385172 mins.
# 8 attributes confirmed important: apc5, atrRatSlow, ATRSlow, bWidthSlow,# deltaMMIFastest10 and 3 more;
# 7 attributes confirmed unimportant: bWdith3, deltaATRrat10, deltaATRrat3, deltaPVR5,
# HurstFast and 2 more;

## PCA
start.time <- Sys.time()
pca.model <- train(vars_filt[, -ncol(vars_filt)], vars_filt[, ncol(vars_filt)], method = 'rf', preProcess = c('pca'),
                   trControl = cntrl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Time difference of 35.44463 mins

start.time1 <- Sys.time()
raw.model <- train(vars_filt[, -ncol(vars_filt)], vars_filt[, ncol(vars_filt)], method = 'rf',
                   trControl = cntrl)
end.time1 <- Sys.time()
time.taken1 <- end.time1 - start.time1
time.taken1

# compare models
resamp.results <- resamples(list(PCA = pca.model,
                                 RAW = raw.model))

trellis.par.set(theme = col.whitebg())
bwplot(resamp.results, layout = c(1, 1))

## GBM model
gbm.data <- vars_filt[, c("target", "deltaMMIFastest10", "trend", "atrRatSlow")]   

# create indexes for TSCV windows
init = 200 #initial window 
horiz = 20 #prediction horizon 
wdw <- createTimeSlices(1:nrow(gbm.data), initialWindow = init, horizon = horiz, 
                        skip = horiz-1, fixedWindow = TRUE)
trainSlices <- wdw[[1]]
testSlices <- wdw[[2]]

# verify visually correct window setup:
trainSlices[[length(trainSlices)]]
testSlices[[length(testSlices)]]

# trainControl function
cntrl <- trainControl(summaryFunction=absretSummary, savePredictions=T, returnResamp="final", 
                      index=trainSlices, indexOut=testSlices) 

# train gbm model            
set.seed(53)
gbm.model <- train(x=gbm.data[, -1], y=gbm.data[, 1], method="gbm", trControl=cntrl, metric="profit", 
                   maximize=TRUE, tuneGrid = expand.grid(interaction.depth = 3,
                                             n.trees = 1000,
                                             shrinkage = 1,
                                             n.minobsinnode = 10))

# extract predictions and observations from each test window
gbm.preds <- gbm.model$pred[, c("pred", "obs")]

# generate trades and PnLs for various prediction thresholds
i <- 1
trades <- list()
PnL <- list()
for(thresh in c(0, 20, 50, 100, 125, 150)) {
  gbm.trades <- ifelse(gbm.preds$pred > thresh, gbm.preds$obs, 
                       ifelse(gbm.preds$pred < -thresh, -gbm.preds$obs, 0))
  gbm.PnL <- cumsum(gbm.trades)
  
  trades[[i]] <- gbm.trades
  PnL[[i]] <- gbm.PnL
  i <- i+1
}

# Plot the equity curves
require(ggplot2)
require(reshape)
PnL.df <- as.data.frame(PnL)
colnames(PnL.df) <- c("T0", "T20", "T50", "T100", "T125", "T150")

# calculate and subtract transaction costs
lots <- 1/(1000*vars$ATRMod[ (init+1):testSlices[[length(testSlices)]][horiz] ])
cost <- lots*20
PnL.df <- PnL.df - cost

# add date for plotting
PnL.df$Date <- as.Date(as.character(rownames(gbm.data[(init+1):testSlices[[length(testSlices)]][horiz], ])), format="%d.%m.%Y")

# add buy and hold PnL curve
PnL.df$BuyHold <- cumsum(gbm.data[(init+1):testSlices[[length(testSlices)]][horiz], "target" ])

# melt data to long form
mlt <- melt(PnL.df, id="Date", value="PnL", variable_name="Threshold")

# plot PnLs for various thresholds and buy & hold
ggplot(mlt, aes(x=Date, y=value)) +
  geom_line(aes(colour=Threshold, group=Threshold)) +
  ylab("PnL excl. Costs")

### Notes on caret ts cross validation:
# We can set up TSCV using timeSlices and initial window, horizon and skip to mimic WFO. 
# If we do hyperparameter tuning, savePreds="all" returns all predictions from each test window 
# for each hyperparameter combination. To get an assessment of how it would have peformed in 
# trading, set up the same train function but with the hyperparameters set to the best identified 
# in TSCV. Then savePreds="all" returns only the test window results - from there you can perform 
# other analysis. Also note that caret fits a model on the entire data set after cross-validation 
# - this is generally not what we want to do in this context. 

