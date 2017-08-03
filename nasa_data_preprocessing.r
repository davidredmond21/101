
library(moments)

source("common.r")
dataset <- 3
properties <- dataset_properties(dataset)

channel_count <- properties[1]

data_path <- dataset_path(dataset)

infiles <- list.files(data_path, pattern="*.*", full.names=TRUE)

RMS <- function(b) {
    sqrt(sum(b^2)/length(b))
}

processthe.files <- function(file, i) {
    data <- read.table(file, header=FALSE, sep="\t", row.names=NULL)
    bearing <- as.vector(data[,i])
    RMSVal <- RMS(bearing)
    CF <- if (abs(max(bearing)) >= abs(min(bearing))) {
      (abs(max(bearing))/RMSVal)
    }
    else {
      (abs(min(bearing))/RMSVal)
    }
    cbind(skewness(bearing), kurtosis(bearing), RMSVal, CF)
}

dataset_count <- length(infiles)

for (channel in 1:channel_count) {

    df <- data.frame(kurt=rep(NA, dataset_count), skew=rep(NA, dataset_count), rms=rep(NA, dataset_count), cf=rep(NA, dataset_count))
    for (i in 1:dataset_count) {
        result <- processthe.files(infiles[i], channel)
        df[i,] <- result
    }
    
    write.csv(df, file.path(results_path(dataset), paste("channel_", channel, ".txt", sep='')), row.names=FALSE)
}
    
