library(moments)
library(robustbase)

source("common.r")
dataset <- 1
properties <- dataset_properties(dataset)

dataset_count <- properties[2]

data_path <- results_path(dataset)

b1 <- read.csv(file.path(data_path, "channel_8.txt"))
b1 <- b1[,1:4]

col1 <- c(b1[,1])
col2 <- c(b1[,2])
col3 <- c(b1[,3])
col4 <- c(b1[,4])
dataset <- cbind(col1,col2,col3,col4)

col1Norm <- ((col1-min(col1))/(max(col1)-min(col1)))
col2Norm <- ((col2-min(col2))/(max(col2)-min(col2)))
col3Norm <- ((col3-min(col3))/(max(col3)-min(col3)))
col4Norm <- ((col4-min(col4))/(max(col4)-min(col4)))
meansNorm <- c(median(col1Norm), median(col2Norm), median(col3Norm), median(col4Norm))
means <- c(median(col1), median(col2), median(col3), median(col4))
Sx <- cov(dataset)
res <- covMcd(x = dataset)
var <- c(var(col1), var(col2), var(col3), var(col4))
b1Norm <- cbind(col1Norm[1:dataset_count],col2Norm[1:dataset_count],col3Norm[1:dataset_count],col4Norm[1:dataset_count])

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Euclidean <- sqrt(sum((b1Norm[x,] - meansNorm)^2))
    df[x,] <- c(Bearing1Euclidean)
}
write.csv(df, file.path(data_path, "Euclidean3.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Manhattan <- sum(abs(b1Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Manhattan)
}
write.csv(df, file.path(data_path, "Manhattan3.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Chebyshev <- max(abs(b1Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Chebyshev)
}
write.csv(df, file.path(data_path, "Chebyshev3.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Penrose <- sum(((b1[x,] - means)^2/(var*length(var))))
    df[x,] <- c(Bearing1Penrose)
}
write.csv(df, file.path(data_path, "Penrose3.txt"), row.names=FALSE)

Bearing1Mahalanobis <- mahalanobis(b1,means,Sx)
toPrint <- cbind(Bearing1Mahalanobis)
df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (i in 1:dataset_count) {
    df[i,] <- c(toPrint[i,1])
}
write.csv(df, file.path(data_path, "Mahalanobis3.txt"), row.names=FALSE)

Bearing1MCD <- res$mah[1:dataset_count]
toPrint2 <- cbind(Bearing1MCD)
df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (i in 1:dataset_count) {
    df[i,] <- c(toPrint2[i,1])
}
write.csv(df, file.path(data_path, "RobustMahalanobis3.txt"), row.names=FALSE)

