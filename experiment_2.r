library(moments)
library(robustbase)

source("common.r")
dataset <- 2
properties <- dataset_properties(dataset)

dataset_count <- properties[2]

data_path <- results_path(dataset)

b1 <- read.csv(file.path(data_path, "channel_1.txt"))
b2 <- read.csv(file.path(data_path, "channel_2.txt"))
b3 <- read.csv(file.path(data_path, "channel_3.txt"))

b1 <- b1[,1:4]
b2 <- b2[,1:4]
b3 <- b3[,1:4]

col1 <- c(b2[,1])
col2 <- c(b2[,2])
col3 <- c(b2[,3])
col4 <- c(b2[,4])
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
col1 <- c(b1[,1], b2[,1], b3[,1])
col2 <- c(b1[,2], b2[,2], b3[,2])
col3 <- c(b1[,3], b2[,3], b3[,3])
col4 <- c(b1[,4], b2[,4], b3[,4])
col1Norm <- ((col1-min(col1))/(max(col1)-min(col1)))
col2Norm <- ((col2-min(col2))/(max(col2)-min(col2)))
col3Norm <- ((col3-min(col3))/(max(col3)-min(col3)))
col4Norm <- ((col4-min(col4))/(max(col4)-min(col4)))
b1Norm <- cbind(col1Norm[1:984],col2Norm[1:984],col3Norm[1:984],col4Norm[1:984])
b2Norm <- cbind(col1Norm[985:1968],col2Norm[985:1968],col3Norm[985:1968],col4Norm[985:1968])
b3Norm <- cbind(col1Norm[1969:2952],col2Norm[1969:2952],col3Norm[1969:2952],col4Norm[1969:2952])

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Euclidean <- sqrt(sum((b1Norm[x,] - meansNorm)^2))
    Bearing2Euclidean <- sqrt(sum((b2Norm[x,] - meansNorm)^2))
    Bearing3Euclidean <- sqrt(sum((b3Norm[x,] - meansNorm)^2))
    df[x,] <- c(Bearing1Euclidean, Bearing2Euclidean, Bearing3Euclidean)
}
write.csv(df, file.path(data_path, "Euclidean2.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Manhattan <- sum(abs(b1Norm[x,] - meansNorm))
    Bearing2Manhattan <- sum(abs(b2Norm[x,] - meansNorm))
    Bearing3Manhattan <- sum(abs(b3Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Manhattan, Bearing2Manhattan, Bearing3Manhattan)
}
write.csv(df, file.path(data_path, "Manhattan2.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Chebyshev <- max(abs(b1Norm[x,] - meansNorm))
    Bearing2Chebyshev <- max(abs(b2Norm[x,] - meansNorm))
    Bearing3Chebyshev <- max(abs(b3Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Chebyshev, Bearing2Chebyshev, Bearing3Chebyshev)
}
write.csv(df, file.path(data_path, "Chebyshev2.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Penrose <- sum(((b1[x,] - means)^2/(var*length(var))))
    Bearing2Penrose <- sum(((b2[x,] - means)^2/(var*length(var))))
    Bearing3Penrose <- sum(((b3[x,] - means)^2/(var*length(var))))
    df[x,] <- c(Bearing1Penrose, Bearing2Penrose, Bearing3Penrose)
}
write.csv(df, file.path(data_path, "Penrose2.txt"), row.names=FALSE)

Bearing1Mahalanobis <- mahalanobis(b1,means,Sx)
Bearing2Mahalanobis <- mahalanobis(b2,means,Sx)
Bearing3Mahalanobis <- mahalanobis(b3,means,Sx)
toPrint <- cbind(Bearing1Mahalanobis,Bearing2Mahalanobis,Bearing3Mahalanobis)
df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (i in 1:dataset_count) {
    df[i,] <- c(toPrint[i,1], toPrint[i,2], toPrint[i,3])
}
write.csv(df, file.path(data_path, "Mahalanobis2.txt"), row.names=FALSE)

Bearing1MCD <- mahalanobis(b1, res$center, res$cov)
Bearing2MCD <- mahalanobis(b2, res$center, res$cov)
Bearing3MCD <- mahalanobis(b3, res$center, res$cov)
toPrint2 <- cbind(Bearing1MCD,Bearing2MCD,Bearing3MCD)
df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (i in 1:dataset_count) {
    df[i,] <- c(toPrint2[i,1], toPrint2[i,2], toPrint2[i,3])
}
write.csv(df, file.path(data_path, "RobustMahalanobis2.txt"), row.names=FALSE)
