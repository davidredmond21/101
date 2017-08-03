# This assumes the bearing files from Appendix 1 are in the C:\ directory.
library(moments)
library(robustbase)

source("common.r")
dataset <- 1
properties <- dataset_properties(dataset)

dataset_count <- properties[2]

data_path <- results_path(dataset)

b1 <- read.csv(file.path(data_path, "channel_1.txt"))
b2 <- read.csv(file.path(data_path, "channel_2.txt"))
b3 <- read.csv(file.path(data_path, "channel_3.txt"))

b1 <- b1[,1:4]
b2 <- b2[,1:4]
b3 <- b3[,1:4]

col1 <- c(b1[,1], b2[,1], b3[,1])
col2 <- c(b1[,2], b2[,2], b3[,2])
col3 <- c(b1[,3], b2[,3], b3[,3])
col4 <- c(b1[,4], b2[,4], b3[,4])
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
b2Norm <- cbind(col1Norm[(dataset_count+1):(2*dataset_count)],col2Norm[(dataset_count+1):(2*dataset_count)],col3Norm[(dataset_count+1):(2*dataset_count)],col4Norm[(dataset_count+1):(2*dataset_count)])
b3Norm <- cbind(col1Norm[(2*dataset_count+1):(3*dataset_count)],col2Norm[(2*dataset_count+1):(3*dataset_count)],col3Norm[(2*dataset_count+1):(3*dataset_count)],col4Norm[(2*dataset_count+1):(3*dataset_count)])

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Euclidean <- sqrt(sum((b1Norm[x,] - meansNorm)^2))
    Bearing2Euclidean <- sqrt(sum((b2Norm[x,] - meansNorm)^2))
    Bearing3Euclidean <- sqrt(sum((b3Norm[x,] - meansNorm)^2))
    df[x,] <- c(Bearing1Euclidean, Bearing2Euclidean, Bearing3Euclidean)
}
write.csv(df, file.path(data_path, "Euclidean1.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Manhattan <- sum(abs(b1Norm[x,] - meansNorm))
    Bearing2Manhattan <- sum(abs(b2Norm[x,] - meansNorm))
    Bearing3Manhattan <- sum(abs(b3Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Manhattan, Bearing2Manhattan, Bearing3Manhattan)
}
write.csv(df, file.path(data_path, "Manhattan1.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Chebyshev <- max(abs(b1Norm[x,] - meansNorm))
    Bearing2Chebyshev <- max(abs(b2Norm[x,] - meansNorm))
    Bearing3Chebyshev <- max(abs(b3Norm[x,] - meansNorm))
    df[x,] <- c(Bearing1Chebyshev,"\t", Bearing2Chebyshev,"\t", Bearing3Chebyshev,"\n")
}
write.csv(df, file.path(data_path, "Chebyshev1.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
for (x in 1:dataset_count) {
    Bearing1Penrose <- sum(((b1[x,] - means)^2/(var*length(var))))
    Bearing2Penrose <- sum(((b2[x,] - means)^2/(var*length(var))))
    Bearing3Penrose <- sum(((b3[x,] - means)^2/(var*length(var))))
    df[x,] <- c(Bearing1Penrose, Bearing2Penrose, Bearing3Penrose)
}
write.csv(df, file.path(data_path, "Penrose1.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
Bearing1Mahalanobis <- mahalanobis(b1,means,Sx)
Bearing2Mahalanobis <- mahalanobis(b2,means,Sx)
Bearing3Mahalanobis <- mahalanobis(b3,means,Sx)
toPrint <- cbind(Bearing1Mahalanobis,Bearing2Mahalanobis,Bearing3Mahalanobis)
for (i in 1:dataset_count) {
    df[i,] <- c(toPrint[i,1], toPrint[i,2], toPrint[i,3])
}
write.csv(df, file.path(data_path, "Mahalanobis1.txt"), row.names=FALSE)

df <- data.frame(V1=rep(NA, dataset_count), V2=rep(NA, dataset_count), V3=rep(NA, dataset_count))
Bearing1MCD <- res$mah[1:dataset_count]
Bearing2MCD <- res$mah[dataset_count + 1:2 * dataset_count]
Bearing3MCD <- res$mah[2 * dataset_count + 1:3 * dataset_count]
toPrint2 <- cbind(Bearing1MCD,Bearing2MCD,Bearing3MCD)
for (i in 1:dataset_count) {
    df[i,] <- c(toPrint2[i,1], toPrint2[i,2], toPrint2[i,3])
}
write.csv(df, file.path(data_path, "RobustMahalanobis1.txt"), row.names=FALSE)

