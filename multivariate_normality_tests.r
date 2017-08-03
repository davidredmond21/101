
library(MVN)

source("common.r")
dataset <- 2

b1 <- read.csv(file.path(results_path(dataset), "bearing1.txt"), header=TRUE)
b1 <- b1[,1:4]

mardiaTest(b1, qqplot=TRUE)
hzTest(b1)
roystonTest(b1)
