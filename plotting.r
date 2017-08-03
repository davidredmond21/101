
# Plot the various metrics calculated in experimetns 1-3
# Not part of the example code form the book

library(ggplot2)
library(reshape)

source("common.r")
dataset <- 1
properties <- dataset_properties(dataset)

dataset_count <- properties[2]

data_path <- results_path(dataset)

plot.data <- function(filename, title, colcount=3) {
    df <- read.csv(filename)
    df$idx <- as.integer(row.names(df))
    #df <- df[651:984,]
    #df <- df[3500:4448,]
    colnames(df) <- c("Bearing 1", "Bearing 2", "Bearing 3", "idx")
    if (colcount == 3) {
        melted <- melt(df, id.vars="idx")
        plt <- ggplot(melted, aes(x=idx, y=value, group=variable, colour=variable))
    }
    else {
        plt <- ggplot(df, aes(x=idx, y=`Bearing 1`, colour=`Bearing 1`))
    }
    
    plt <- plt + geom_line()
    plt <- plt + theme_classic()
    plt <- plt + xlab("Time") + ylab(title)
    plt
}

# Note: bearing 3 is not shown in the book
# (very hard to see, but otherwise it doesn't makes sense)

# Experiment 1
#plot.data(file.path(data_path, "Euclidean1.txt"), "Euclidean")
#plot.data(file.path(data_path, "Manhattan1.txt"), "Manhattan")
#plot.data(file.path(data_path, "Chebyshev1.txt"), "Chebyshev")
#plot.data(file.path(data_path, "Penrose1.txt"), "Penrose")
#plot.data(file.path(data_path, "Mahalanobis1.txt"), "Mahalanobis")
#plot.data(file.path(data_path, "RobustMahalanobis1.txt"), "Robust Mahalanobis")

# Experiment 2
#plot.data(file.path(data_path, "Euclidean2.txt"), "Euclidean")
#plot.data(file.path(data_path, "Manhattan2.txt"), "Manhattan")
#plot.data(file.path(data_path, "Chebyshev2.txt"), "Chebyshev")
#plot.data(file.path(data_path, "Penrose2.txt"), "Penrose")
#plot.data(file.path(data_path, "Mahalanobis2.txt"), "Mahalanobis")
#plot.data(file.path(data_path, "RobustMahalanobis2.txt"), "Robust Mahalanobis")

# Experiment 3
#plot.data(file.path(data_path, "Euclidean3.txt"), "Euclidean", colcount=1)
#plot.data(file.path(data_path, "Manhattan3.txt"), "Manhattan", colcount=1)
#plot.data(file.path(data_path, "Chebyshev3.txt"), "Chebyshev", colcount=1)
#plot.data(file.path(data_path, "Penrose3.txt"), "Penrose", colcount=1)
#plot.data(file.path(data_path, "Mahalanobis3.txt"), "Mahalanobis", colcount=1)
plot.data(file.path(data_path, "RobustMahalanobis3.txt"), "Robust Mahalanobis", colcount=1)

