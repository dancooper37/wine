library(ggplot2)

wine <- read.csv("wine.csv")

# Create dataframe containing frequencies of each quality rating
quality_freq <- data.frame(seq(1, 10, 1), tabulate(wine$quality, nbins=10))
colnames(quality_freq) <- c("Quality", "Frequency")

# Doing a sneaky bar plot, but making it look like a histogram :/
barplot(quality_freq$Frequency, 
        main="Wine Quality Distribution",
        xlab="Quality Score",
        ylab="Frequency",
        names.arg=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        ylim=c(0, 700),
        space = 0)

