library(ggplot2)

wine <- read.csv("wine.csv")

# Create dataframe containing frequencies of each quality rating
quality_freq <- data.frame(seq(1, 10, 1), tabulate(wine$quality, nbins=10))
colnames(quality_freq) <- c("quality", "freq")

# Doing a sneaky bar plot, but making it look like a histogram :/
barplot(quality_freq$freq, 
        main="Wine Quality Distribution",
        xlab="Quality Score",
        ylab="Frequency",
        names.arg=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        ylim=c(0, 700),
        space = 0)

# Create subset of only acid, sugar and quality columns
salt_sugar_quality <- wine[c("chlorides","residual_sugar", "quality")]

# Create new column, calculate ratio of acid and sugar
# Fixed acidity and residual sugar are measured in mg/L and g/L, so converted between those
salt_sugar_quality <- transform(salt_sugar_quality, ratio = chlorides / residual_sugar)

# Check normality of acid and sugar concentrations
shapiro.test(wine$chlorides)
shapiro.test(wine$residual_sugar)
hist(wine$chlorides)
hist(wine$residual_sugar)

# Data is not normally distributed, so perform a spiderman
cor.test(salt_sugar_quality$ratio, salt_sugar_quality$quality, method = "spearman")

# Plot just so I can maintain some remnant of sanity
plot(salt_sugar_quality$ratio, salt_sugar_quality$quality)







