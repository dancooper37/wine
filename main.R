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
# To account for extra dimension of division, log transform
salt_sugar_quality <- transform(salt_sugar_quality, ratio = log10(chlorides / residual_sugar))

# salt_sugar_quality$quality <- as.factor(salt_sugar_quality$quality)



# Check normality of acid and sugar concentrations
shapiro.test(wine$chlorides)
shapiro.test(wine$residual_sugar)
hist(wine$chlorides)
hist(wine$residual_sugar)

shapiro.test(salt_sugar_quality$ratio)
hist(salt_sugar_quality$ratio)

# Data is not normally distributed, so perform a spiderman
cor.test(salt_sugar_quality$ratio, salt_sugar_quality$quality, method = "spearman")
cor.test(salt_sugar_quality$chlorides, salt_sugar_quality$quality, method = "spearman")
cor.test(salt_sugar_quality$residual_sugar, salt_sugar_quality$quality, method = "spearman")


# Box plot of log transformed salt/sugar ratio against quality
ggplot(salt_sugar_quality, aes(x=quality, y=chlorides)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Percieved Quality") +
  ylab("Chloride Ions (g/L)")

summary(salt_sugar_quality$chlorides)

ggplot(salt_sugar_quality, aes(x=quality, y=residual_sugar)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Percieved Quality") +
  ylab("Residual Sugar (g/L)")

summary(salt_sugar_quality$residual_sugar)

ggplot(salt_sugar_quality, aes(x=quality, y=ratio)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Percieved Quality") +
  ylab("Log(Chloride Ion : Sugar)")

summary(salt_sugar_quality$ratio)

#Subset data into above or below 0
salt_sugar_quality$mix <- ifelse(salt_sugar_quality$quality > 5, "ABOVE", "BELOW")

ggplot(salt_sugar_quality, aes(x=mix, y=ratio)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Percieved Quality") +
  ylab("Log(Chloride Ion : Sugar)")

# Mann-Whitney U test between high and low ratios of chlorides and sugar
wilcox.test(salt_sugar_quality$ratio~salt_sugar_quality$mix)

plot(salt_sugar_quality$chlorides, salt_sugar_quality$residual_sugar)

ggplot(data = salt_sugar_quality, aes(residual_sugar, chlorides, color = quality)) +
  geom_point(size=1) +
  labs(x = "Residual Sugar (g/L)", y = "Chlorides (g/L)", color = "Quality") +
  scale_color_viridis_b() +
  theme_minimal()
