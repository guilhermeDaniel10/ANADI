library(readr)
abalone_data <- read_csv("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP8/abalone.data.txt")

dim(abalone_data)
spec(abalone_data)
str(abalone_data)
head(abalone_data)

summary(abalone_data)

# 1
# Data exploration

# Rings correlation
round(cor(abalone_data[,2:9])[8,1:7],digits=3)

library(PerformanceAnalytics)
chart.Correlation(abalone_data[,1],as.numeric)

sumHeight <- apply(abalone_data[,c(6:8)])


abalone_data$Sex[, c('M', 'F', 'I')] <- sapply(abalone_data$Sex[, c('M', 'F','I')], unclass)
