#Ex1
library(readr)
concrete <- read_csv("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP7/concrete.csv")
View(concrete)
str(concrete)
head(concrete)
tail(concrete)
#Tamanho
dim(concrete)
#Sumario
summary(concrete)

hist(concrete$strength, probability = TRUE, main="Resistência betão")
lines(density(concrete$strength), col = "Red", lwd = 2)

boxplot(concrete[,1:8])
plot(concrete$age, concrete$strength, pch=16, main="Resistência vs Idade")

correlation <- round(cor(concrete)[9,1:8],digits=3)

correlation[order(correlation,decreasing = TRUE), drop=FALSE]

library(PerformanceAnalytics)

chart.Correlation(concrete[,c(1,2,5,8,9)], histogram = TRUE, method = "pearson")
chart.Correlation(concrete[,c(3,4,6,7,9)], histogram = TRUE, method = "pearson")

# Normalizar dados
minmaxnomr <- function(y){
  miny <- min(y)
  maxy <- max(y)
  return ((y - miny)/(maxy - miny))
}

concrete.norm <- as.data.frame(apply(concrete,2,minmaxnomr))
apply(concrete.norm,2,min)
apply(concrete.norm,2,max)

# Desnormalizar dados
minmaxdesnor <- function(y, goal.attrib) {
  return (y*(max(goal.attrib)-min(goal.attrib)) + min(goal.attrib))
}

# Verificar desnomalizacao
strength <- minmaxdesnor(concrete.norm$strength,concrete$strength)
summary(strength)
summary(concrete$strength)

#Ex4

set.seed(123)

index <- sample(1:nrow(concrete.norm), 0.7*nrow(concrete.norm))

data.train <- concrete.norm[index,]
data.tst <- concrete.norm[-index,]

summary(data.train$strength)
summary(data.tst$strength)

RMSE <- function(test, predicted){
  sqrt(mean((test - predicted)^2))
}

# Ex5
library(neuralnet)

set.seed(739)

# Rede com 1 nivel no interno 
numnodes <- 1
nnet <- neuralnet(strength ~ slag + ash + water + superplastic + coarseagg + fineagg + age,
                  data = data.train,
                  hidden=numnodes)

names(nnet)

nnet$weights

nnet$result.matrix

nnet$net.result

plot(nnet)

nn.pred <- compute(nnet, data.tst[,1:8])

nn.pred.strength <- minmaxdesnor(nn.pred$net.result,concrete$strength)
test.strength <- minmaxdesnor(data.tst$strength,concrete$strength)

RMSE(nn.pred.strength, test.strength)
summary(test.strength)

#Ex f


# Rede com 3 nivel no interno 
numnodes <- 3
nnet <- neuralnet(strength ~ slag + ash + water + superplastic + coarseagg + fineagg + age,
                  data = data.train,
                  hidden=numnodes)

names(nnet)

nnet$weights

nnet$result.matrix

nnet$net.result

plot(nnet)

nn.pred <- compute(nnet, data.tst[,1:8])

nn.pred.strength <- minmaxdesnor(nn.pred$net.result,concrete$strength)
test.strength <- minmaxdesnor(data.tst$strength,concrete$strength)

RMSE(nn.pred.strength, test.strength)
summary(test.strength)

