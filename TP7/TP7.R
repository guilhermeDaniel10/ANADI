library(readr)

#a
concrete <- read_csv("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP7/concrete.csv")
dim(concrete)
summary(concrete)
str(concrete)
head(concrete)

#b
hist(concrete$strength, probability = TRUE, main="Resistência betão")
lines(density(concrete$strength), col="Red", lwd=2)

#c
boxplot(concrete[,1:8])

plot(concrete$age, concrete$strength, pch=16, main="Resistencia vs Idade")

#Correlação previsores c/ Strength

correlations <- round(cor(concrete)[9,1:8], digits=3)

correlations[order(correlations,decreasing=TRUE), drop=FALSE]

library(PerformanceAnalytics)

chart.Correlation(concrete[,c(1,2,5,8,9)], histogram=TRUE, method="pearson")
chart.Correlation(concrete[,c(3,4,6,7,9)], histogram=TRUE, method="pearson")


#c)


normMinimax <- function(y){
  ((y - min(y)) / (max(y) - min(y)))
}

desnormMinimax <- function(y, goalAttribute){
  y * (max(goalAttribute)-min(goalAttribute)) + min(goalAttribute)
}

concrete.norm <- as.data.frame(apply(concrete, 2, normMinimax))
apply(concrete.norm, 2, min)
apply(concrete.norm, 2, max)

#Verif. desnormaliz.
strength <- desnormMinimax(concrete.norm$strength, concrete$strength)

set.seed(123)
index <- sample(1:nrow(concrete.norm), 0.7*nrow(concrete.norm))            

data.train <- concrete.norm[index, ]
data.tst <- concrete.norm[-index, ]

RMSE <- function(test, predicted){
  sqrt(mean((test - predicted)^2))
}

#-------------------------------------
library(neuralnet)
library(rpart)
numnodes <- 5

set.seed(1)

nnet <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                  data = data.train,
                  hidden=numnodes)

names(nnet)
nnet$weights
nnet$result.matrix
nnet$net.result
plot(nnet)

nn.pred <- compute(nnet, data.tst[,1:8])

nn.pred.strength <- desnormMinimax(nn.pred$net.result, concrete$strength)
test.strength <- desnormMinimax(data.tst$strength, concrete$strength)

RMSE(nn.pred.strength, test.strength)
summary(test.strength)


#------------------------------------
library(rpart)
library(neuralnet)
library(plyr)
pbar <- create_progress_bar('text')
pbar$init(k)

set.seed(278)
k <- 10
folds <- sample(1:k, nrow(concrete), replace = TRUE)

cv.error <- matrix(nrow = k, ncol = 3)
numnodes <- 5

for (i in 1:k){
  ntrain.cv <- concrete.norm[folds != i, ]
  ntest.cv <- concrete.norm[folds == i, ]
  
  nnet <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                    data = data.train,
                    hidden=numnodes)
  
  nn.pred <- compute(nnet, ntest.cv[, 1:8])
  nn.pred <- desnormMinimax(nn.pred$net.result, concrete$strength)
  
  train.cv <- concrete[folds != i, ]
  test.cv <- concrete[folds == i, ]
  
  mlr.model <- lm(strength ~., data = train.cv)
  mlr.pred <- predict(mlr.model, test.cv)
  
  rpart.model <- rpart(strength ~., data = train.cv)
  rpart.pred <- predict(mlr.model, test.cv)
  
  cv.error[i, ] <- c(RMSE(nn.pred, test.cv$strength),
                     RMSE(mlr.pred, test.cv$strength),
                     RMSE(rpart.pred, test.cv$strength))
  
  
  
  pbar$step()
}

colnames(cv.error) <- c('nnet', 'mlr', 'rpart')
cv.error

apply(cv.error, 2, mean)
apply(cv.error, 2, sd)

# ----------------------------------- EX 8 ----------------------------------

diff <- cv.error[,1] ~ cv.error[,3]

shapiro.test(diff)
# Podemos assumir a normalidade porque p-value = 0.919 > 0.05

# paired t-test:
t.test(cv.error[,1], cv.error[,3], paired=TRUE,alternative="two.sided")