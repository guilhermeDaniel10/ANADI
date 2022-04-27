library(datarium)
library(rpart)
library(rpart.plot)

data("marketing", package="datarium")
#dimensao dos dados
dim(marketing)

#estrutura interna dos dados
str(marketing)

#Primeiros elementos dos dados
head(marketing)

#ultimos elemento dos dados
tail(marketing)

summary(marketing)

boxplot(marketing[,1:3])

# Histogramas
par(mfrow=c(1,3))
hist(marketing$youtube,probability = TRUE, main="youtube")
hist(marketing$facebook,probability = TRUE, main="facebook")
hist(marketing$newspaper,probability = TRUE, main="newspaper")

# Plot
par(mfrow=c(1,3))
plot(marketing$youtube,marketing$sales,ylab="sales")
plot(marketing$facebook,marketing$sales,ylab="sales")
plot(marketing$newspaper,marketing$sales,ylab="sales")

#correlacao entre sales e as variaveis 
cor(marketing)

# alinea C

# Metodo holdout
# Treino: 70%
# Teste: 30%

# Antes de usar funcao aleatoria, é necessário definir uma seed
# Sem seed, era sempre gerado um index diferente
set.seed(5)
index <- sample(1:nrow(marketing),0.7*nrow(marketing))

data.train <- marketing[index,]
data.tst <- marketing[-index,]

# alinea d
# i

#equacao de regressao
slr.model <- lm(sales~youtube,data=data.train)

# ii
par(mfrow=c(1,1))
plot(data.train$youtube,data.train$sales)
abline(slr.model,col="red")

# Mean absolute error- modelos de regressao
# MAE = (1/n)Som(n,i=1)|yi - ybari|

#Mean square error - sempre maior que MAE
# MSE = (1/n)Som(n,i=1)(yi - ybari)^2

# RMSE = sqrt(MSE)
# RMSE > MAE  

mae <- function(test, predict) {
  mean(abs(test - predict))
}

rmse <- function(test, predict) {
  sqrt(mean((test - predict) ^ 2))
}

# Modelo de regressao linear multipla

# cada coeficiente é como se os parametros sem ser o atual fossem 0 
mlr.model <- lm(sales ~ youtube + facebook + newspaper, data=data.train)

mlr.model



summary(mlr.model)
# Residuals: 10400 dolares acima 
# Residuals: 34225 abaixo 
# Coeficientes- t value- testa se existe uma relação significativa entre os previsores e atribuo objetivo
# Newspaper não é relevante por ser negativo
#R squared- a explicação das vendas é explicada 88%- é um bom modelo
# em modelos de data mining quer-se sempre precisões (R squared) a cima de 80 %
# quando um modelo acerta muito sobre o conjunto de treino, ele pode estar a ajustar-se aos dados

#Adjusted R squared- penalização do r quadrado para penalizar regressao com variaveis promissores

mlr.pred <- predict(mlr.model,data.tst)

mae(mlr.pred,data.tst$sales)
rmse(mlr.pred,data.tst$sales)
# RMSE é sempre superior ao MAE

summary(data.tst$sales)

# alinea g
# asterisco são os nós folha
rpart.model <- rpart(sales~., data=data.train)
#Vendas em cima da percetagem
#quanto maior a venda, maior a percentagem
rpart.plot(rpart.model,digits=3)

rpart.pred <- predict(rpart.model, data.tst)
summary(rpart.pred)
summary(data.tst$sales)

cor(rpart.pred, data.tst$sales)

mae(rpart.pred, data.tst$sales)
rmse(rpart.pred, data.tst$sales)

# ml: MAE = 1.495 RMSE = 1.876
# rpart: MAR = 1.638 RMSE = 2.028