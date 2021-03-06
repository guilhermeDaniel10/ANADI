library(datarium)
library(rpart)
library(rpart.plot)
# Arvore decisao
# Regressao- atributos objetivos continuos
# classificacao: atributos objetivos discretos: 1- binaria (2 classes) 2- multi class (> 2)

# Exercicio 1
# Alinea a
data("BreastCancer", package = "mlbench")

dim(BreastCancer)

#estrutura interna dos dados
str(BreastCancer)

#Primeiros elementos dos dados
head(BreastCancer)

#ultimos elemento dos dados
tail(BreastCancer)

summary(BreastCancer)

# eliminar a primeira coluna
BreastCancer <- BreastCancer[, -1]

#fazer com que o dados n�o numericos sejam categoricos
BreastCancer[,1:9] <- sapply(BreastCancer[, 1:9], as.numeric)

str(BreastCancer)

summary(BreastCancer)

BreastCancer <- na.omit(BreastCancer)

# alinea b
boxplot(BreastCancer[,1:9])

par(mfrow=c(3,3))

for(i in 1:9){
  boxplot(BreastCancer[,i]~Class, data=BreastCancer,main=names(BreastCancer[i]))
}


BreastCancer$Class <- factor(BreastCancer$Class,
                             levels=c("malignant", "benign"),
                             labels("M", "B"))

# 11 � maligno
AbsFreq <- table(BreastCancer$Class)
PropFreq <- prop.table(table(BreastCancer$Class))*100
cbind(AbsFreq,PropFreq)

# alinea C

# Metodo holdout
# Treino: 70%
# Teste: 30%

# Antes de usar funcao aleatoria, � necess�rio definir uma seed
# Sem seed, era sempre gerado um index diferente
set.seed(1)
index <- sample(1:nrow(BreastCancer),0.7*nrow(BreastCancer))

data.train <- BreastCancer[index,]
data.tst <- BreastCancer[-index,]

# True positive � a classe de interesse
# True negative � a classe que n�o tem interesse
# previs�es erradas d�o falso negativo na classe de n�o interesse
# previs�es erradas d�o falso positivo na classe de interesse

# Taxa de acerto (accuracy) = (True positive + True negative)/(True positive + false negative + false positive + true negative)
# Taxa de erro = 1 - Taxa de acerto 

# Precision = True Positive / True positive + false positive -> d� a percetagem de previsoes corretas
# Recall = True positive / True positive + false negative -> quantidade de celulas malignas acertadas

# Precision e Recall s�o competitivas

#Recall tem que ser maximizado caso seja maligno e for detetado benigno (false negative)
# Matriz de confus�o

#         | ^maligno | ^benigno
#_________|__________|____________
#maligno  |    TP    |    FN
#_________|__________|____________
#benigno  |    FP    |    TN

# F1= (2*precision*recall)/precision + recall


rpart.model <- rpart(Class~.,data=data.train)
rpart.plot(rpart.model,digits=3)
rpart.pred <- predict(rpart.model,data.tst,type='class')

#matriz de confusao
cfmatrix <- table(data.tst$Class,rpart.pred)

#         | ^maligno | ^benigno
#_________|__________|____________
#maligno  |    TP    |    FN
#_________|__________|____________
#benigno  |    FP    |    TN
# Taxa de acerto (accuracy) = (True positive + True negative)/(True positive + false negative + false positive + true negative)
accuracy <- function(cfM) {
  sum(diag(cfM))/sum(cfM)
}

accuracy(cfmatrix)

rmse <- function(test, predict) {
  sqrt(mean((test - predict) ^ 2))
}

