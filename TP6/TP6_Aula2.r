library(rpart.plot)
library(rpart)
data('BreastCancer',package='mlbench')
dim(BreastCancer)
summary(BreastCancer)
str(BreastCancer)
head(BreastCancer)
#View(BreastCancer)
BreastCancer <- BreastCancer[ ,-1]
#Apagar a primeira coluna (Identifier)

BreastCancer[,1:9] <- sapply(BreastCancer[,1:9],as.numeric)
#Tornar os dados das colunas 1 a 10 em numéricos

str(BreastCancer)
summary(BreastCancer)

BreastCancer <- na.omit(BreastCancer)

boxplot(BreastCancer[,1:10])

par(mfrow=c(3,3))

for(i in 1:9){
  boxplot(BreastCancer[,i]~Class,
          data=BreastCancer,
          main=names(BreastCancer[i]))
}

#Atributo objetivo fica à direita
#Os outros atributos são previsoras

#Como as medidas estão bem desfazadas, estas são boas previsoras

BreastCancer$Class <- factor(BreastCancer$Class,
                             levels=c("malignant", "benign"),
                             labels("M", "B"))

AbsFreq <- table(BreastCancer$Class)
PercFreq <- prop.table(table(BreastCancer$Class))*100
cbind(AbsFreq,PercFreq)

#c

set.seed(1)

index <- sample(1 : nrow(BreastCancer), 0.7 * nrow(BreastCancer))

dt.train <- BreastCancer[index, ]
dt.tst <- BreastCancer[-index, ] 

#MSE e MAE só podem ser usados em modelos de regressão

#MATRIZ DE CONFUSAO -> MODELOS DE CLASSIFICAÇÃO

#TP = True positive
#TN = True Negative
#FP = False Positive
#FN = False negative

#          | maligno     benigno
#------------------------------
# maligno  |   TP          FN
# benigno  |   FP          TN

#Tx. acerto = (TP + TN) / (TP + FN + FP + TB)
#(accuracy)

#TxErro = 1- Tx.acerto

#Precision = TP / TP + FP

#Recall = TP / TP + FN
#Maximizar recall de modo a diminuir número de Falsos Negativos


#           |    Spam   |  N Spam
#----------------------------------------
# Spam      |     TP    |  FN
# N Spam    |     FP    |  TN


# F1 = (2 x precision x recall)  / (Precision + recall)

#d


rpart.model <- rpart(Class ~ . ,data = dt.train)
rpart.plot(rpart.model, digits=3)


rpart.pred <- predict(rpart.model, dt.tst, type = 'class')

cfmatrix <- table(dt.tst$Class, rpart.pred)

dim(dt.train)
dim(dt.tst)
cfmatrix

txAcerto<-function(tp, fn, fp, tn){
  (tp + tn) / (tp + fn + fp + tn)
}

txAcerto(cfmatrix[1], cfmatrix[3], cfmatrix[2], cfmatrix[4])

#e

data <- BreastCancer
data <- na.omit(data)
accuracy <- c()
for (i in 1:10){
  set.seed(i)
  index <- sample(1:nrow(data), 0.7*nrow(data))
  dt.train <- data[index,]
  dt.tst <- data[-index,]
  rpart.model <- rpart(Class~., data=dt.train)
  rpart.pred <- predict(rpart.model, dt.tst, type="class")
  cfmatrix <- table(dt.tst$Class, rpart.pred)
  accuracy <- c(accuracy,sum(diag(cfmatrix)/sum(cfmatrix)))
}
cat(paste("Tx.acerto média:", round(mean(accuracy), digits=3), "Desvio padrao: ", round(sd(accuracy), digits=3)))

#f
modelEvaluation <- function(tstlabels, predlabels){
  if(length(unique(tstlabels)) == length(unique(predlabels))){
    cfmatrix <- table(tstlabels, predlabels)
    accuracy <- sum(diag(cfmatrix))/sum(cfmatrix)
    precision <- cfmatrix[1,1]/sum(cfmatrix[,1])
    recall <- cfmatrix[1,1]/sum(cfmatrix[1, ])
    f1 <- 2*precision*recall/(precision + recall)
    data.frame(accuracy = round(accuracy, digits = 3), 
               precision = round(precision, digits = 3), 
               recall = round(recall, digits = 3),
               f1 = round(f1, digits = 3))
  }
}

#g
data <- BreastCancer
data <- na.omit(data)
k<-10
set.seed(123)
cvf <- sample( 1:k, nrow(data), replace = TRUE) #Função sample divide os dados em 10 folds aleatoriamente
table(cvf)
resdf <- data.frame(accuracy = double(), precision = double(), recall = double(), f1 = double())

for( i in 1:k){
  dt.tst <- data[cvf == i, ]
  dt.train <- data[cvf != i, ]
  rpart.model <- rpart(Class~., data=dt.train)
  rpart.pred <- predict(rpart.model, dt.tst, type="class")
  resdf <- rbind(resdf, modelEvaluation(dt.tst$Class, rpart.pred))
}

mean(resdf$accuracy)
sd(resdf$accuracy)

apply(resdf, 2, mean)
apply(resdf, 2, sd)
