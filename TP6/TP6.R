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

#fazer com que o dados não numericos sejam categoricos
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

# 11 é maligno
AbsFreq <- table(BreastCancer$Class)
PropFreq <- prop.table(table(BreastCancer$Class))*100
cbind(AbsFreq,PropFreq)