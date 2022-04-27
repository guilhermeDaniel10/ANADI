# TP4) Correlação e regração linear

# Ex1)    Kendall (tau)

anunc <- c(8, 12, 16, 8, 8, 8, 27, 12, 8, 16)
medid <- c(4.8, 5, 17.5, 6.4, 6.7, 4.3, 20.3, 8, 3.5, 6.3)

# H0: tau = 0    vs    H1: tau != 0
cor.test(anunc, medid, alternative = "two.sided", method = "kendall")

# O coeficiente de kendall é 0.597. Como p0=0.0257<alfa, rejeita-se H0
# há evidências de ma relação significativa ent o tempo anunciado e o medido.

# Ex2)
library(readr)
Notas <-
  read_delim(
    "C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP4/Notas.txt",
    delim = "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  )
View(Notas)

# Converter notas calc e estat em numerico (Calc_n e Estat_n)
niveis_Estat <-
  c("Excelente",
    "Muito Bom",
    "Bom",
    "Suficiente",
    "Insuficiente",
    "Mau")
codigos_Estat <- 1:6
niveis_Calc <- c("A", "B", "C", "D", "E", "F")
codigos_Calc <- 1:6
Notas$Estat_n <-
  codigos_Estat[match(Notas$Estatística, niveis_Estat)]
Notas$Calc_n <- codigos_Calc[match(Notas$Cálculo, niveis_Calc)]


# H0: rho = 0  vs   H1: rho > 0
cor.test(Notas$Calc_n,
         Notas$Estat_n,
         alternative = "greater",
         method = "spearman")

# Ex 3
library(readr)
fang_data <-
  read_csv("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP4/fang_data.csv")
View(fang_data)

library(Hmisc)
rcorr(as.matrix(fang_data[2:5]))


# H0 : R = 0
# H1 : R != 0
# Se p > 0.05, rejeita-se H0
# Se p < 0.001 < alfa -> rejeita-se H0

# Os dados são continuoes, mas serao normais
library(nortest)
lillie.test(fang_data$Facebook)

hist(fang_data$Facebook)
hist(fang_data$Amazon)
hist(fang_data$Netflix)

rcorr(as.matrix(fang_data[2:5]), type = "spearman")
# não se encontam diferenças entre os coefs de Pearson e Spearman
#Conclusão:

# Ex 5
xi <- c(21, 24, 32, 47, 50, 59, 68, 74, 62, 50, 41,30)
yi <- c(
  185.79,
  214.47,
  288.03,
  424.84,
  454.58,
  539.03,
  621.55,
  675.06,
  562.03,
  452.93,
  369.95,
  273.98
)
# 5)a)

plot(xi,yi)

# 5)b)
reg1 <- lm (yi ~ xi)
summary(reg1)

# Residuals- Diferença entre o valor observador e o esperado, ou seja,
# é o erro de previsão

# y chapeu é o estimado 
# y^= b0 + b1 x xi
# y^=6.3355+9.20836xi -> equação da reta

# H0: Beta1 = 0
# H1: Beta1 != 0

# A relação linear entre as variáveis é significativa, porque
# p value é 

b0 <- reg1$coefficients[1] ; b0
b1 <- reg1$coefficients[2] ; b1
# ou 
coef(reg1)
### yi = -6.335502 + 9.208362 * xi
y40 <- b0 + b1*40 ; y40
# ou
x0 <- data.frame(xi=40)
predict(reg1,x0)
# y(40)=361.999

# 5)c)
cor.test(xi,yi,method="pearson")

# r=0.9999, ou eja, há uma correlação linear positiva forte e
# essa correlação é significativa (p<2.2e-16)

# 5)d) Pressupostos RL: Homocedasticidade:Normalidade dos Residuos de 
yi
fitted(reg1)
xi_df <- data.frame(xi)
predict(reg1,xi_df)


residuals(reg1)   # = yi-fitted(reg1)

# Residuos vs valores ajustados
par(mfrow=c(2,1))
plot(fitted(reg1), residuals(reg1), xlab="val. Ajustados", ylab="Residuos")
abline(h=0)
plot(xi, residuals(reg1),xlab="xi",ylab="Residuos")
abline(h=0)

# Amostra pequena, dificil concluir se ha um padrao ou nao

# Teste à variancia
# H0: As variancias sao iguais vs H1: as variancias sao diferentes
mx <- median(xi); mx
var.test(residuals(reg1)[xi>mx], residuals(reg1)[xi<mx])

# Não se rejeita H0 (p=0.687), ou seja, não há evidência de que as variâncias

#NOTA: Para amostrar grandes (n>30), assumindo residuos independentes
# e com distribuição normal, pode-se efetuar um test de Breusch-Pagan
# (função bptest do package lmtest).

library(lmtest)
bptest(formula = yi ~ xi)

### 2º Pressuposto: Normalidade dos Residuos de media zero
dev.off()
qqnorm(residuals(reg1), xlab="Rsiduos", ylab="quantis teoricos")
qqline(residuals(reg1))
hist(residuals(reg1))
shapiro.test(residuals(reg1))

# Os resultados do teste confirmam que podemos assumir a distribuicao
# dos residuos (p=0.4066)

# H0: Os residuos tem media 0   vs   H1: Os residuos tem media diff, de
t.test(residuals(reg1),mu=0,alternative = "two.sided")
#Os resultados confirmar também que podemos assumir
# que os residuaos tem media 0 (p=1)

### 3º Pressuposto: independencia dos residuos
# H0: Os residuos são independentes vs H1: Os residuos não são independentes
library(car)
durbinWatsonTest(reg1)


# Ex 6
rendimento <- c (14,19,23,12,9,15,22,25,15,10,12,16)
capital <- c(31,40,49,20,21,34,54,52,28,21,24,34)

# a)
plot(rendimento,capital)

reg2 <- lm(capital ~ rendimento)
summary(reg2)
abline(reg2)
# Os dados são adequados a um modelo linear

# 6)b)
x0 <- data.frame(rendimento=20)
predict(reg2,x0)
# O modelo preve um seguro de vida medio de 

#6)c

# Residuos vs valores ajustados
par(mfrow=c(2,1))
plot(fitted(reg2), residuals(reg2))
abline(h=0)
# Amostra pequena dificil concluir se ha um padrao ou nao 

# Teste à variancia (n=12<30)
medRend <- median(Rend); mdRend
var.test(residuals(reg2)[Rend>medRend], residuals(reg2)[Rend<medRend])
# nao se rejeita H0 (p=0.806), ou seja,
# nao ha evidencia de que as variancias nao sejam iguais 

# Independencia dos residuos
# H0: Os residuos sao independentes vs H1: os residuos nao sao independentes

library(car)
durninWatsonTest(reg2)
# Como p>0.8>0.05 

#Temis entao evidencia de que os residuos cumprem os 2 pressupostos de 
#homocedasticidade

#6)d) Normalidade de Residuos de media zero
dev.off()
qqnorm(residuals(reg2))
qqline(residuals(reg2))

t.test(residuals(reg2),mu=0, alternative="two.sided")


# Exercicio 7
library(readr)
ExemploMontegomery <- read_delim("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP4/ExemploMontegomery.csv", 
                                 delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                 trim_ws = TRUE)
# Observar os dados que temos 
