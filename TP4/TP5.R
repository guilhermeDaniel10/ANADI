# TP5) Correlação e regração linear

# Ex1)    Kendall (tau)

anunc <- c(8, 12, 16, 8, 8, 8, 27, 12, 8, 16)
medid <- c(4.8, 5, 17.5, 6.4, 6.7, 4.3, 20.3, 8, 3.5, 6.3)

# H0: tau = 0    vs    H1: tau != 0
cor.test(anunc, medid, alternative = "two.sided", method = "kendall")

# O coeficiente de kendall é 0.597. Como p0=0.0257<alfa, rejeita-se H0
# há evidências de ma relação significativa ent o tempo anunciado e o medido.

# Ex2)
library(readr)
Notas <- read_table2("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP5/Notas.txt")
View(Notas)

# Converter notas calc e estat em numerico (Calc_n e Estat_n)
niveis_Estat <- c("Excelente","Muito Bom", "Bom", "Suficiente", "Insuficiente", "Mau")
codigos_Estat <- 1:6
niveis_Calc <- c("A","B","C","D","E","F")
codigos_Calc <- 1:6
Notas$Estatística <- codigos_Estat[match(Notas$Estatística,niveis_Estat)]
Notas$Cálculo <- codigos_Calc[match(Notas$Cálculo,niveis_Calc)]
