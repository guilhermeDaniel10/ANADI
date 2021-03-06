#TP3- Testes n�o param�tricos
# Alinea 1
#Sucesso- Ter pontua��o superior a 50 pontos
#Tn ~ Bi(n=25,p=0,45)
# p^ = 13/25

#H0 : p = 0,45
#H1 : p > 0,45

# Teste unilateral � direita, logo queremos P(Tn>13 | p0=0,45)
#13:25 � de 13 a 25 um a um
sum(dbinom(13:25, 25, 0.45))

#ou
pbinom(12, 25, prob = 0.45, lower.tail = F)

#ou ainda
test1 <-
  binom.test(13,
             25,
             p = 0.45,
             alternative = "greater",
             conf.level = 0.95)
test1$p.value

#Conclus�o: como p=0.306>alfa, n�o se rejeita H0. N�o h� evid�ncias de que os
#oficiais do ex�rcito reajam melhor sobre press�o do que a popula��o em geral.


# Al�nea 2
#Foram selecionadas aleatoriamente 19 crian�as
#15 crian�as escolheram o balde vermelho

# Tn ~ Bi(n=19, p=0.5)
# H0 : P = 0.5
# H1 : p > 0.5

alfa <- 0.05

test2 <-
  binom.test(15, 19, alternative = "greater", conf.level = 0.95)
test2$p.value

if (test2$p.value < alfa) {
  print("H0 rejeitada")
} else {
  print("N�o se pode rejeitar H0")
}


# Conclus�o: Como p=0.0096< alfa, rejeita-se H0. Assume-se para um n�vel de signific�ncia de 5%
# de 5%, que as crian�as escolhem com mais frequ�ncia o balde vermelho do que o balde azul

# Al�nea 3
#Como a amostra � grande (n=2000) e:
p_est <- 700 / 2000
2000 * p_est * (1 - p_est) #np(1-p) > 9

# Tn ~ Bi(2000, p=0.3)
# H0: p = 0.3
# H1: p > 0.3

test3 <-
  prop.test(700, 2000, p = 0.3, alternative = "greater") #95% � o default
# Com p= 6e-07 < alfa, rejeta-se H0. Asusme-se que o partido ir� ter uma vota��o >30%,
# com um n�vel de confian�a de 5%
test3$p.value
test3$method

#Curiosidade (teste exato)
binom.test(700,
           2000,
           p = 0.3,
           alternative = "greater",
           conf.level = 0.95)


#Al�nea 4
n <- length(Ex4$produto) # Tamanho da amostra
n
n_A <- sum(Ex4$produto == "A")

p_estA <- n_A / n # p^ de A
p_estA

p_estB <- 1 - p_estA
p_estB

n * p_estA * (1 - p_estA) # >9 -> binomial aproximada (prop.test)

# H0: pA = pB
# H1: pA != pB
test4 <-
  prop.test(n_A, n, alternative = "two.sided") #95% � o default

# Como p=0.56 > alfa, n�o se rejeita H0. N�o se pode afirmar que o sucesso dos produtos A e B
# seja diferente, com um n�vel de signific�ncia de 5%

# Al�nea 5
# p1 = 1/6 = p2
# pi- � a probabilidade de sair face i (i=1,2,..,6)

# H0: p1 = p2 = p3 = p4 = p5 = p6 = 1/6 (o dado n�o est� viciado)
# H1: pi != pj (para algum par (i,j), i != j e entre 1 e 6)

#Contagens:
contador <- numeric(6)
for (i in 1:6) {
  contador[i] <- sum(Ex5$Resultado == i)
}
contador
prop_est <- contador / 60

test5 <-
  chisq.test(contador) # por defeito faz o ajustamento � uniforme,
#caso contr�rio ter�amos que inserir ambos os vetores numa matriz (rbind, p.e.)

#Como p=0.39 > alfa, n�o se rejeita H0. N�o podemos afirmar que o dado est� viciado
#para um n�vel de signific�ncia de 5%


# Al�nea 6

Ex6 <-
  c(25.36,
    24.64,
    25.37,
    25.17,
    24.56,
    24.56,
    24.80,
    25.21,
    25.38,
    24.55)

# Podemos vetrificar a normaidade de v�rias formas:
shap_t <- shapiro.test(Ex6)
shap_t$p.value

# N�o aplic�vel aqui pois n � < 30
library(nortest)
Lill_t <- lillie.test(Ex6)
Lill_t$p.value

# Ou usando o QQ plot
qqnorm(Ex6)
qqline(Ex6)

#Tanto pelo Shapiro Wilk como pelo QQ plot, os dados desviam-se de
# uma dist.normal, e portanto temos que usar um teste n�o param�trico (em alternativa ao test t)

#Testes n�o param�tricos: Wilcoxon (se dist.sim�trica) ou o teste do sinal
hist(Ex6)

library(moments)
skewness(Ex6) #|skewness| < 0.1 => distribui��o sim�trica

#Assumimos ent�o que a dist. de onde provem os dados � sim�trica
# podemos ent�o aplicar o teste de Wilcoxon
# H0: a mediana = 25
# H1: a mediana > 25
wilcox.test(Ex6,
            alternative = "greater",
            mu = 25,
            exact = FALSE)
#exact = FALSE porque h� empates

## Alinea 7

tempo_arranque_antes <- c(14, 9.0, 12.5, 13, 9.5, 12.1)

tempo_arranque_depois <- c(13.8, 8.9, 12.6, 12.8, 9.2, 14.2)

t_dif <- tempo_arranque_antes - tempo_arranque_depois

#a)
# Hipoteses: H0: mediana= 0 vs mediana > 0
library(BSDA)
sigint <- SIGN.test(t_dif, mu = 0, alternative = "greater")
sigint$p.value

#Como p=p.344 > alfa, n�o podemos rejeitar H0. N�o podemos
#afirmar que haja uma reducao significativa no tempo mediano
# de arranque com o novo dispositivo, para um nivel de significancia de 0.05

#b) Wilcoxon
# verificar a simetria dos dados:
hist(t_dif)
library(moments)
skewness(t_dif)

#|skewness|>1 => assimetria forte
# nao podemos aplicar testes Wilcoxon

#Verificar normalidade
shapiro.test(t_dif)


# alinea 8
# a)
library(readr)
cevada <-
  read_csv("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP3/cevada.csv")
View(cevada)

# n >= 30 -> TLC    Xbarra ~ N(miu,oLixado)
# Podemos assumir a normalidade porque o n � maior que 30
# H0: miu_dif= 0    vs     H1: miu_dif != 0
t.test(cevada$Y1,
       cevada$Y2,
       paired = TRUE,
       alternative = "two.sided")

# podemos afirmar que as colheitas n�o foram

# b)
# Como a amostra � inferior a 30, n�o � normal. Por isso tenta-se a hipotese

colh_dif <- cevada$Y1 - cevada$Y2
hist(colh_dif)
skewness(colh_dif)

#0.1<|skewness|<1 => assimatria moderada, podemos aplicar wilcoxon
wilcox.test(
  cevada$Y1,
  cevada$Y2,
  paired = TRUE,
  alternative = "two.sided",
  exact = FALSE
)
#Corrobora o resultado anterior (p=0.0053)


# 11)

library(readr)
dados_vacinas <-
  read_csv("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP3/dados_vacinas.csv")
View(dados_vacinas)

# Amostrar independentes, 3 grupos

library(dplyr)
antic_vA <- filter(dados_vacinas, vacina == "A")
antic_vA
antic_vB <- filter(dados_vacinas, vacina == "B")
antic_vB
antic_vC <- filter(dados_vacinas, vacina == "C")
antic_vC

# Amostrar pequneas -> Kruskal-Wallis
# H0: as medianas de quantidade de anticorpos s�o iguais para as 3 vacinas
# H1: pelo menos uma das vacinas produz uma quantidade diferente de anticorpos das outras vacinas
kruskal.test(anticorpos ~ vacina, data = dados_vacinas)

# Como p=0.026 < alfa, rejeita-se H0. Podemos afimar que pelo menos uma das vacinas produz quantidades de anticorpos diferentes das restantes,
#para um n�vel de signific�ncia de 0.05

vaci_data <- cbind(as.data.frame(antic_vA[2]),
                   as.data.frame(antic_vB[2]),
                   as.data.frame(antic_vC[2]))
boxplot(vaci_data)


# Ex13)
# Importar dados

library(readr)
desempenho <-
  read_delim(
    "C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP3/desempenho.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )
View(desempenho)

# (temos ent�o 12x3 avalia��es) -> teste de Friedman:


#H0: h� concord�ncia entre avaliadores sobre a melhor marca
#H1: n�o h� concord�ncia entre avaliadores sobre a melhor marca

friedman.test(desempenho$Resposta, desempenho$Marca, desempenho$Utilizador)

# Como p=0.0026 < alfa, rejeita-se H0. H� evid�ncias que nos permite concluir que n�o h�
# concord�ncia sobre a melhor marca entre os utilizadores, para um n�vel de signific�ncia de 0.05

# Ex14)
# Importar dados
library(readr)
aval_restaurantes <-
  read_csv("C:/Users/Guilherme/Desktop/ANADI_REPO/ANADI/TP3/aval_restaurantes.csv")
View(aval_restaurantes)

attach(aval_restaurantes)
aval_rest <-
  data.frame(
    gastronomo = rep(c(1, 2, 3, 4, 5, 6), 4),
    restaurante = c(rep("A", 6), rep("B", 6), rep("C", 6), rep("D", 6)),
    avaliacao = c(A, B, C, D)
  )
detach(aval_restaurantes)

# 6 gastronomos avaliam 4 restaurantes -> amostrar emparelhadas -> Teste de Friedman
# H0: h� concordancia entre os gastronomos sobre o melhor restaurante
# H1: n�o h� concordancia entre os gastronomos sobre o melhor restaurante
friedman.test(avaliacao~restaurante|gastronomo,data=aval_rest)
