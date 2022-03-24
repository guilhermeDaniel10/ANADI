#TP3- Testes não paramétricos 
# Alinea 1
#Sucesso- Ter pontuação superior a 50 pontos
#Tn ~ Bi(n=25,p=0,45)
# p^ = 13/25

#H0 : p = 0,45
#H1 : p > 0,45

# Teste unilateral à direita, logo queremos P(Tn>13 | p0=0,45)
#13:25 é de 13 a 25 um a um
sum(dbinom(13:25,25,0.45))

#ou
pbinom(12,25,prob=0.45,lower.tail = F)

#ou ainda
test1 <- binom.test(13,25,p=0.45,alternative = "greater", conf.level = 0.95)
test1$p.value

#Conclusão: como p=0.306>alfa, não se rejeita H0. Não há evidências de que os 
#oficiais do exército reajam melhor sobre pressão do que a população em geral.


# Alínea 2
#Foram selecionadas aleatoriamente 19 crianças
#15 crianças escolheram o balde vermelho

# Tn ~ Bi(n=19, p=0.5)
# H0 : P = 0.5
# H1 : p > 0.5

alfa <- 0.05

test2 <- binom.test(15,19,alternative = "greater", conf.level = 0.95)
test2$p.value

if(test2$p.value < alfa) {
  print("H0 rejeitada")
} else {
  print("Não se pode rejeitar H0")
}


# Conclusão: Como p=0.0096< alfa, rejeita-se H0. Assume-se para um nível de significância de 5%
# de 5%, que as crianças escolhem com mais frequência o balde vermelho do que o balde azul

# Alínea 3
#Como a amostra é grande (n=2000) e:
p_est <- 700/2000
2000*p_est*(1-p_est) #np(1-p) > 9

# Tn ~ Bi(2000, p=0.3)
# H0: p = 0.3
# H1: p > 0.3

test3 <- prop.test(700,2000,p=0.3,alternative = "greater") #95% é o default
# Com p= 6e-07 < alfa, rejeta-se H0. Asusme-se que o partido irá ter uma votação >30%,
# com um nível de confiança de 5%
test3$p.value
test3$method

#Curiosidade (teste exato)
binom.test(700,2000,p=0.3,alternative = "greater", conf.level=0.95)


#Alínea 4
n <- length(Ex4$produto) # Tamanho da amostra
n
n_A <- sum(Ex4$produto == "A")
 
p_estA <- n_A/n # p^ de A
p_estA

p_estB <- 1-p_estA
p_estB

n*p_estA*(1-p_estA) # >9 -> binomial aproximada (prop.test)

# H0: pA = pB
# H1: pA != pB
test4 <- prop.test(n_A,n, alternative = "two.sided") #95% é o default

# Como p=0.56 > alfa, não se rejeita H0. Não se pode afirmar que o sucesso dos produtos A e B
# seja diferente, com um nível de significância de 5%

# Alínea 5
# p1 = 1/6 = p2
# pi- é a probabilidade de sair face i (i=1,2,..,6)

# H0: p1 = p2 = p3 = p4 = p5 = p6 = 1/6 (o dado não está viciado)
# H1: pi != pj (para algum par (i,j), i != j e entre 1 e 6)

#Contagens:
contador <- numeric(6)
for (i in 1:6) {
  contador[i] <- sum(Ex5$Resultado == i)
}
contador
prop_est <- contador/60

test5 <- chisq.test(contador) # por defeito faz o ajustamento à uniforme,
#caso contrário teríamos que inserir ambos os vetores numa matriz (rbind, p.e.)

#Como p=0.39 > alfa, não se rejeita H0. Não podemos afirmar que o dado está viciado
#para um nível de significância de 5%


# Alínea 6

Ex6 <- c(25.36,24.64,25.37,25.17,24.56,24.56,24.80,25.21,25.38,24.55)

# Podemos vetrificar a normaidade de várias formas:
shap_t <- shapiro.test(Ex6)
shap_t$p.value

# Não aplicável aqui pois n é < 30
library(nortest)
Lill_t <- lillie.test(Ex6)
Lill_t$p.value

# Ou usando o QQ plot
qqnorm(Ex6)
qqline(Ex6)

#Tanto pelo Shapiro Wilk como pelo QQ plot, os dados desviam-se de
# uma dist.normal, e portanto temos que usar um teste não paramétrico (em alternativa ao test t)

#Testes não paramétricos: Wilcoxon (se dist.simétrica) ou o teste do sinal
hist(Ex6)

library(moments)
skewness(Ex6) #|skewness| < 0.1 => distribuição simétrica

#Assumimos então que a dist. de onde provem os dados é simétrica
# podemos então aplicar o teste de Wilcoxon
# H0: a mediana = 25
# H1: a mediana > 25
wilcox.test(Ex6, alternative= "greater", mu=25, exact = FALSE)
#exact = FALSE porque há empates
