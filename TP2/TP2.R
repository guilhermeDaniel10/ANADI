#Uma p�gina internet, dedicada a divulgar atrativos da cidade do Porto, afirma que o pre�o m�dio da
#renda de um apartamento T1 � de 720 euros mensais. Por consulta a uma p�gina web do setor
#imobili�rio, registaram-se os valores de renda mensal em euros de apartamentos T1 para alugar na
#cidade do Porto. 

# 1. Selecionados os an�ncios mais recentes, a 14 de fevereiro de 2022, observaram-se os seguintes
# valores:
rendas <- c(980,650,950,800,980,500,750,680,650,795,600,900,700,700,600,
650,650,650,1000,800)

#Considerando que o pre�o de arrendamento dos apartamentos, nas condi��es enunciadas,
#constitui uma vari�vel aleat�ria com distribui��o Normal:

#  a) Teste a hip�tese H0 : � = 720 contra H1 : � != 720 com ?? = 0.05;

#Teste bilateral: a verdadeira m�dia da popula��o � 720?
# � � a m�dia da popula��o

mu0 <- 720
alfa <- 0.05

#argumentos: rendas- � o vetor;
#mu- � par�metro da fun��o t test e guarda do valor de teste;
#alternativa: tipo de test
#conf.level: conf.level e significa que se � disposto a errar 5%
#p value- � uma probabilidade
result <- t.test(rendas, mu=mu0, alternative="two.sided", conf.level=1-alfa)

# b) Existe evidencia estat�stica para desmentir a p�gina internet?
#R: Como p=0.3915>0.05=alfa, ent�o n�o rejeitamos H0 e n�o podemos afirmar
#que o pre�o m�dio seja diferente dos 720 anunciados para um n�vel de signific�ncia de 5%.



#2. Selecionados os an�ncios mais antigos, na mesma data, observaram-se os seguintes valores:
#  1600 880 650 800 500 900 700 1040 599 650 700 850 1100 500 800
#990 1390 1590 700 1100
#Com estes dados, podemos afirmar que os an�ncios mais antigos s�o menos competitivos por
#serem mais caros? 

rendas_A <- c(1600,880,650,800,500,900,700,1040,599,650,700,850,1100,500,800,990,1390,1590,700,1100)

# a) Formule as hip�teses nula e alternativa para esta quest�o; 
# Teste Unilateral � direita (ver sempre o H1 para determinar o tipo de teste de hip�tese)
# H0: muA <= 720
# H1: muA > 720

# b) Indique o valor observado da estat�stica de teste t0 e o valor de prova p; 

resultado_A <- t.test(rendas_A, mu=mu0, alternative="greater", conf.level=1-alfa)
estatisticaDeTesteA <- resultado_A$statistic
pValue_A <- resultado_A$p.value

# c) Apresente a resposta `a quest�o e a respetiva justifica��o. 
# R: Como p aprox=0.01 < 0.05=alfa, ent�o rejeitamos H0. Para um n�vel de signific�ncia
# de 0.05 podemos afirmar que o pre�o m�dio das rendas antigas � superior aos 720 anunciados

#3. Na mesma data, e com os mesmos crit�rios de sele��o, ordenaram-se os an�ncios por
#Relev�ncia, e registaram-se os 20 valores de renda dos mais relevantes:
#  550 690 750 690 800 980 980 1300 750 475 680 700 650 640 795
# 650 600 1050 950 780
#Existir� evid�ncia estat�stica de que os an�ncios mais recentes prop�em renda mensal distinta
#das propostas consideradas mais relevantes pelos gestores da p�gina? 
 
# a) Formule as hip�teses nula e alternativa para esta quest�o; 

# muREL = popula��o relevante da al�nea A
# mu = popula��o atual

#H0: mu - muREL = 0
#H1: mu - muREL != 0

#Emparelhada: d� para conectar as duas amostrar (possivelmente em tempos diferentes)- interse��o
#S�o independentes

#� necess�rio fazer o teste Levene
# Logo:
# H0: sigma^2 = sigmaREL^2
# H1: sigma^2 != sigmaREL^2


rendas_rel <- c(550,690,750,690,800,980,980,1300,750,475,680,700,650,640,795,650,600,1050,950,780)

dados <- c(rendas, rendas_rel)
grupos <- as.factor(c(rep(1, length(rendas)), rep(2, length(rendas_rel))))
# c) Para um n�vel de signific�ncia de 1%, assumindo que os pre�os de renda mensal dos
# apartamentos T1 no Porto, anunciados nesta p�gina como os mais recentes e como os mais
# relevantes, constituem vari�veis aleat�rias independentes com distribui��o normal e
# vari�ncias desconhecidas, teste a hip�tese de estas vari�veis terem m�dias distintas. 

library(car)
leveneTest(dados, grupos, center=mean)
#Valor de P � a probabilidade de ser retirado um valor maior que F
# O teste de Levene n�o rejeita a hip�tese nula porque Pr > alfa=0.01 -> Podemos admitir homegenidade das vari�ncias
alfa <- 0.01
mu0Atual <- 0

t.test(rendas, rendas_rel, mu=mu0, conf.level = 1-alfa, var.equal = T, paired = F)

## ALINEA 5
 
Quoc_a <- c(1.0,0.8,1.9,1.1,2.7,NA)
Quoc_b <- c(1.7,2.5,3.0,2.2,3.7,1.9)
Quoc_c <- c(1.0,1.3,3.2,1.4,1.3,2.0)
Quoc_d <- c(3.8,2.8,1.9,3.0,2.5,NA)

dados_quoc <- as.data.frame(cbind(Quoc_a,Quoc_b,Quoc_c,Quoc_d))

boxplot(dados_quoc)

# Admitindo a normalidade e independencia, usaoms teste ANOVA:
# H0: miu1 = miu2 = miu3 = miu4 (os quocientes dos cusots com I&D sao semelhantes entres as empresas)
# H1: Existe pelo menos 1 (i,j), i!=j, tal que miu_i!=miu_k, para i e j em {1,2,3,4}

Empresa <- c(rep("Emp_a",6),rep("Emp_b",6),rep("Emp_c",6),rep("Emp_d",6))
Quoc <-c(Quoc_a,Quoc_b,Quoc_c,Quoc_d)
Mdados <- data.frame(Empresa,Quoc)
View(Mdados)
result <- aov(Quoc ~ Empresa, data = Mdados)
summary (result)

# Como p=0.0359 < alfa, rejeita-se H0. H� evidencias de que pelo menos uma empresa
#tem custos diferentes das restantes, para um nivel de significancia de 0.05

  
