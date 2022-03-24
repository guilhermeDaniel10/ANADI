#Uma página internet, dedicada a divulgar atrativos da cidade do Porto, afirma que o preço médio da
#renda de um apartamento T1 é de 720 euros mensais. Por consulta a uma página web do setor
#imobiliário, registaram-se os valores de renda mensal em euros de apartamentos T1 para alugar na
#cidade do Porto. 

# 1. Selecionados os anúncios mais recentes, a 14 de fevereiro de 2022, observaram-se os seguintes
# valores:
rendas <- c(980,650,950,800,980,500,750,680,650,795,600,900,700,700,600,
650,650,650,1000,800)

#Considerando que o preço de arrendamento dos apartamentos, nas condições enunciadas,
#constitui uma variável aleatória com distribuição Normal:

#  a) Teste a hipótese H0 : µ = 720 contra H1 : µ != 720 com ?? = 0.05;

#Teste bilateral: a verdadeira média da população é 720?
# µ é a média da população

mu0 <- 720
alfa <- 0.05

#argumentos: rendas- é o vetor;
#mu- é parâmetro da função t test e guarda do valor de teste;
#alternativa: tipo de test
#conf.level: conf.level e significa que se é disposto a errar 5%
#p value- é uma probabilidade
result <- t.test(rendas, mu=mu0, alternative="two.sided", conf.level=1-alfa)

# b) Existe evidencia estatística para desmentir a página internet?
#R: Como p=0.3915>0.05=alfa, então não rejeitamos H0 e não podemos afirmar
#que o preço médio seja diferente dos 720 anunciados para um nível de significância de 5%.



#2. Selecionados os anúncios mais antigos, na mesma data, observaram-se os seguintes valores:
#  1600 880 650 800 500 900 700 1040 599 650 700 850 1100 500 800
#990 1390 1590 700 1100
#Com estes dados, podemos afirmar que os anúncios mais antigos são menos competitivos por
#serem mais caros? 

rendas_A <- c(1600,880,650,800,500,900,700,1040,599,650,700,850,1100,500,800,990,1390,1590,700,1100)

# a) Formule as hipóteses nula e alternativa para esta questão; 
# Teste Unilateral à direita (ver sempre o H1 para determinar o tipo de teste de hipótese)
# H0: muA <= 720
# H1: muA > 720

# b) Indique o valor observado da estatística de teste t0 e o valor de prova p; 

resultado_A <- t.test(rendas_A, mu=mu0, alternative="greater", conf.level=1-alfa)
estatisticaDeTesteA <- resultado_A$statistic
pValue_A <- resultado_A$p.value

# c) Apresente a resposta `a questão e a respetiva justificação. 
# R: Como p aprox=0.01 < 0.05=alfa, então rejeitamos H0. Para um nível de significância
# de 0.05 podemos afirmar que o preço médio das rendas antigas é superior aos 720 anunciados

#3. Na mesma data, e com os mesmos critérios de seleção, ordenaram-se os anúncios por
#Relevância, e registaram-se os 20 valores de renda dos mais relevantes:
#  550 690 750 690 800 980 980 1300 750 475 680 700 650 640 795
# 650 600 1050 950 780
#Existirá evidência estatística de que os anúncios mais recentes propõem renda mensal distinta
#das propostas consideradas mais relevantes pelos gestores da página? 
 
# a) Formule as hipóteses nula e alternativa para esta questão; 

# muREL = população relevante da alínea A
# mu = população atual

#H0: mu - muREL = 0
#H1: mu - muREL != 0

#Emparelhada: dá para conectar as duas amostrar (possivelmente em tempos diferentes)- interseção
#São independentes

#É necessário fazer o teste Levene
# Logo:
# H0: sigma^2 = sigmaREL^2
# H1: sigma^2 != sigmaREL^2


rendas_rel <- c(550,690,750,690,800,980,980,1300,750,475,680,700,650,640,795,650,600,1050,950,780)

dados <- c(rendas, rendas_rel)
grupos <- as.factor(c(rep(1, length(rendas)), rep(2, length(rendas_rel))))
# c) Para um nível de significância de 1%, assumindo que os preços de renda mensal dos
# apartamentos T1 no Porto, anunciados nesta página como os mais recentes e como os mais
# relevantes, constituem variáveis aleatórias independentes com distribuição normal e
# variâncias desconhecidas, teste a hipótese de estas variáveis terem médias distintas. 

library(car)
leveneTest(dados, grupos, center=mean)
#Valor de P é a probabilidade de ser retirado um valor maior que F
# O teste de Levene não rejeita a hipótese nula porque Pr > alfa=0.01 -> Podemos admitir homegenidade das variâncias
alfa <- 0.01
mu0Atual <- 0

t.test(rendas, rendas_rel, mu=mu0, conf.level = 1-alfa, var.equal = T, paired = F)



  
