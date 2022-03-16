library("readxl")

# EXERCICIO 1
# Alinea a

# Lê o ficheiro na folha quadro com a respetiva range
diasSemChuva <- read_excel("C:/Users/Guilherme/Desktop/PL1_ANADI/nDiasSemChuva.xlsx", range= "Quadro!A8:J69")
View(diasSemChuva) # mostra a tabela num formato de leitura acessível

# Alinea b
colnames(diasSemChuva)[1] <- "Anos" # Muda o nome apenas de uma coluna, com o dataframe identificado e com o index da coluna a ser modificada

# Alinea c
# No boxplot está limitado o gráfico apenas 
# da coluna 2 à 10 do dataframe (por causa da coluna dos anos)
boxplot(diasSemChuva[2:10])

#Para que o gráfico fique correto, é necessiário substituir os 0 por NA
diasSemChuva[diasSemChuva == 0] <- NA


boxplot(diasSemChuva[2:10]) # O boxplot já não tem os valores 0 como anteriormente

# Boxplot cria um diagrama de extremos e quartis, bom para dados quantitativos
# Analisando o gráfica, o linha dentro de cada retângulo é a mediana (2º quartil),
# o 3ª quartil é a base de topo do retângulo, o 1º quartil é a base de baixo 
# do retângulo
# as bolas são outliers, se for severo são asteriscos
# Os limites inferiores são representados pela reta mais a baixo
# e os limites superiores é a reta mais a cima

# 1º quartil -> 25% dos dados são inferiores ou iguais a este valor
# 2º quartil -> é a mediana, 50% dos dados são menores ou iguais ao valor
# 3º quartil -> 75% dos dados são inferiores ou iguais ao valor


#Remover outliers com outline = false
#Adiciona também um título ao gráfico e um título ao eixo das ordenas 
boxplot(diasSemChuva[2:10], main="Dias sem chuva", ylab="Dias sem chuva", outline=FALSE)


# Analisando o gráfico, a estação com menos dias de chuva é Angra do Heroísmo
# a estação com mais dias de chuva é o Funchal 
# Existe uma grande dispersão de dados em Castelo Branco 

# Em Viana do Castelo existe bastante assimetria, e no Porto simetria

# Alinea f
# Faz uma tabela com os quartis, com o respetivo valor dos quartis
# ou seja, valor inferior, superior, 1º e 3º quartil, e mediana
quartis.CS <- quantile(diasSemChuva$`Castelo Branco`)

# Para mostrar apenas o 1º e 3º quartil
quartis.CS2 <- quantile(diasSemChuva$`Castelo Branco`,c(0.25,0.75))

# Faz uma tabela com os quartis apenas para o Porto, onde a existência de na é true
# se na fosse false, nunca seriam gerados os quartis
quartis.P <- quantile(diasSemChuva$`Porto`,na.rm=TRUE)

# Alinea g

table(diasSemChuva$Porto)

nc <- nclass.Sturges(diasSemChuva$Porto) # Faz um calculo do numero de grupos sugeridos para a quantidade de dados
tabela <- table(cut(diasSemChuva$Porto,breaks=nc)) # Faz a tabela em 7 classes de acordo com o calculo anterior

# Frequência relativa

prop.table(tabela)

# Alinea h
histDiasSemChuvaPorto <- hist(diasSemChuva$Porto, main="Dias sem chuva no Porto", xlab="Dias sem chuva de 1960 a 2020")
#Aceder aos detalhes da representação do gráfico
histDiasSemChuvaPorto$breaks 
freqAbsPorto <- histDiasSemChuvaPorto$counts # frequência absoluta
freqRelPorto <- freqAbsPorto/sum(freqAbsPorto) # frequência relativa


# Evolução temporal - alínea i

evolucaoTemporalPorto <- ts(diasSemChuva$Porto, start = 1960)
evolucaoTemporalFaro <- ts(diasSemChuva$Faro, start = 1960)

ts.plot(evolucaoTemporalPorto, evolucaoTemporalFaro, lty=c(1:2), main="Dias sem chuva",
        ylab="Numero de dias", xlab="Ano")
legend("bottomleft", legend=c("Porto", "Faro"), lty=c(1:2))
