library("readxl")

# EXERCICIO 1
# Alinea a

# L� o ficheiro na folha quadro com a respetiva range
diasSemChuva <- read_excel("C:/Users/Guilherme/Desktop/PL1_ANADI/nDiasSemChuva.xlsx", range= "Quadro!A8:J69")
View(diasSemChuva) # mostra a tabela num formato de leitura acess�vel

# Alinea b
colnames(diasSemChuva)[1] <- "Anos" # Muda o nome apenas de uma coluna, com o dataframe identificado e com o index da coluna a ser modificada

# Alinea c
# No boxplot est� limitado o gr�fico apenas 
# da coluna 2 � 10 do dataframe (por causa da coluna dos anos)
boxplot(diasSemChuva[2:10])

#Para que o gr�fico fique correto, � necessi�rio substituir os 0 por NA
diasSemChuva[diasSemChuva == 0] <- NA


boxplot(diasSemChuva[2:10]) # O boxplot j� n�o tem os valores 0 como anteriormente

# Boxplot cria um diagrama de extremos e quartis, bom para dados quantitativos
# Analisando o gr�fica, o linha dentro de cada ret�ngulo � a mediana (2� quartil),
# o 3� quartil � a base de topo do ret�ngulo, o 1� quartil � a base de baixo 
# do ret�ngulo
# as bolas s�o outliers, se for severo s�o asteriscos
# Os limites inferiores s�o representados pela reta mais a baixo
# e os limites superiores � a reta mais a cima

# 1� quartil -> 25% dos dados s�o inferiores ou iguais a este valor
# 2� quartil -> � a mediana, 50% dos dados s�o menores ou iguais ao valor
# 3� quartil -> 75% dos dados s�o inferiores ou iguais ao valor


#Remover outliers com outline = false
#Adiciona tamb�m um t�tulo ao gr�fico e um t�tulo ao eixo das ordenas 
boxplot(diasSemChuva[2:10], main="Dias sem chuva", ylab="Dias sem chuva", outline=FALSE)


# Analisando o gr�fico, a esta��o com menos dias de chuva � Angra do Hero�smo
# a esta��o com mais dias de chuva � o Funchal 
# Existe uma grande dispers�o de dados em Castelo Branco 

# Em Viana do Castelo existe bastante assimetria, e no Porto simetria

# Alinea f
# Faz uma tabela com os quartis, com o respetivo valor dos quartis
# ou seja, valor inferior, superior, 1� e 3� quartil, e mediana
quartis.CS <- quantile(diasSemChuva$`Castelo Branco`)

# Para mostrar apenas o 1� e 3� quartil
quartis.CS2 <- quantile(diasSemChuva$`Castelo Branco`,c(0.25,0.75))

# Faz uma tabela com os quartis apenas para o Porto, onde a exist�ncia de na � true
# se na fosse false, nunca seriam gerados os quartis
quartis.P <- quantile(diasSemChuva$`Porto`,na.rm=TRUE)

# Alinea g

table(diasSemChuva$Porto)

nc <- nclass.Sturges(diasSemChuva$Porto) # Faz um calculo do numero de grupos sugeridos para a quantidade de dados
tabela <- table(cut(diasSemChuva$Porto,breaks=nc)) # Faz a tabela em 7 classes de acordo com o calculo anterior

# Frequ�ncia relativa

prop.table(tabela)

# Alinea h
histDiasSemChuvaPorto <- hist(diasSemChuva$Porto, main="Dias sem chuva no Porto", xlab="Dias sem chuva de 1960 a 2020")
#Aceder aos detalhes da representa��o do gr�fico
histDiasSemChuvaPorto$breaks 
freqAbsPorto <- histDiasSemChuvaPorto$counts # frequ�ncia absoluta
freqRelPorto <- freqAbsPorto/sum(freqAbsPorto) # frequ�ncia relativa


# Evolu��o temporal - al�nea i

evolucaoTemporalPorto <- ts(diasSemChuva$Porto, start = 1960)
evolucaoTemporalFaro <- ts(diasSemChuva$Faro, start = 1960)

ts.plot(evolucaoTemporalPorto, evolucaoTemporalFaro, lty=c(1:2), main="Dias sem chuva",
        ylab="Numero de dias", xlab="Ano")
legend("bottomleft", legend=c("Porto", "Faro"), lty=c(1:2))
