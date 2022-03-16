census<-read.csv("C:/Users/Guilherme/Desktop/PL1_ANADI/adult.csv", header = FALSE) #importar os dados

class(census) #verificar a classe dos dados

View(census) #visualizar o resumo de dados

str(census) #estrutura interna do dataframe

dim(census) #dimensao do dataframe

names(census) #nomes das colunas do data frame

head(census) #visualizar as primeiras 6 linhas do dataframe