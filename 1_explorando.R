#################################################
##                                             ##
##  1. EXPLORANDO E CORREGINDO UMA DATA FRAME  ##
##  Guillermo L. Flórez Montero                ##
##                                             ##
#################################################




# Primeiro carregamos o diretório de trabalho

setwd("D:/Cursos/Semana da Bio 2020/datos")



# Vamos explorar um pouco os dados "a_lituratus.csv"
# estão no Github, não esqueçam de ler os metadados




# Usaremos esses pacotes para baixá-los diretamente do github

# caso não estejam descarregados use as linhas 27 e 28, tirando o jogo-da-velha

# install.packages("readr")
# install.packages("curl")

# o comando library permite carregar os pacotes no ambiente de trabalho

library(readr)
library(curl)





# Vamos aos dados

midata = read_csv("https://raw.githubusercontent.com/gflorezm/SBio2020/master/a_lituratus.csv")
View(midata)


# podemos explorar as primeiras seis e últimas seis linhas na consola

head(midata)
tail(midata)



# podemos ver um resumo também

str(midata)
summary(midata)


# muitas vezes as tabelas de dados podem ter erros que devem ser corregidos
# antes de iniciar uma análise.

# Vamos conferir valores faltantes (NA) na planilha (já o summary disse que há 17)
# para isso usamos um teste lógico

is.na(midata)

# Eu perguintei. R... meus dados tem NA?
# o R respondeu com uma data frame igualzinha mas preenchida com valores lógicos
# TRUE quando é um NA e FALSE quando não é.



# Vou pedir ao R somar os NA que estão em cada uma das colunas do data frame

sum(is.na(midata$Habitat))
sum(is.na(midata$Forearm))
sum(is.na(midata$body_mass))
sum(is.na(midata$Diet_richness))
sum(is.na(midata$Sex))


# Percebam que os NA só estão na coluna Diet_richness
# podemos identificar exatamente quais dados são


midata[is.na(midata$Diet_richness),]


# Na linha 85 diz: R... Separa as linhas de midata
# quando o valor na coluna Diet_richness seja NA.




################################################################

# quando indexamos em  R, do lado do lado esquerdo da virgula
# são as linhas e do lado direito as colunas [linhas,Colunas]

# vejam o valor da linha 2, coluna 3

midata[2,3]

# vejam toda a linha 16

midata[16,]

################################################################




# Identificar NAs é importante pois muitas funções não lidam bem com eles
# por outro lado sabemos (ver os metadados) que não deveria ter NAs
# e caso existir devem asumir valor de 0, façamos isso... dar valor 0 para eles


midata$Diet_richness[is.na(midata$Diet_richness)] = 0

# pronto, conferimos que não tem mais NA

midata[is.na(midata$Diet_richness),]




# Agora vamos conferir que todo esteja bem com as variáveis qualitativas

table(midata$Habitat)
table(midata$Sex)

# Percebam que no sexo há um erro de digitação que faz com que se crie uma
# uma nova categoría, aparecem: Female, Male e male. Vamos arrumar o erro de digitação

midata$Sex[midata$Sex == "male"] = "Male"
midata$Sex = factor(midata$Sex, levels = c("Male", "Female")) #convertir de novo em fator

#Pronto, conferir

table(midata$Sex)

summary(midata)



# PRONTO GENTEEEE
# Consertamos a tabela de dados, vamos salvá-la como CSV

write_csv(midata, "alituratus2.csv")




# FIM #
