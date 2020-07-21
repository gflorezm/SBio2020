#################################################
##                                             ##
##  2. ANÁLISE EXPLORATÓRIA DE DADOS           ##
##  Guillermo L. Flórez Montero                ##
##                                             ##
#################################################


# Antes de iniciar uma sugestão:

# Não excecute um script inteiro
# va linha por linha, lendo e tentando entender
# o que quer dizer cada linha.

# Caso não entenda uma linha, pergunte...



# Este tutorial trata sobre exploração gráfica de dados
# utilizando principalmente ferramentas do pacote base de R






# Primeiro carregamos o diretório de trabalho
# lembre de substituir o caminho da minha pasta

setwd("D:/Cursos/Semana da Bio 2020/datos")



# Vamos explorar um pouco os dados "alituratus2.csv"
# são derivados dos dados "a_lituratus.csv" porém foram corregidos
# no tutorial anterior e salvos no diretório de cada um



# Vamos aos dados

midata2 = read.csv("alituratus2.csv")
View(midata2)


# Podemos testar calcular média, mediana, desvio padrão
# de uma variável

mean(midata2$Forearm)
median(midata2$Forearm)
sd(midata2$Forearm)

# Inclusive os quantis

quantile(midata2$Forearm, probs = 0.25)
quantile(midata2$Forearm, probs = 0.5) # a mediana
quantile(midata2$Forearm, probs = 0.75)


#Tentem aplicar essas funções ao dataframe inteiro

















# Para aplicar uma função a uma data frame
# utilizamos a função sapply()

sapply(midata2[,2:4], mean)
sapply(midata2[,2:4], median)
sapply(midata2[,2:4], sd)


# Perceberam por quê estou indexando as colunas 2:4?




# VAMOS BRINCAR COM GRÁFICOS

plot(midata2$Forearm)
boxplot(midata2$Forearm)
stripchart(midata2$Forearm, method = "stack")

# Coloca os três juntos numa mesma janela

x11() # abre uma janela gráfica

# primeiro dividimos a àrea do gráfico
# em uma matriz de 1 linha e 3 colunas

par(mfrow = c(1,3))

plot(midata2$Forearm)
boxplot(midata2$Forearm)
stripchart(midata2$Forearm, method = "stack")

par(mfrow = c(1,1)) # Devolvemos à área gráfica normal



# vamos ver todas as possibilidades que tem
# a função par() dos gráficos

?par

# A maioría desses parâmetros podem ser mudados
# dentro da função de graficação.


# AGORA?

# Vamos explorar duas variables ao mesmo tempo no boxplot

# Cuando usamos duas variáveis, é comum usar uma sintaxe tipo
# variável resposta ~ variável preditora

boxplot(Forearm~Habitat, data = midata2)


# mudar a cor do gráfico?... pode

boxplot(Forearm~Habitat, data = midata2,
        col = "red")


# Colocar uma cor para cada tipo de hábitat?... pode

boxplot(Forearm~Habitat, data = midata2,
        col = c("red", "blue", "green"))

# mudar o nome dos eixos, tambem pode

boxplot(Forearm~Habitat, data = midata2,
        col = c("red", "blue", "green"),
        ylab = "Comprimento do antebraço")


# Veja todo o que o par() pode fazer por você






# Vamos fazer um gráfico tipo xy plot 
# da relação entre a massa corporal e a riqueza da dieta

# Aquí vamos usar farias ferramentas de edição par()

plot(diet_richness ~ body_mass, 
     data = midata2, pch = 16, 
     xlim = c(70,160), ylim = c(0,12), 
     xlab = "Massa corporal (g)", 
     ylab = "Longitud del antebrazo (mm)", 
     cex.lab = 1.2, font.lab = 2)

# posso colorir os pontos de acordo ao tipo de hábitat
# mas para isso, preciso indexar,
# sepaerar e sobrepor gráficos)

plot(midata2$diet_richness[midata2$Habitat == "Forest"] ~ 
       midata2$body_mass[midata2$Habitat == "Forest"],
     col = "green", pch = 16, cex = 1.2,
     xlim = c(70,160), ylim = c(0,12), 
     xlab = "Massa corporal (g)", 
     ylab = "Riqueza da dieta", 
     cex.lab = 1.2, font.lab = 2)

par(new = TRUE)
plot(midata2$Diet_richness[midata2$Habitat == "Urban"] ~ 
       midata2$body_mass[midata2$Habitat == "Urban"],
     col = "grey30", pch = 16, cex = 1.2,
     xaxt = "n", yaxt = "n",
     xlim = c(70,160), ylim = c(0,12), 
     xlab = " ", 
     ylab = " ", 
     cex.lab = 1.2, font.lab = 2)



# Vamos praticar com os gráficos usados na aula

# Ao longo dos tutoriais sempre irá um espaço para fazer gráficos
# explorando nossos dados ou resultados








########################################
#
#   GRÁFICOS USADOS NA AULA
#
########################################


# NOTA: quando forem fazendo os gráficos
# executem linha por linha e vejam o que va
# acrescentando a linha seguinte


#######################################

# Histograma do slide 38 da aula

x11() # Abre uma janela gráfica

hist(midata2$Forearm, nclass = 15, probability = TRUE,
     xlim = c(40,75), ylim = c(0,0.12), col = "grey80",
     cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5,
     xlab = "Comprimento do antebraço (mm)",
     ylab = "Frequência", main = " Histograma")

# adiciona as os valores das observações
rug(jitter(midata2$Forearm), col = "red")

# Eu gosto dos gráficos fechados
box()


# Adicionar a media (slide 39)

abline(v = mean(midata2$Forearm),
       col = "darkred", lty = 3, lwd = 3)

# o texto
text(x = 56.5, y = 0.09, labels = "56.38",
     adj = c(0,1), cex = 1.2)


# sobrepor uma gaussiana (slide 40)

curve(expr = dnorm(x, mean = mean(midata2$Forearm),
                   sd = sd(midata2$Forearm)),
      add = TRUE, col = "darkred")


#########################################################

# Histograma separando as gaussianas por tipo de hábitat
# Slide 42


# Primeiro separamos os dados de antebrço em vetores
# de acordo com o tipo de habitat

# dos bichos urbanos
ant_urb = midata2$Forearm[midata2$Habitat == "Urban"]

# dos bichos periurbanos
ant_per = midata2$Forearm[midata2$Habitat == "Periurban"]

# dos bichos da floresta
ant_flo = midata2$Forearm[midata2$Habitat == "Forest"]


# o histograma base é o mesmo
X11()
hist(midata2$Forearm, nclass = 15, probability = TRUE,
     xlim = c(40,75), ylim = c(0,0.12), col = "grey80",
     cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5,
     xlab = "Comprimento do antebraço (mm)",
     ylab = "Frequência", main = " Histograma")
rug(jitter(midata2$Forearm), col = "red")

box()


# Adicionar as medias 

# Urbanos
abline(v = mean(ant_urb), col = "darkblue",
       lty = 3, lwd = 3)
# texto
text(x = 62.5, y = 0.09, labels = "Urbano = 61.97",
     adj = c(0,1), cex = 0.9)

# Periurbanos
abline(v = mean(ant_per), col = "maroon",
       lty = 3, lwd = 3)
# texto
text(x = 52, y = 0.09, labels = "Perirbano = 51.81",
     adj = c(0,1), cex = 0.9)


# florestais
abline(v = mean(ant_flo),
       col = "green4", lty = 3, lwd = 3)
# texto
text(x = 51.4, y = 0.08, labels = "Florestal = 51.18",
     adj = c(0,1), cex = 0.9)


# Adicionar as curvas gaussianas (Slide 43)
# (na mesma cor que as medias)

# urbana 
curve(expr = dnorm(x, mean = mean(ant_urb),
                   sd = sd(ant_urb)),
      add = TRUE, col = "darkblue", lwd = 2)

# Periurbana
curve(expr = dnorm(x, mean = mean(ant_per),
                   sd = sd(ant_per)),
      add = TRUE, col = "maroon", lwd = 2)

# Florestal
curve(expr = dnorm(x, mean = mean(ant_flo),
                   sd = sd(ant_flo)),
      add = TRUE, col = "green4", lwd = 2)



#######################################################
# Jitter plot do slide 44

# primeiro faço um plot vazio
plot(0, 0, cex = 0, ylim = c(40,75), xlim = c(0,1),
     cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5,
     xaxt = "n", xlab = " ",
     ylab = "Comprimento do antebraço (mm)",
     main = " Jitter plot")
box()

# agora adiciono os pontos

# Urbano
points(ant_urb~jitter(rep(0.5, length(ant_urb)),25),  
       pch = 21, cex = 2.5, col = "black", lwd = 2,
       bg = "darkblue")

# Periurbano
points(ant_per~jitter(rep(0.5, length(ant_per)),25),  
       pch = 21, cex = 2.5, col = "black", lwd = 2,
       bg = "maroon")

# Florestal
points(ant_flo~jitter(rep(0.5, length(ant_flo)),25),  
       pch = 21, cex = 2.5, col = "black", lwd = 2,
       bg = "green4")



##########################################################
# Finalmente o boxplot

boxplot(Forearm ~ Habitat, data = midata2,
        cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5,
        xlab = " Habitat ", main = "Boxplot",
        ylab = "Comprimento do antebraço (mm)",
        col = c("green4", "maroon", "darkblue"),
        pch = 16, lwd = 2)





###########################################################
#
#   DESAFÍO
#   Explorem graficamente a ideia de que
#   Ha diferencias na masa corporal de acordo com o sexo
#
###########################################################



## QUEM GOSTA DE CORES DIFERENTES, PODE TESTAR
## A FUNÇÃO rgb() PARA SELECIONAR A COR DO GRÁFICO
## SÓ ESCREVELA DENTRO DO ARGUMENTO col = ... DO PLOT




# FIM
