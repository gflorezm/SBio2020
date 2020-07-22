#################################################
##                                             ##
##  3. INFERÊNCIA ESTATÍSTICA I                ##
##  Guillermo L. Flórez Montero                ##
##                                             ##
#################################################

# Diretório de trabalho

setwd("D:/Cursos/Semana da Bio 2020/datos")




##############################################################
#
# Vamos criar uma realidade, um universo onde 
# conhecemos o tamanho da população estatística (5000000)
# e os valores reais de media (5) e desvio padrão (3)


universo = rnorm(5000000, mean = 5, sd = 3)
mean(universo)
sd(universo)




# Agora, vamos esquecer a origem de tudo (DARK) e vamos fazer o papel do cientista
# vamos amostrar essa população para tentar estimar a media populacional (Parâmetro)
# a partir da media amostral (estimador do parâmetro).



# Vamos fazer 150 amostras independentes de 120 individuos cada uma.

# Antes de fazer a amostragem, vamos criar uma matriz para armazenar as amostras

amostras = matrix(nrow = 120, ncol = 150)

# Tradução da linha 39
# R... faz uma matriz de 120 linhas e 150 colunas
# e armazena-a como um objeto chamado "amostras"

View(amostras)  # é uma matriz vazia que depois vamos preencher


# Agora vamos fazer a magia da amostragem, para isso vamos usar um loop
# por que gente... são 150 amostras e se fazemos uma a uma vai dar mínimo
# 150 linhas de código.


for (i in 1:150)
{
   amostras[,i] = sample(universo, size = 120, replace = FALSE)
   }


# Olhem isso gente, vamos tentar traduzir. O loop diz:

# Amigo R. você vai fazer isso i vezes, 
# sendo que i vai de 1 a 150 (o número de amostras)

# Entre as chavetas está a ordem que vai executar 150 vezes
# a ordem é: faz uma amostra do objeto que chamas-se "universo",
# de tamanho 120 e sem substituição (bicho que saiu não se seleciona mais)
# salva na coluna i (que ele sabe que vai de 1 a 150) da matriz "amostras"


# Agora vamos calcular as medias de todas as amostras

medias_s = apply(amostras,2, mean)

# podemos explorar esa distribuição de medias

media_m = mean(medias_s)
median(medias_s)
min(medias_s)
max(medias_s)

# E vamos salvar nossos percentis que representam os extremos de incerteza
# tanto para o lado esquerdo quanto para o direito da Normal
# de 0.025 e de 0.975
# somando esses recortes dá 0,05 que é meu valor de alfa.

q.025 = quantile(medias_s, probs = 0.025)
q.975 = quantile(medias_s, probs = 0.975)

q.025
q.975


# Podemos fazer um histograma das medias e ver o que deu

hist(medias_s, probability = TRUE,
     col = "grey80", nclass = 10,
     xlim = c(4,6),
     cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5,
     xlab = " ", ylab = " ", main = " ")
box()

# Adicionamos uma Normal   

curve(expr = dnorm(x, mean = media_m,
                   sd = sd(medias_s)),
      add = TRUE, col = "darkred")

# adicionamos a media das medias
abline(v = media_m, col = "darkred", lty = 3, lwd = 3)

#Adicionamos os percentis que representam os extremos de incerteza
# aquela que eu definí como alfa = 0,05

abline(v = q.975, col = "red")
abline(v = q.025, col = "red")



hist(d_ef, probability = TRUE,
     ylim = c(0,0.40), col = "grey80",
     xlim = c(-10, 6), cex = 0.1)
lines(density(d_ef), col = "blue", lwd = 2)
curve(expr = dt(x, df = 28, log = FALSE),
      add = TRUE, lwd = 2)

lines(y = c(0.05, 0.05),
      x = c(-11, qt(0.975, df = 28)), col = "red", lwd = 2)
lines(y = c(0, 0.05),
      x = c(qt(0.975, df = 28), qt(0.975, df = 28)), col = "red", lwd = 2)
box()

abline(v = qt(0.025, df = 28, lower.tail = TRUE))









########################################
#
#   VAMOS TENTAR UM TESTE DE HIPÓTESES
#
########################################


#
# A NOSSA HIPÓTESE É QUE BICHOS URBANOS SÃO
# MAIORES DO QUE BICHOS FLORESTAIS
# USAMOS A MEDIDA DE ANTEBRAÇO
#




# Usamos os dados de a_lituratus_2

d_alit = read.csv("alituratus2.csv")

# d_alit: dados de Artibeus lituratus

# Lembramos que H0 é de que os antebraços dos animais urbanos e florestais são iguais

# para facilitar o assunto, separemos os dados em dois objetos diferentes (um para cada habitat)

forest = d_alit$Forearm[d_alit$Habitat == "Forest"]
urban = d_alit$Forearm[d_alit$Habitat == "Urban"]


# Vamos fazer o teste que é muito simples

# Lembrem que o valor de t é uma diferença de medias dos antebraços
# dos bichos urbanos e florestais, aproximada a uma distribuição teórica t student
# que asume a hipótese nula como certa

teste_t = t.test(urban, forest)

teste_t

## Graficamos a distribuição t student para 150 graus de liberdade

curve(dt(x, df = 150), 
      cex.lab = 1.5, cex.axis = 1.2, lwd = 2,
      xlim = c(-22,22), ylim = c(0,0.5),
      xaxt = "n", xlab = "t", ylab = "p(x)")
axis(side = 1, at = seq(-20,20,5), cex.axis = 1.2)

# adicionamos os pontos de corte para alfa de 0,05

# Primeiro o inferior
abline(v = qt(0.025, df = 150, 
              lower.tail = TRUE), col = "Darkred")

# agora o superior
abline(v = qt(0.975, df = 150, 
              lower.tail = TRUE), col = "Darkred")

# agora vamos colocar o valor de t (diferência de medias padronizada)

points(x = teste_t$statistic, y = teste_t$p.value,
       pch = 16, col = "Darkred")





##############################################
#  Intervalos de confiança usando t Student  #
##############################################


# Para finalizar, vamos olhar os testes de hipóteses da perspectiva dos intervalos de confiança


# Vamos aos antebraços de Artibeus lituratus
# Posso usar Intervalos de Confiança para ver se as médias são parecidas
# Se os intervalos se tocam em algum ponto então as médias são probabilísticamente parecidas
# por tanto não confiaria muito na minha hipótese alternativa


# Vamos fazer tudo passo a passo

# Calculamos as médias de cada habitat e os guardar em um vetor

medias = c(mean(forest), mean(urban))

# Calculamos o desvio padrão de cada habitat e os guardamo em um vetor

desvios = c(sd(forest), sd(urban))

# Contamos as observações de cada tipo de habitat e os colocamos em um vetor

numero = c(length(forest), length(urban))

# Calculamos o erro padrão de cada média e os colocamos em um vetor

errorest = desvios/sqrt(numero)

# Observe que em todos estes objetos, primeiro está o valor para "Forest" e depois para "Urban"
# Criamos um objeto com os nomes dos habitats, porque os usarei mais tarde

Habitat = c("Forest", "Urban")


# Calcula o intervalo de confiança usando o valor de t para alfa = 0,025 (lembrem-se que é bicaudal)

# primeiro o valor de t crítico.

tcrit = c(qt(0.025, numero[1]-1, lower.tail = FALSE),
          qt(0.025, numero[2]-1, lower.tail = FALSE))

# intervalo superior
icsup = medias + (tcrit * errorest)

# intervalo inferior
icinf = medias - (tcrit * errorest)

# Vamos a colocar em uma tabela para ver melhor

IC_habitat = data.frame("Habitat" = Habitat, "Media Antebraço" = medias,
                        "IC inferior" = icinf, "IC Superior" = icsup)

IC_habitat

#comparemos com o test t
t.test(urban, forest)


### Um gráfico de médias e intervalos de confiança ###

# é mais fácil ver se os intervalos se tocam usando un gráfico

# Criamos um gráfico vazío (este é meu estilo de fazer)
bp1 = barplot(c(0,0) ~ Habitat,
              ylim = c(50, 65),
              ylab = "Forearm length (mm)",
              xlab = "Hábitat",
              cex.lab = 1.2, font.lab = 2)

# adicionamos os pontos que serão as medias
points(bp1, medias, pch = 19, cex = 1.2)

# adicionamos os intervalos de confiança como setinhas
arrows(bp1, icsup, bp1, icinf, code = 3, angle = 90, length = 0.07)

box() #coloque o gráfico em uma caixa





# Feito, agora podem brincar com os dados de CAVALOS
# Mas ai tem um truque....
# O análise é focado a verificar se a diferença de massa
# em media é igual a Zero ou não


#
# FIM
#

