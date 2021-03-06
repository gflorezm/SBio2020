#################################################
##                                             ##
##  3. Introdu��o � ANOVA                      ##
##  Guillermo L. Fl�rez Montero                ##
##                                             ##
#################################################

# se n�o tem o pacote vioplot use a fun��o
# install.packages()

library(vioplot)



###################################################
# Vamos recriar o microuniverso da aula de ANOVA  #
###################################################


# Quer se testar o efeito de dois f�rmacos sobre a libera��o de proteina W
# no sangue. selecionou-se uma amostra de 60 pacientes saud�ves.
# se dividiram em 3 grupos de 20 pacientes tratados assim:

# Grupo_A = F�rmaco A
# Grupo_B = F�rmaco B
# Grupo_C = Placebo


set.seed(1234)  # Para manter a replicabilidade

# permite que ao rodar o script de novo as simula��es dem os mesmos valores

grupo_C = rnorm(20, mean = 11, sd = 3.2)
grupo_B = rnorm(20, mean = 13, sd = 2.9)
grupo_A = rnorm(20, mean = 18, sd = 2.8)


# Vamos criar um vetor com os dados juntos e outro com os nomes dos tratamentos
# isso facilita o uso de algumas fun��es por estar organizados de forma apropriada
# uma vari�vel em cada coluna...

Proteina_W = c(grupo_C, grupo_B, grupo_A)
Tratamento = rep(c("Placebo", "Farmaco_B", "Farmaco_A"),
                 each = 20)







#########################################################
#
# PRIMEIRO FAZEMOS UMA EXPLORA��O DOS DADOS
#
#########################################################


# M�dias

med_C = mean(grupo_C)
med_B = mean(grupo_B)
med_A = mean(grupo_A)
med_geral = mean(Proteina_W)
# Desvio padr�o

dp_C = sd(grupo_C)
dp_B = sd(grupo_B)
dp_A = sd(grupo_A)
dp_geral = sd(Proteina_W)


# Podemos graficar as hip�teses como histogramas

# Ho: todas as m�dias s�o iguais

hist(Proteina_W, nclass = 15, probability = TRUE,
     xlim = c(3,25), ylim = c(0,0.2), col = "palegreen",
     cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5,
     xlab = "Proteina W em Sangue",
     ylab = "Frequ�ncia", main = " ")
box()

# media geral
abline(v = med_geral, lty = 3, lwd = 3,
       col = "darkred")

# Normal
curve(expr = dnorm(x, mean = med_geral, sd = dp_geral),
      add = TRUE, col = "darkred", lwd = 3)



# Ha: pelo menos uma das m�dias � diferente

hist(Proteina_W, nclass = 15, probability = TRUE,
     xlim = c(3,25), ylim = c(0,0.2), col = "palegreen",
     cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5,
     xlab = "Proteina W em Sangue",
     ylab = "Frequ�ncia", main = " ")
box()

# media placebo
abline(v = med_C, lty = 3, lwd = 3,
       col = "grey20")

# media f�rmaco B
abline(v = med_B, lty = 3, lwd = 3,
       col = "Turquoise3")

# media Farmaco A
abline(v = med_A, lty = 3, lwd = 3,
       col = "tomato2")


# Normal Placebo
curve(expr = dnorm(x, mean = med_C, sd = dp_C),
      add = TRUE, col = "grey20", lwd = 3)

# Normal F�rmaco B
curve(expr = dnorm(x, mean = med_B, sd = dp_B),
      add = TRUE, col = "Turquoise3", lwd = 3)

# Normal F�rmaco A
curve(expr = dnorm(x, mean = med_A, sd = dp_A),
      add = TRUE, col = "Tomato2", lwd = 3)









##########################################
#
#   Podemos fazer uma explora��o gr�fica
#
##########################################



#####################################################

## Jitter plot

# primeiro fa�o um plot vazio
plot(0, 0, cex = 0, ylim = c(5,25), xlim = c(0,1),
     cex.axis = 1.2, cex.lab = 1.2,
     xaxt = "n", xlab = " ", font.lab = 2,
     ylab = "Proteina W em sangue",
     main = " ")
box()

# agora adiciono os pontos


# Placebo
points(grupo_C ~ jitter(rep(0.5, length(grupo_C)),25),  
       pch = 21, cex = 2.5, col = "black", lwd = 2,
       bg = "grey65")


# F�rmaco B
points(grupo_B ~ jitter(rep(0.5, length(grupo_B)),25),  
       pch = 22, cex = 2.5, col = "black", lwd = 2,
       bg = "turquoise3")


# F�rmaco A
points(grupo_A ~ jitter(rep(0.5, length(grupo_A)),25),  
       pch = 24, cex = 2.5, col = "black", lwd = 2,
       bg = "tomato2")



#####################################################

# Boxplot

boxplot(Proteina_W ~ Tratamento,
        cex.axis = 1.2, cex.lab = 1.2,
        xlab = " Tratamento ",
        ylab = "Proteina W em sangue",
        names = c("Farmaco A", "Farmaco B", "Placebo"),
        col = c("tomato2", "turquoise3", "grey65"),
        pch = 16, lwd = 2.5, notch=TRUE)


######################################################

# Violinplot

vioplot(Proteina_W ~ Tratamento,
        cex.axis = 1.5,
        xlab = " ",
        ylab = " ",
        names = c("Farmaco A", "Farmaco B", "Placebo"),
        col = c("tomato2", "turquoise3", "grey65"),
        pch = 16, lwd = 2)
mtext("Tratamento", cex = 1.5, side = 1, line = 3)
mtext("Prote�na W em Sangue", cex = 1.5, side = 2, line = 3)



##################################################################
#
# GR�FICO DOS PONTOS E OS RESIDUOS
# Esse gr�fico fancy aprendi no curso de linguagem R em Ecologia
# oferecido pelo Prof. Alexandre Oliveira no IB-USP em 2016
#
##################################################################

plot(x = 1:60, y = Proteina_W, cex = 1.2,
     pch = rep(c(16,15,17), each = 20),
     col= rep(c("grey30", "turquoise3", "tomato2"), each=20),
     xaxt = "n", cex.axis = 1.2, cex.lab = 1.2,
     xlab = " ", ylab = "Proteina W em sangue")

#Adicionamos as medias

lines(c(1,20),c(med_C, med_C), col = "grey30", lwd = 2)
lines(c(21,40),c(med_B, med_B), col = "turquoise3", lwd = 2)
lines(c(41,60),c(med_A, med_A), col = "tomato2", lwd = 2)

abline(h = med_geral, col = "darkred", lwd = 2) # Opcional

# Agora as linhas dos residuos...

for(i in 1:20)
{
  lines(c(i,i),c(Proteina_W[i],med_C), col= "grey30")
}

for(j in 21:40)
{
  lines(c(j,j),c(Proteina_W[j],med_B), col= "turquoise3")
}

for(k in 41:60)
{
  lines(c(k,k),c(Proteina_W[k],med_A), col= "tomato2")
}


###########################################################################


##################################
#
# C�lculo de ANOVA (na m�o)
#
##################################


# Primeiro calculamos a Variac�o Entre Grupos (dos grupos)

medias = c(med_A, med_B, med_C)

sq_gru = 20*sum((medias - med_geral)^2)


# agora a Varia��o Intra Grupos (dos res�duos)

sq_C = sum((grupo_C - med_C)^2)
sq_B = sum((grupo_B - med_B)^2)
sq_A = sum((grupo_A - med_A)^2)

sq_res = sq_C + sq_B + sq_A



# A soma dos quadrados total �

sq_total = sq_gru + sq_res



# Agora calculamos F

# primeiro os quadrados medios com 2 e 57 g.l.

qm_gru = sq_gru / 2

qm_res = sq_res / 57


F_farmacos = qm_gru / qm_res



# Ele tem um valor de p

p_farmacos = pf(F_farmacos,2,57, lower.tail=FALSE)
  
  
  
  

# VAMOS VER ISSO NUM GR�FICO.... 
  
  
curve(expr=df(x, 2,57), lwd = 2,
      main="Distribui��o F de Fisher (df=2,57)",
      xlab="Valor de F", ylab="Densidade Probabil�stica",
      xlim=c(-0,33))
abline(h = 0, lty = 2)


# Um corte no F cr�tico para alfa = 0.05

abline(v = qf(0.05, 2, 57, lower.tail = FALSE), col="darkred")

# adicionamos nosso valor de F emp�rico

points(x = F_farmacos, y = 0, pch = 16, col = "blue")
  
####################################################################  
  




# L�gico que tem uma forma simples...

anova_tc = aov(Proteina_W ~ Tratamento)
summary(anova_tc)

# Podemos fazer a an�lise Post-Hoc de diferen�a de medias
# o argumento ordered = TRUE � para visualizar as diferen�as positivas primeiro

tukey_tc = TukeyHSD(anova_tc, ordered = TRUE)


# Outra forma de fazer mas que n�o d� para fazer o teste de Tuckey
oneway.test(Proteina_W ~ Tratamento)




# PODEM BRINCAR COM OS DADOS DE a_lituratus


## FIM
