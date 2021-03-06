############################################
############################################
##                                        ##
##        5. REGRESS�O LINEAR             ##
##     Guillermo L. Fl�rez Montero        ## 
##                                        ##
############################################
############################################




#############################################

# Vamos precisar desses pacotes

library(readr)


#############################################

# Trabalharemos com os dados de aves_urbanas.csv
# N�o esque�a de ler os metadados

aves = read_csv("https://raw.githubusercontent.com/gflorezm/SBio2020/master/aves_urbanas.csv")



# Vamos usar a fun��o lm()

mod_div = lm(Shannon ~ Area, data = aves)

# a fun��o summary devolve um resumo do modelo linear (igual que com ANOVA)

summary(mod_div)

# Podemos entender todos os elementos no summary?



# Podem at� fazer o anova do modelo linear que compara o modelo de regres�o
# com uma hip�tese nula

summary(aov(mod_div))


# a fun��o plot devolve coisas interessantes do modelo

plot(mod_div)



# Posso extrair os coeficientes para o gr�fico

COEF = coefficients(mod_div)

# E tamb�m o intervalo de confian�a dos coeficientes

COEFIC = confint(mod_div)



# Agora fazemos um gr�fico com a reta de regress�o 
# e o intervalo  de confian�a da reta


X11()
plot(0,0,
     cex.lab = 1.5, cex.axis = 1.2,
     xlim = c(0,200), ylim = c(1,4),
     xlab = "�rea do parque (ha)",
     ylab = "�ndice de Diversidade de Shannon")
rect(par("usr")[1], par("usr")[3], par("usr")[2], 
     par("usr")[4], col = "grey90")
points(Shannon ~ Area, data = aves,
       pch = 21, cex = 2.5, lwd = 1.5,
       bg = rgb(234,27,96, alpha = 160,
                maxColorValue = 255))

# Adicionamos a reta
curve(COEF[1]+(COEF[2]*x), add = TRUE, lwd = 3)

# Adicionamos o intervalo de confian�a
curve(COEFIC[1,1]+(COEFIC[2,1]*x), add = TRUE, 
      col="BLUE", lty = 3, lwd = 3)
curve(COEFIC[1,2]+(COEFIC[2,2]*x), add = TRUE, 
      col = "Blue", lty = 3, lwd = 3)




######### FIM #####


# Podem brincar com os seus dados
