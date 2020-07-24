############################################
############################################
##                                        ##
##        5. REGRESSÃO LINEAR             ##
##     Guillermo L. Flórez Montero        ## 
##                                        ##
############################################
############################################




#############################################

# Vamos precisar desses pacotes

library(readr)


#############################################

# Trabalharemos com os dados de aves_urbanas.csv
# Não esqueça de ler os metadados

aves = read_csv("https://raw.githubusercontent.com/gflorezm/SBio2020/master/aves_urbanas.csv")



# Vamos usar a função lm()

mod_div = lm(Shannon ~ Area, data = aves)

# a função summary devolve um resumo do modelo linear (igual que com ANOVA)

summary(mod_div)

# Podemos entender todos os elementos no summary?



# Podem até fazer o anova do modelo linear que compara o modelo de regresão
# com uma hipótese nula

summary(aov(mod_div))


# a função plot devolve coisas interessantes do modelo

plot(mod_div)



# Posso extrair os coeficientes para o gráfico

COEF = coefficients(mod_div)

# E também o intervalo de confiança dos coeficientes

COEFIC = confint(mod_div)



# Agora fazemos um gráfico com a reta de regressão 
# e o intervalo  de confiança da reta


X11()
plot(0,0,
     cex.lab = 1.5, cex.axis = 1.2,
     xlim = c(0,200), ylim = c(1,4),
     xlab = "Área do parque (ha)",
     ylab = "Índice de Diversidade de Shannon")
rect(par("usr")[1], par("usr")[3], par("usr")[2], 
     par("usr")[4], col = "grey90")
points(Shannon ~ Area, data = aves,
       pch = 21, cex = 2.5, lwd = 1.5,
       bg = rgb(234,27,96, alpha = 160,
                maxColorValue = 255))

# Adicionamos a reta
curve(COEF[1]+(COEF[2]*x), add = TRUE, lwd = 3)

# Adicionamos o intervalo de confiança
curve(COEFIC[1,1]+(COEFIC[2,1]*x), add = TRUE, 
      col="BLUE", lty = 3, lwd = 3)
curve(COEFIC[1,2]+(COEFIC[2,2]*x), add = TRUE, 
      col = "Blue", lty = 3, lwd = 3)




######### FIM #####


# Podem brincar com os seus dados
