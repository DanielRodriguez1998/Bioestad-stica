
###############################################################
# LIBRERÍAS NECESARIAS
###############################################################
# install.packages("DescTools")
# install.packages("multcomp")
# install.packages("emmeans")
library(DescTools)
library(multcomp)
library(emmeans)

###############################################################
# CHI-CUADRADA SIMPLE
###############################################################
observados <- c(152, 39, 53, 6)
esperados <- c(140.6, 46.9, 46.9, 15.6)
chisq.test(observados, p = esperados / sum(esperados))

###############################################################
# KOLMOGOROV-SMIRNOV
###############################################################
observados <- c(5, 5, 3, 1, 1)
esperados <- c(3, 3, 3, 3, 3)
ecdf_observada <- cumsum(sort(observados)) / sum(observados)
ecdf_esperada <- cumsum(esperados) / sum(esperados)
diferencias <- abs(ecdf_observada - ecdf_esperada)
max(diferencias)

###############################################################
# MCNEMAR
###############################################################
datos <- matrix(c(16, 18, 12, 4), nrow = 2, byrow = TRUE)
mcnemar.test(datos)

###############################################################
# WILCOXON
###############################################################
x <- c(142,140,144,144,142,146,149,150,142,148)
y <- c(138,136,147,139,146,141,143,145,136,146)
wilcox.test(x, y, paired = TRUE)

###############################################################
# MANN-WHITNEY
###############################################################
grupo1 <- c(118.6,120.1,122,124.1,126.5,128.8,129.6)
grupo2 <- c(121.5,123.4,123.8,124.3,130.2,130.8)
wilcox.test(grupo1, grupo2)

###############################################################
# COCHRAN
###############################################################
datos <- data.frame(Grupo1=c(1,1,0,1,0,0,0,0), Grupo2=c(0,1,0,1,1,1,0,0),
                    Grupo3=c(0,1,0,0,1,0,1,1), Grupo4=c(0,1,1,1,1,0,1,1),
                    Grupo5=c(1,1,0,0,1,1,1,0))
# Limpiar filas con mismos valores
datos_limpio <- datos[!apply(datos,1,function(r) all(r==r[1])),]

###############################################################
# FRIEDMAN
###############################################################
datos <- data.frame(Trat1=c(1.5,1.4,1.4,1.2,1.4), Trat2=c(2.7,2.9,2.1,3.0,3.3),
                    Trat3=c(2.2,2.2,2.4,2.0,2.5), Trat4=c(1.3,1.0,1.1,1.3,1.5))
friedman.test(as.matrix(datos))

###############################################################
# KRUSKAL-WALLIS
###############################################################
datos <- data.frame(Grupo1=c(14,12.1,19.6,8.2), Grupo2=c(8.4,5.1,7.3,6.6),
                    Grupo3=c(6.9,5.3,5.8,4.1))
grupos <- rep(1:3, each=4)
kruskal.test(unlist(datos), g=grupos)

###############################################################
# ANCOVA
###############################################################
data2 <- data.frame(Covariable=rep(40:60,each=2),
                    Turno=rep(c("Matutino","Vespertino"),each=9),
                    Metodo=as.factor(rep(1:3)),
                    X=runif(18,40,100))
modelo_ancova <- lm(X ~ Metodo * Turno + Covariable, data = data2)
anova(modelo_ancova)

###############################################################
# SHAPIRO TEST EJEMPLO
###############################################################
antes <- c(214,362,202,158,403,219,307,331)
despues <- c(232,276,224,412,562,203,340,313)
shapiro.test(antes)
shapiro.test(despues)
