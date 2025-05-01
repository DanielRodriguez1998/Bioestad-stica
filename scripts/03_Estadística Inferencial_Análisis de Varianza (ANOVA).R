
###############################################################
# LIBRERÍAS NECESARIAS
###############################################################
# install.packages("agricolae")
# install.packages("car")
# install.packages("multcomp")
# install.packages("lsmeans")
# install.packages("emmeans")
# install.packages("DescTools")

library(agricolae)
library(car)
library(multcomp)
library(lsmeans)
library(emmeans)
library(DescTools)

###############################################################
# ANOVA DE UN SOLO FACTOR
###############################################################
X <- c(4.7, 4.9, 5, 4.8, 4.7)
Y <- c(4.6, 4.4, 4.3, 4.4, 4.1, 4.2)
Z <- c(4.8, 4.7, 4.6, 4.4, 4.7, 4.8)
W <- c(4.9, 5.2, 5.4, 5.1, 5.6)
resultado_anova <- aov(c(X, Y, Z, W) ~ rep(c("X", "Y", "Z", "W"), c(5, 6, 6, 5)))
summary(resultado_anova)

###############################################################
# ANOVA CON DATASET EXTERNO (Dietas)
###############################################################
# dietas <- read.csv("dietas.csv", header = TRUE)
# anova1 <- aov(incrementopeso ~ Dietas, data = dietas)
# summary(anova1)
# leveneTest(incrementopeso ~ Dietas, data = dietas, center = "median")
# bartlett.test(incrementopeso ~ Dietas, data = dietas)
# TukeyHSD(anova1, "Dietas")
# scheffe.test(anova1, "Dietas")
# duncan.test(anova1, "Dietas", alpha = 0.05)

###############################################################
# ANOVA BIFACTORIAL
###############################################################
Sexo <- rep(c("Machos", "Hembras"), each = 12)
Especie <- rep(c("Sp1", "Sp2", "Sp3"), times = 8)
Medicion <- c(21.5, 14.5, 16, 19.6, 17.4, 20.3, 20.9, 15, 18.5, 22.8, 17.8, 19.3,
              14.8, 12.1, 14.4, 15.6, 11.4, 14.7, 13.5, 12.7, 13.8, 16.4, 14.5, 12)
datos_bifactorial <- data.frame(Sexo, Especie, Medicion)
resultado_anova_bifactorial <- aov(Medicion ~ Sexo * Especie, data = datos_bifactorial)
summary(resultado_anova_bifactorial)

###############################################################
# ANOVA EN BLOQUES
###############################################################
data4 <- data.frame(
  Persona = rep(c("A", "B", "C", "D", "E", "F"), each = 4),
  Peso = rep(c("Sin peso", "1 kg", "2 kg", "5 kg"), times = 6),
  Rango_Articular = c(180, 178, 130, 81, 171, 170, 99, 65, 180, 180, 105, 72,
                      179, 120, 102, 75, 177, 180, 110, 83, 179, 175, 170, 98)
)
modelo <- aov(Rango_Articular ~ Peso + Error(Persona), data = data4)
summary(modelo)
DunnettTest(x = data4$Rango_Articular, g = data4$Peso)

###############################################################
# ANCOVA
###############################################################
datos <- data.frame(
  IMC = c(32, 30, 31, 32, 31, 30, 30, 32, 33, 31, 33, 32, 34, 35, 33, 34, 32, 33, 34, 35),
  Peso = c(75, 79, 75, 72, 76, 80, 83, 79, 81, 78, 75, 70, 73, 72, 71, 73, 76, 78, 72, 74),
  Variable_Dependiente = c(1.3, 2, 0.9, 2.5, 1.4, 0.9, 1.2, 0.8, 1, 0.9, 2, 1.5, 1.3, 1.9,
                           1.4, 1.2, 1.6, 1.5, 0.9, 1),
  Sexo = rep(c("H", "M"), each = 10),
  Tratamiento = rep(1:2, each = 5)
)
modelo_lm <- lm(Variable_Dependiente ~ IMC + Peso + Sexo + Tratamiento, data = datos)
anova(modelo_lm)

###############################################################
# MANOVA
###############################################################
data1 <- data.frame(
  Medicamento = rep(c("A", "B"), each = 5),
  D1_Glucosa = c(236, 200, 125, 145, 100, 120, 140, 160, 100, 89),
  D1_HB = c(207.5, 340, 179.5, 356, 131, 131, 169.5, 73, 124.5, 105.5),
  D2_Glucosa = c(189, 300, 156, 224.5, 102, 109, 207, 94, 89, 100),
  D2_HB = c(118, 260, 185.5, 124.5, 134, 118, 169.5, 73, 124.5, 105.5),
  D3_Glucosa = c(159, 230, 123, 204, 99, 120, 169, 150, 127, 109),
  D3_HB = c(131, 169.5, 73, 124.5, 105.5, 185.5, 224.5, 134, 118, 118)
)
manova_result <- manova(cbind(D1_Glucosa, D1_HB, D2_Glucosa, D2_HB, D3_Glucosa, D3_HB) ~ Medicamento, data = data1)
summary(manova_result, test = "Pillai")
