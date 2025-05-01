
###############################################################
# PROBLEMA 1: Distribución de Poisson
###############################################################
lambda <- 2
intervalo_tiempo <- 3
prob_al_menos_3 <- 1 - ppois(2, lambda * intervalo_tiempo)
cat("Probabilidad al menos 3 pacientes:", prob_al_menos_3, "\n")

###############################################################
# PROBLEMA 2: Distribución Binomial
###############################################################
n <- 15; p <- 0.75; k_values <- 0:15
probabilidades <- dbinom(k_values, size=n, prob=p)
for (i in 1:length(k_values)) {
  cat("Probabilidad de", k_values[i], "personas:", probabilidades[i], "\n")
}

###############################################################
# PROBLEMA 3: Binomial acumulada
###############################################################
n <- 400; p <- 0.10
prob_total <- pbinom(55, n, p) - pbinom(44, n, p)
cat("Probabilidad 45<=x<=55:", round(prob_total,4), "\n")

###############################################################
# PROBLEMA 4: Normalidad y varianza
###############################################################
muestra <- c(132,133,91,108,67,169,54,203,190,133,96,130,187,121,163,166,104,110,157,138)
shapiro.test(muestra)
var(muestra)
sd(muestra)

###############################################################
# PRUEBA CHI-CUADRADO VARIANZA
###############################################################
sigma_poblacion <- 45; n <- length(muestra)
chi_stat <- (n-1) * var(muestra) / sigma_poblacion^2
df <- n -1
pchisq(chi_stat, df)

###############################################################
# PROBLEMA 5: t-student muestras independientes
###############################################################
n1 <- 31; x1 <-199; s1 <-28
n2 <-31; x2 <-172; s2 <-21
alpha <-0.01
t_stat <- (x1 - x2) / sqrt(s1^2/n1 + s2^2/n2)
df <-n1 +n2 -2
p_value <-pt(t_stat, df, lower.tail=FALSE)
cat("t-stat:", round(t_stat,4), "p-value:", round(p_value,4), "\n")

###############################################################
# PROBLEMA 6: Prueba F
###############################################################
grupo1 <- c(202.7,199.5,200.5,201.3,199.9,199.9,199.4,199.9,197.8,200.3)
grupo2 <- c(7.3,8,7.8,9,8.6,8.6,7.9,6.8,7.9,9.4)
var1 <- var(grupo1); var2 <- var(grupo2)
F_stat <- var1 / var2
cat("F-stat:", F_stat, "\n")

###############################################################
# PROBLEMA 7: Diferencia de proporciones
###############################################################
n1 <-50; x1 <-18; n2 <-50; x2 <-20
p1 <-x1/n1; p2 <-x2/n2; p <- (x1 + x2)/(n1 + n2)
z_stat <- (p1 - p2) / sqrt(p*(1-p)*(1/n1 +1/n2))
cat("z-stat:", round(z_stat,4), "\n")

###############################################################
# PROBLEMA 8: t pareado
###############################################################
antes <- c(74,84,81,110,105,100,110,68,79,86)
despues <- c(115,140,176,191,158,180,179,140,167,157)
t.test(despues, antes, paired=TRUE)
