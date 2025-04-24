# Análisis de Inferencia Estadística con distribuciones discretas y pruebas de hipótesis

# Distribución Poisson: Probabilidad de llegada de pacientes
# Tasa promedio de llegada de pacientes por hora
lambda <- 2

# Intervalo de tiempo en horas
intervalo_tiempo <- 3

# Cálculo de la probabilidad de que lleguen al menos 3 pacientes
prob_al_menos_3 <- 1 - ppois(2, lambda * intervalo_tiempo)

# Mostramos el resultado
cat("La probabilidad de que lleguen al menos 3 pacientes entre las 2pm y las 5pm es:", prob_al_menos_3, "\n")

# Distribución Binomial: Cálculo de probabilidades de éxito en promoción
# Tamaño de la muestra
n <- 15

# Probabilidad de éxito
p <- 0.75

# Números posibles de éxitos
k_values <- 0:15

# Calculamos la probabilidad para cada valor de k
probabilidades <- dbinom(k_values, size = n, prob = p)

# Mostramos los resultados
for (i in 1:length(k_values)) {
  cat("Probabilidad de", k_values[i], "personas esperando una promoción:", probabilidades[i], "\n")
}




# Distribución Binomial: Rango de muestras contaminadas
# Tamaño de la muestra
n <- 400

# Probabilidad de contaminación
p <- 0.10

# Números posibles de muestras contaminadas
x_values <- 45:55

# Calculamos la probabilidad para cada valor de x
probabilidades1 <- pbinom(44, size=n, prob = p)
probabilidades2 <- pbinom(55, size=n, prob = p)
prob_total<-probabilidades2-probabilidades1
prob_total
probabilidades2
probabilidades1
# Mostramos el resultado
cat("La probabilidad de que 45 <= x <= 55 muestras estén contaminadas es:", round(prob_total, 4), "\n")







# Prueba de normalidad y prueba F para comparación de varianzas
# Datos proporcionados
muestra <- c(132, 133, 91, 108, 67, 169, 54, 203, 190, 133, 96, 130, 187, 121, 163, 166, 104, 110, 157, 138)
sigma_poblacion <- 45
nivel_confianza <- 0.99
varianza<-var(muestra)

# Tamaño de la muestra
n <- length(muestra)


# Test de Shapiro-Wilk
shapiro_test_result <- shapiro.test(muestra)

# Mostrar el resultado
cat("Estadístico de prueba:", round(shapiro_test_result$statistic, 4), "\n")
cat("Valor p:", round(shapiro_test_result$p.value, 4), "\n")
cat("Varianza de la muestra:", round(varianza, 4), "\n")
cat("desviación estandar:", round(sqrt(varianza), 4), "\n")


# Interpretación
if (shapiro_test_result$p.value < 0.05) {
  cat("Resultado: Se rechaza la hipótesis nula. Los datos no siguen una distribución normal.\n")
} else {
  cat("Resultado: No se rechaza la hipótesis nula. Los datos parecen seguir una distribución normal.\n")
}


# Estadístico de prueba F
s_muestra <- sd(muestra)
F <- (s_muestra^2) / (sigma_poblacion^2)

# Intervalo de confianza para la varianza de la muestra
alpha <- 1 - nivel_confianza
IC <- c(((n - 1) * s_muestra^2) / qchisq(1 - alpha/2, df = n - 1), ((n - 1) * s_muestra^2) / qchisq(alpha/2, df = n - 1))


# Valor p
p_value <- pf(F, df1 = n - 1, df2 = Inf)

# Mostrar el valor p
cat("Valor p:", round(p_value, 4), "\n")
# Mostrar resultados
cat("Estadístico de prueba F:", F, "\n")
cat("Intervalo de confianza del 99% para la varianza de la muestra:", round(sqrt(IC[1]), 4), ",", round(sqrt(IC[2]), 4), "\n")

# Regla de decisión
if (F < IC[1] | F > IC[2]) {
  cat("Decisión: Rechazar H0\n")
} else {
  cat("Decisión: No rechazar H0\n")
}

# Conclusión
cat("Conclusión: Con un nivel de confianza del 99%, no hay suficiente evidencia para concluir que la desviación estándar de la muestra es menor que la de la población.\n")







# Prueba Chi-cuadrado: Evaluación de varianza con varianza poblacional conocida
# Datos proporcionados
muestra <- c(132, 133, 91, 108, 67, 169, 54, 203, 190, 133, 96, 130, 187, 121, 163, 166, 104, 110, 157, 138)
sigma_poblacion <- 45
nivel_significancia <- 0.01

# Tamaño de la muestra
n <- length(muestra)
n
# Estadístico de prueba chi-cuadrado para comparación de varianzas
chi_stat <- (n - 1) * var(muestra) / sigma_poblacion^2

# Grados de libertad
df <- n - 1

p_value <- pchisq(chi_stat, df, lower.tail = TRUE)
p_value
# Región crítica
chi_critical <- qchisq(1 - nivel_significancia, df = df)

# Intervalo de confianza
conf_int <- sqrt((n - 1) * var(muestra) / qchisq(c(1 - nivel_significancia/2, nivel_significancia/2), df = df))

# Mostrar resultados
cat("Estadístico de prueba chi-cuadrado:", round(chi_stat, 4), "\n")
cat("Región crítica:", round(chi_critical, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")
# Regla de decisión
if (chi_stat > chi_critical) {
  cat("Decisión: Rechazar H0\n")
} else {
  cat("Decisión: No rechazar H0\n")
}

# Mostrar intervalo de confianza
cat("Intervalo de confianza para la desviación estándar poblacional:", 
    "[", round(sqrt((n - 1) * var(muestra) / qchisq(1 - nivel_significancia/2, df = df)), 4),
    ",", round(sqrt((n - 1) * var(muestra) / qchisq(nivel_significancia/2, df = df)), 4), "]\n")




# Prueba t para comparación de medias entre dos grupos independientes
# Datos proporcionados
n1 <- 31
x1_bar <- 199
s1 <- 28

n2 <- 31
x2_bar <- 172
s2 <- 21

alpha <- 0.01


# Grados de libertad
df <- n1 + n2 - 2

# Cuantil t de Student
t_quantile <- qt(1 - alpha / 2, df)

# Intervalo de confianza
ci_lower <- (x1_bar - x2_bar) - t_quantile * sqrt((s1^2 / n1) + (s2^2 / n2))
ci_upper <- (x1_bar - x2_bar) + t_quantile * sqrt((s1^2 / n1) + (s2^2 / n2))

# Mostrar resultados
cat("Límite inferior del IC (99%):", round(ci_lower, 4), "\n")
cat("Límite superior del IC (99%):", round(ci_upper, 4), "\n")

# Estadístico de prueba t
t_stat <- (x1_bar - x2_bar) / sqrt((s1^2 / n1) + (s2^2 / n2))

# Grados de libertad
df <- n1 + n2 - 2

# Valor crítico
t_critical <- qt(1 - alpha, df)

# Valor p
p_value <- pt(t_stat, df, lower.tail = FALSE)

# Mostrar resultados
cat("Estadístico de prueba t:", round(t_stat, 4), "\n")
cat("Valor crítico:", round(t_critical, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")

# Regla de decisión
if (t_stat > t_critical) {
  cat("Decisión: Rechazar H0\n")
} else {
  cat("Decisión: No rechazar H0\n")
}







# Datos proporcionados
n1 <- 31
x1_bar <- 199
s1 <- 28

n2 <- 31
x2_bar <- 172
s2 <- 21

alpha <- 0.01

# Grados de libertad
df <- n1 + n2 - 2

# Cuantil t de Student
t_quantile <- qt(1 - alpha / 2, df)

# Intervalo de confianza
ci_lower <- (x1_bar - x2_bar) - t_quantile * sqrt((s1^2 / n1) + (s2^2 / n2))
ci_upper <- (x1_bar - x2_bar) + t_quantile * sqrt((s1^2 / n1) + (s2^2 / n2))

# Mostrar resultados
cat("Límite inferior del IC (99%):", round(ci_lower, 4), "\n")
cat("Límite superior del IC (99%):", round(ci_upper, 4), "\n")

# Estadístico de prueba t
t_stat <- (x1_bar - x2_bar) / sqrt((s1^2 / n1) + (s2^2 / n2))

# Grados de libertad
df <- n1 + n2 - 2

# Valor crítico
t_critical <- qt(1 - alpha, df)

# Valor p
p_value <- pt(t_stat, df, lower.tail = FALSE)

# Mostrar resultados
cat("Estadístico de prueba t:", round(t_stat, 4), "\n")
cat("Valor crítico:", round(t_critical, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")

# Regla de decisión
if (t_stat > t_critical) {
  cat("Decisión: Rechazar H0\n")
} else {
  cat("Decisión: No rechazar H0\n")
}

# Conclusión
if (t_stat > t_critical) {
  cat("Conclusión: Hay evidencia suficiente para rechazar la hipótesis nula.\n")
} else {
  cat("Conclusión: No hay suficiente evidencia para rechazar la hipótesis nula.\n")
}








# Datos proporcionados
n1 <- 31
x1_bar <- 199
s1 <- 28

n2 <- 31
x2_bar <- 172
s2 <- 21

alpha <- 0.01

# Grados de libertad
df <- n1 + n2 - 2

# Cuantil t de Student
t_quantile <- qt(1 - alpha / 2, df)

# Intervalo de confianza
ci_lower <- (x1_bar - x2_bar) - t_quantile * sqrt((s1^2 / n1) + (s2^2 / n2))
ci_upper <- (x1_bar - x2_bar) + t_quantile * sqrt((s1^2 / n1) + (s2^2 / n2))

# Mostrar resultados
cat("Límite inferior del IC (99%):", round(ci_lower, 2), "\n")
cat("Límite superior del IC (99%):", round(ci_upper, 2), "\n")

# Estadístico de prueba t
t_stat <- (x1_bar - x2_bar) / sqrt((s1^2 / n1) + (s2^2 / n2))

# Grados de libertad
df <- n1 + n2 - 2

# Valor crítico
t_critical <- qt(1 - alpha, df)

# Valor p
p_value <- pt(t_stat, df, lower.tail = FALSE)

# Mostrar resultados
cat("Estadístico de prueba t:", round(t_stat, 4), "\n")
cat("Valor crítico:", round(t_critical, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")

# Regla de decisión
if (t_stat > t_critical) {
  cat("Decisión: Rechazar H0\n")
} else {
  cat("Decisión: No rechazar H0\n")
}

# Conclusión
if (t_stat > t_critical) {
  cat("Conclusión: Con un nivel de significancia de", alpha, ", hay evidencia suficiente para rechazar la hipótesis nula.\n")
  cat("Por lo tanto, podemos concluir con un 99% de confianza que los resultados promedio obtenidos por el método 1 son mayores a los obtenidos por el método 2.\n")
} else {
  cat("Conclusión: No hay suficiente evidencia para rechazar la hipótesis nula con un nivel de significancia de", alpha, ".\n")
  cat("No podemos concluir que los resultados promedio obtenidos por el método 1 sean mayores a los obtenidos por el método 2 con un 99% de confianza.\n")
}










# Prueba F: Comparación de varianzas entre dos grupos

# Datos proporcionados
# Datos
grupo2 <- c(7.28, 8, 7.76, 9.05, 8.64, 8.56, 7.87, 6.78, 7.94, 9.43, 8.49, 7.68, 9.88, 9.59, 5.96, 7.84, 8.98, 7.9, 7.55, 7.66, 7.37, 7.08, 6.91, 6.66, 7.84 )
grupo1 <- c(202.71, 199.48, 200.52, 201.32, 199.93, 199.9, 199.35, 199.96, 197.84, 200.33, 200.16, 198.61, 199.57, 200.09, 199.57, 197.14, 200.88, 203.07, 200.74, 202.06, 198.43, 203.43, 201.69, 202.58, 202.22, 197.02, 198.74, 198.66, 202.7, 199.14, 195.23)
# Prueba F de Fisher
var_grupo1 <- var(grupo1)
var_grupo2 <- var(grupo2)
var_grupo1
var_grupo2
F_stat <- var_grupo1 / var_grupo2
F_critical__lower<-F_stat/1.94
F_critical_upper<-F_stat/(1/1.89)
F_critical_upper
F_critical__lower
F_stat


# Grados de libertad
df1 <- length(grupo1) - 1
df2 <- length(grupo2) - 1

# Intervalo de confianza para la razón de varianzas
alpha <- 0.05
#F_critical_lower <- qf(alpha / 2, df1, df2)
#F_critical_upper <- qf(1 - alpha / 2, df1, df2)

ci_lower <- var_grupo1 / var_grupo2 * (1 / F_critical_upper)
ci_upper <- var_grupo1 / var_grupo2 * (1 / F_critical_lower)

# Mostrar resultados
cat("Límite inferior del IC (95%):", round(ci_lower, 4), "\n")
cat("Límite superior del IC (95%):", round(ci_upper, 4), "\n")




# Valor p
p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)
p_value
# Mostrar resultados
cat("Estadístico de prueba F:", round(F_stat, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")

# Regla de decisión
if (p_value < 0.05) {
  cat("Decisión: Rechazar H0 (las varianzas son diferentes)\n")
} else {
  cat("Decisión: No rechazar H0 (no hay suficiente evidencia para decir que las varianzas son diferentes)\n")
}

# Prueba de diferencia de proporciones entre dos muestras
# Datos proporcionados
n1 <- 50
x1 <- 18

n2 <- 50
x2 <- 20

# Proporciones observadas
p1 <- x1 / n1
p2 <- x2 / n2

# Proporción global
p <- (x1 + x2) / (n1 + n2)

# Estadístico de prueba z
z_stat <- (p1 - p2) / sqrt(p * (1 - p) * (1/n1 + 1/n2))

# Valor crítico
alpha <- 0.01
z_critical <- qnorm(1 - alpha/2)

# Valor p
p_value <- 2 * (1 - pnorm(abs(z_stat)))



# Nivel de significancia
alpha <- 0.01

# Cuantil z correspondiente al nivel de significancia alpha/2
z_value <- qnorm(1 - alpha/2)

# Intervalo de confianza para la diferencia de proporciones
ci_lower <- (p1 - p2) - z_value * sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
ci_upper <- (p1 - p2) + z_value * sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))

# Mostrar resultados
cat("Límite inferior del IC (99%):", round(ci_lower, 4), "\n")
cat("Límite superior del IC (99%):", round(ci_upper, 4), "\n")


# Mostrar resultados
cat("Estadístico de prueba z:", round(z_stat, 4), "\n")
cat("Valor crítico:", round(z_critical, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")

# Regla de decisión
if (abs(z_stat) > z_critical) {
  cat("Decisión: Rechazar H0\n")
} else {
  cat("Decisión: No rechazar H0\n")
}








# Prueba de diferencia de proporciones entre dos muestras
# Datos proporcionados
n1 <- 50
x1 <- 18

n2 <- 50
x2 <- 20

# Proporciones observadas
p1 <- x1 / n1
p2 <- x2 / n2

# Proporción global
p <- (x1 + x2) / (n1 + n2)

# Estadístico de prueba z
z_stat <- (p1 - p2) / sqrt(p * (1 - p) * (1/n1 + 1/n2))

# Valor crítico
alpha <- 0.01
z_critical <- qnorm(1 - alpha/2)

# Valor p
p_value <- 2 * (1 - pnorm(abs(z_stat)))

# Nivel de significancia
alpha <- 0.01

# Cuantil z correspondiente al nivel de significancia alpha/2
z_value <- qnorm(1 - alpha/2)

# Intervalo de confianza para la diferencia de proporciones
ci_lower <- (p1 - p2) - z_value * sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
ci_upper <- (p1 - p2) + z_value * sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))

# Mostrar resultados
cat("Límite inferior del IC (99%):", round(ci_lower, 4), "\n")
cat("Límite superior del IC (99%):", round(ci_upper, 4), "\n")
cat("Estadístico de prueba z:", round(z_stat, 4), "\n")
cat("Valor crítico:", round(z_critical, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")

# Regla de decisión
if (abs(z_stat) > z_critical) {
  cat("Decisión: Rechazar H0\n")
} else {
  cat("Decisión: No rechazar H0\n")
}

# Conclusión
if (abs(z_stat) > z_critical) {
  cat("Conclusión: Hay evidencia suficiente para rechazar la hipótesis nula.\n")
  cat("Por lo tanto, existen diferencias significativas entre las proporciones reales de niños con gingivitis en ambas escuelas.\n")
} else {
  cat("Conclusión: No hay suficiente evidencia para rechazar la hipótesis nula.\n")
  cat("No podemos concluir que existan diferencias significativas entre las proporciones reales de niños con gingivitis en ambas escuelas.\n")
}








# Prueba t para muestras pareadas: efecto de una intervención en latidos del corazón
# Datos proporcionados
antes <- c(74, 84, 81, 110, 105, 100, 110, 68, 79, 86)
despues <- c(115, 140, 176, 191, 158, 180, 179, 140, 167, 157)

# Realizar la prueba t de muestras pareadas
t_test <- t.test(despues, antes, paired = TRUE, alternative = "greater", conf.level = 0.95)

# Obtener el intervalo de confianza
ci_lower <- t_test$conf.int[1]
ci_upper <- t_test$conf.int[2]

# Mostrar resultados
cat("Estadístico de prueba t:", round(t_test$statistic, 4), "\n")
cat("Valor p:", round(t_test$p.value, 4), "\n")
cat("Intervalo de confianza (95%):", round(ci_lower, 4), "a", round(ci_upper, 4), "\n")

# Regla de decisión
if (t_test$p.value < 0.05) {
  cat("Decisión: Rechazar H0 (hay evidencia suficiente para indicar un aumento)\n")
} else {
  cat("Decisión: No rechazar H0 (no hay suficiente evidencia para indicar un aumento)\n")
}








# Prueba t para muestras pareadas: efecto de una intervención en latidos del corazón
# Datos proporcionados
antes <- c(74, 84, 81, 110, 105, 100, 110, 68, 79, 86)
despues <- c(115, 140, 176, 191, 158, 180, 179, 140, 167, 157)

# Realizar la prueba t de muestras pareadas
t_test <- t.test(despues, antes, paired = TRUE, alternative = "greater", conf.level = 0.95)

# Obtener el intervalo de confianza
ci_lower <- t_test$conf.int[1]
ci_upper <- t_test$conf.int[2]

# Mostrar hipótesis
cat("Hipótesis nula (H0): La condición experimental no aumenta el número de latidos del corazón.\n")
cat("Hipótesis alternativa (Ha): La condición experimental aumenta el número de latidos del corazón.\n\n")

# Mostrar resultados
cat("Estadístico de prueba t:", round(t_test$statistic, 4), "\n")
cat("Valor p:", round(t_test$p.value, 4), "\n")
cat("Intervalo de confianza (95%):", round(ci_lower, 4), "a", round(ci_upper, 4), "\n")

# Regla de decisión
if (t_test$p.value < 0.05) {
  cat("Regla de decisión: Rechazar H0 (hay evidencia suficiente para indicar un aumento)\n")
} else {
  cat("Regla de decisión: No rechazar H0 (no hay suficiente evidencia para indicar un aumento)\n")
}

# Decisión final
if (t_test$p.value < 0.05) {
  cat("Decisión final: Hay evidencia suficiente para rechazar la hipótesis nula.\n")
  cat("Conclusión: La condición experimental aumenta significativamente el número de latidos del corazón.\n")
} else {
  cat("Decisión final: No hay suficiente evidencia para rechazar la hipótesis nula.\n")
  cat("Conclusión: No podemos concluir que la condición experimental aumente significativamente el número de latidos del corazón.\n")
}




# Datos proporcionados
antes <- c(74, 84, 81, 110, 105, 100, 110, 68, 79, 86)
despues <- c(115, 140, 176, 191, 158, 180, 179, 140, 167, 157)

# Paso 1: Calcular las diferencias
diferencias <- despues - antes

# Paso 2: Calcular la media de las diferencias (barra d)
media_diferencias <- mean(diferencias)

# Paso 3: Calcular la desviación estándar de las diferencias (s_d)
desviacion_estandar_diferencias <- sd(diferencias)

# Paso 4: Calcular el tamaño de la muestra (n)
n <- length(diferencias)


# Paso 5: Calcular el estadístico de prueba t
estadistico_t <- (media_diferencias - 0) / (desviacion_estandar_diferencias / sqrt(n))

# Paso 6: Determinar el valor p
valor_p <- pt(estadistico_t, df = n - 1, lower.tail = FALSE)

# Mostrar resultados
cat("Estadístico de prueba t:", round(estadistico_t, 4), "\n")
cat("Valor p:", round(valor_p, 4), "\n")


# Paso 7: Calcular los límites del intervalo de confianza
error_estandar <- desviacion_estandar_diferencias / sqrt(n)
t_valor_critico <- qt(0.975, df = n - 1)  # Obtener el valor crítico de t para alpha/2 y n-1 grados de libertad

limite_inferior <- media_diferencias - t_valor_critico * error_estandar
limite_superior <- media_diferencias + t_valor_critico * error_estandar

# Mostrar resultados del intervalo de confianza
cat("Intervalo de confianza (95%): [", round(limite_inferior, 4), ",", round(limite_superior, 4), "]\n")




# Parámetros
n <- 12  # Tamaño de la muestra
k <- 4   # Número de éxitos deseados
p <- 0.3 # Probabilidad de éxito (conejo inmune)

# Calcular la probabilidad usando la distribución binomial
probabilidad <- choose(n, k) * p^k * (1-p)^(n-k)

# Calcular la media y la desviación estándar
media <- n * p
desviacion_estandar <- n * p * (1 - p)

# Mostrar resultados
cat("La media de la distribución binomial es:", round(media, 4), "\n")
cat("La desviación estándar de la distribución binomial es:", round(desviacion_estandar, 4), "\n")
cat("La probabilidad de obtener exactamente 4 conejos inmunes en una muestra de 12 es:", round(probabilidad, 4), "\n")








# Datos proporcionados
antes <- c(74, 84, 81, 110, 105, 100, 110, 68, 79, 86)
despues <- c(115, 140, 176, 191, 158, 180, 179, 140, 167, 157)

# Paso 1: Calcular las diferencias
diferencias <- despues - antes

# Paso 2: Calcular la media y la desviación estándar de las diferencias
media_diferencias <- mean(diferencias)
desviacion_estandar_diferencias <- sd(diferencias)

# Paso 3: Calcular el estadístico t
t_statistic <- media_diferencias / (desviacion_estandar_diferencias / sqrt(length(diferencias)))

# Paso 4: Determinar los grados de libertad
grados_libertad <- length(diferencias) - 1

# Paso 5: Calcular el valor p
valor_p <- pt(t_statistic, df = grados_libertad, lower.tail = FALSE)

# Mostrar resultados
cat("Estadístico de prueba t:", round(t_statistic, 4), "\n")
cat("Grados de libertad:", grados_libertad, "\n")
cat("Valor p:", round(valor_p, 4), "\n")

# Regla de decisión
nivel_significancia <- 0.05
if (valor_p < nivel_significancia) {
  cat("Decisión: Rechazar H0 (hay evidencia suficiente para indicar un aumento en el número de latidos)\n")
} else {
  cat("Decisión: No rechazar H0 (no hay suficiente evidencia para indicar un aumento en el número de latidos)\n")
}















# Datos proporcionados
antes <- c(74, 84, 81, 110, 105, 100, 110, 68, 79, 86)
despues <- c(115, 140, 176, 191, 158, 180, 179, 140, 167, 157)

# Paso 1: Calcular las diferencias
diferencias <- despues - antes

# Paso 2: Calcular la media y la desviación estándar de las diferencias
media_diferencias <- mean(diferencias)
desviacion_estandar_diferencias <- sd(diferencias)

# Paso 3: Calcular el estadístico t
t_statistic <- media_diferencias / (desviacion_estandar_diferencias / sqrt(length(diferencias)))

# Paso 4: Determinar los grados de libertad
grados_libertad <- length(diferencias) - 1

# Paso 5: Calcular el valor p
valor_p <- pt(t_statistic, df = grados_libertad, lower.tail = FALSE)

# Mostrar resultados
cat("Estadístico de prueba t:", round(t_statistic, 4), "\n")
cat("Grados de libertad:", grados_libertad, "\n")
cat("Valor p:", round(valor_p, 4), "\n")

# Regla de decisión
nivel_significancia <- 0.05
if (valor_p < nivel_significancia) {
  cat("Regla de decisión: Rechazar H0 (hay evidencia suficiente para indicar un aumento en el número de latidos)\n")
} else {
  cat("Regla de decisión: No rechazar H0 (no hay suficiente evidencia para indicar un aumento en el número de latidos)\n")
}

# Decisión final
if (valor_p < nivel_significancia) {
  cat("Decisión final: Hay evidencia suficiente para rechazar la hipótesis nula.\n")
  cat("Conclusión: La condición experimental aumenta significativamente el número de latidos del corazón.\n")
} else {
  cat("Decisión final: No hay suficiente evidencia para rechazar la hipótesis nula.\n")
  cat("Conclusión: No podemos concluir que la condición experimental aumente significativamente el número de latidos del corazón.\n")
}
