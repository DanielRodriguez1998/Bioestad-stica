# Estadística Descriptiva 
# Este script realiza el análisis estadístico básico sobre un conjunto de datos numéricos:
# cálculo de frecuencia, medidas de tendencia central, dispersión, curtosis y sesgo, además de generar histogramas, polígonos y ojivas

# ---- CARGA DE LIBRERÍAS NECESARIAS ----
install.packages("e1071")  # Para calcular sesgo
library(e1071)

#install.packages("dplyr") # Ya comentado, pues normalmente lo instalas una vez
library(dplyr)

#install.packages("moments") # Paquete para calcular curtosis
library(moments)

library(ggplot2)  # Para gráficos

# ---- CARGA DE DATOS ----
# Carga los datos desde un archivo de texto
ruta_archivo <- "D:/Maestría/Materias/bioesta/datos.txt"
data <- scan(file = ruta_archivo, what = numeric(), sep = "
")
data

# ---- CÁLCULOS INICIALES ----
# Rango, número de observaciones, número de intervalos según Sturges
Rango <- max(data) - min(data)
n <- length(data)
k <- round(1 + 3.32*log10(n))

# ---- CONSTRUCCIÓN DE INTERVALOS ----
# Determinar ancho del intervalo
desviacion_estandar <- sd(data)
rango_total <- max(data) - min(data)
longitud_intervalo <- round(rango_total / k, 1)
longitud_intervalo <- 4.3  # Definido manualmente

# Limites de intervalos
limites_inferiores <- seq(min(data), max(data), by = longitud_intervalo)
limites_superiores <- limites_inferiores + longitud_intervalo
intervalos <- paste(limites_inferiores, limites_superiores, sep = "-")
intervalos2 <- intervalos

# ---- FRECUENCIAS ----
hist_result <- hist(data, breaks = c(limites_inferiores, max(limites_superiores)), plot = FALSE)
frec_relativa <- hist_result$counts / sum(hist_result$counts)
tabla1 <- data.frame(
  Límite_Inferior = hist_result$breaks[-length(hist_result$breaks)],
  Límite_Superior = hist_result$breaks[-1],
  Frecuencia_total = sum(hist_result$counts),
  Frecuencia_Relativa = frec_relativa,
  Frecuencia_Absoluta = hist_result$counts
)

frecuencias_relativas <- tabla1$Frecuencia_Relativa
frecuencias_relativas_porcentaje <- frecuencias_relativas * 100
fa1 <- 0
frecuencias_relativas2 <- c(fa1, frecuencias_relativas, fa1)
frecuencias_relativas_porcentaje2 <- c(fa1, frecuencias_relativas_porcentaje, fa1)

# Frecuencias acumuladas
Frecuencias_acumuladas <- cumsum(hist_result$counts)
Frecuencias_acumuladas <- c(fa1, Frecuencias_acumuladas)
Frecuencias_acumuladas_porcentaje <- (Frecuencias_acumuladas / sum(hist_result$counts)) * 100

# Frecuencia absoluta manual
frecuencia_absoluta <- table(cut(data, breaks = c(limites_inferiores, max(limites_superiores))))
frecuencia_absoluta2 <- c(fa1, frecuencia_absoluta, fa1)

# ---- MARCAS DE CLASE Y POLÍGONOS ----
marcas_clase <- seq(min(data) + longitud_intervalo / 2, max(limites_superiores) - longitud_intervalo / 2, by = longitud_intervalo)
marcas_clase_poligono <- c(13.598, marcas_clase, 47.998)
limites_superiores2 <- c(15.748, limites_superiores)

# ---- OJIVAS ----
df <- data.frame(limites_superiores2 = limites_superiores2, FrecuenciaAcumulada = Frecuencias_acumuladas)
dfporcentaje <- data.frame(limites_superiores2 = limites_superiores2, FrecuenciaAcumulada = Frecuencias_acumuladas_porcentaje)

# Ojiva absoluta
ggplot(df, aes(x = limites_superiores2, y = FrecuenciaAcumulada)) + geom_point() + geom_line() +
  labs(title = "Ojiva", x = "Límites Superiores", y = "Frecuencia Acumulada") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = df$limites_superiores2)

# Ojiva porcentaje
ggplot(dfporcentaje, aes(x = limites_superiores2, y = FrecuenciaAcumulada)) + geom_point() + geom_line() +
  labs(title = "Ojiva", x = "Límites Superiores", y = "Porcentaje de Frecuencia Acumulada") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = dfporcentaje$limites_superiores2)

# ---- HISTOGRAMAS ----
# Con marcas de clase
barplot(frecuencia_absoluta, names.arg = marcas_clase, main = "Histograma", xlab = "Marca de clase", ylab = "Frecuencia Absoluta", col = "lightblue", border = "white", space = 0)

# Relativa
barplot(frecuencias_relativas, names.arg = marcas_clase, main = "Histograma", xlab = "Marca de clase", ylab = "Frecuencia Relativa", col = "lightblue", border = "white", space = 0)

# Relativa en porcentaje
barplot(frecuencias_relativas_porcentaje, names.arg = marcas_clase, main = "Histograma", xlab = "Marca de clase", ylab = "Porcentaje de Frecuencia Relativa", col = "lightblue", border = "white", space = 0)

# Con intervalos
barplot(frecuencia_absoluta, names.arg = intervalos2, main = "Histograma", xlab = "Intérvalos", ylab = "Frecuencia Absoluta", col = "lightblue", border = "white", space = 0)
barplot(frecuencias_relativas, names.arg = intervalos2, main = "Histograma", xlab = "Intérvalos", ylab = "Frecuencia Relativa", col = "lightblue", border = "white", space = 0)
barplot(frecuencias_relativas_porcentaje, names.arg = intervalos2, main = "Histograma", xlab = "Intérvalos", ylab = "Porcentaje de Frecuencia Relativa", col = "lightblue", border = "white", space = 0)

# ---- POLÍGONOS ----
plot(marcas_clase_poligono, frecuencia_absoluta2, type = "b", pch = 19, col = "blue", xlab = "Marcas de Clase", ylab = "Frecuencia Absoluta", main = "Polígono de Frecuencia", axes = FALSE)
axis(side = 1, at = marcas_clase_poligono, labels = round(marcas_clase_poligono, 2))
axis(side = 2)

plot(marcas_clase_poligono, frecuencias_relativas2, type = "b", pch = 19, col = "blue", xlab = "Marcas de Clase", ylab = "Frecuencia Relativa", main = "Polígono de Frecuencia", axes = FALSE)
axis(side = 1, at = marcas_clase_poligono, labels = round(marcas_clase_poligono, 2))
axis(side = 2)

plot(marcas_clase_poligono, frecuencias_relativas_porcentaje2, type = "b", pch = 19, col = "blue", xlab = "Marcas de Clase", ylab = "Porcentaje de Frecuencia Relativa", main = "Polígono de Frecuencia", axes = FALSE)
axis(side = 1, at = marcas_clase_poligono, labels = round(marcas_clase_poligono, 2))
axis(side = 2)

# ---- MEDIDAS DE TENDENCIA Y DISPERSIÓN ----
media <- mean(data)
mediana <- median(data)
moda <- as.numeric(names(sort(table(data), decreasing = TRUE)[1]))

desviacion_estandar <- sd(data)
varianza <- var(data)
error_estandar <- desviacion_estandar / sqrt(n)
coeficiente_variacion <- (desviacion_estandar / media) * 100

# Cuartiles, deciles, percentiles
q1 <- quantile(data, 0.25, type = 6)
q2 <- quantile(data, 0.50)
q3 <- quantile(data, 0.75, type = 6)
d1 <- quantile(data, 0.1)
d9 <- quantile(data, 0.9)
p10 <- quantile(data, 0.10, type = 6)
p90 <- quantile(data, 0.90, type = 6)

# Curtosis y sesgo
curtosis_result <- kurtosis(data, type = 2)
sesgo <- skewness(data, type = 2)

# ---- RESUMEN EN TABLA ----
minimo <- min(data)
maximo <- max(data)

medidas <- c("Media", "Mediana", "Moda", "Varianza", "Desviacion estandar", "Coeficiente de variación", "Sesgo", "Error estandar", "Rango", "Mínimo", "Máximo", "Curtosis result", "Cuartil1", "Cuartil3", "Percentil10", "Percentil90")
valores <- c(media, mediana, moda, varianza, desviacion_estandar, coeficiente_variacion, sesgo, error_estandar, Rango, minimo, maximo, curtosis_result, q1, q3, p10, p90)
tabla_resultados <- data.frame(Medida = medidas, Valor = valores)

View(tabla_resultados)
