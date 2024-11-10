# Librerias
install.packages("lubridate")
install.packages("tidyverse")
install.packages("rriskDistributions")
install.packages("readxl")
install.packages("BSDA")
install.packages("normtest")
install.packages("nortest")
install.packages("moments")
install.packages("forecast")

library(lubridate)
library(tidyverse)
library(rriskDistributions)
library(readxl)
library(BSDA)
library(normtest) ###REALIZA 5 PRUEBAS DE NORMALIDAD###
library(nortest) ###REALIZA 10 PRUEBAS DE NORMALIDAD###
library(moments) ###REALIZA 1 PRUEBA DE NORMALIDAD###
require(rriskDistributions)
library(forecast)

# 1. Lectura de los datos de los dataset de navegación y conversión
df <- read_excel("Datos de partida para la actividad 2_profesor.xlsx")

# 2. Importar los datos generados en la actividad 1
metricas <- read.table("resultados_actividad1.txt", header = TRUE, sep = ',', dec = '.')

# 3. Desviacion tipica de la media de tiempo que tarda el call center en ponerse en contacto 
#    con el usuario en caso de que el tipo de lead sea FORM por tipo de respuesta
#    ('Positivo', 'No le interesa', 'Ilocalizable')
medias_call_center <- c (1721.0, 3518.0, 188.33)
desviacion_tipica_medias <- sd(medias_call_center)

#    Desviacion tipica de la media de ratio de conversion por campaña, adgroup, sitelink y anuncio
medias_ratio_conversion <- c ( 0.3816888179265104, 0.47719938514674987,  0.04463465479471415, 0.8711688578755147)
desviacion_tipica_medias_ratio_conversion <- sd(medias_ratio_conversion)

# 4. Intervalo de confianza al 95% de las desviaciones tipicas del punto anterior
nivel_confianza <- 0.95
media_medias_call_center <- mean(medias_call_center)
zsum.test(mean.x=media_medias_call_center,sigma.x=desviacion_tipica_medias, n.x=length(df),conf.level=nivel_confianza)

media_medias_ratio_conversion <- mean(medias_ratio_conversion)
zsum.test(mean.x=media_medias_ratio_conversion,sigma.x=desviacion_tipica_medias_ratio_conversion, n.x=length(df),conf.level=nivel_confianza)

# 5. Prevision del numero de llamadas que debe soportar en los proximos dias el call center
CALL_conversiones_con_tiempo_en_contestar <- read.table("CALL_contact_con_tiempo_en_contestar.csv", header = TRUE, sep = ',', dec = '.')
df_arima <- data.frame("id_conversion" = CALL_conversiones_con_tiempo_en_contestar$Column1,
                       "hora" = CALL_conversiones_con_tiempo_en_contestar$hour,
                       "hora_redondeada" = CALL_conversiones_con_tiempo_en_contestar$rounded_hour,
                       "tiempo_minutos" = CALL_conversiones_con_tiempo_en_contestar$tiempo_en_contestar)
#Gráfica de lineas inicial
ggplot(data = df_arima, aes(x = hora_redondeada, y = tiempo_minutos)) + 
  geom_line()

#Creando objeto ts para podelo
tiempo_minutos_ts <- ts(df_arima$tiempo_minutos,
                        start = 1,
                        frequency = 24)
# Ajuste del modelo
ajuste <- auto.arima(y = tiempo_minutos_ts)
summary(ajuste)

# Predicciones
predicciones <- forecast(ajuste)
min(predicciones[['lower']])
min(predicciones[['upper']])

p_predict <- autoplot(predicciones)
p_predict


# 6. Tipo de distribucion de los ratios de conversión por producto obtenidos en la actividad 1
medias_ratio_conversion_marcas <- read.table("media_ratios_conversion_marca.txt", header = TRUE, sep = ',', dec = '.')
hist(medias_ratio_conversion_marcas$media_ratio_conversion, main = "Histograma de frecuencias", # Frecuencia
     ylab = "Frecuencia")

tipo_distribucion <- fit.cont(medias_ratio_conversion_marcas$media_ratio_conversion)

rriskFitdist.cont(medias_ratio_conversion_marcas$media_ratio_conversion, "norm")$stimate
