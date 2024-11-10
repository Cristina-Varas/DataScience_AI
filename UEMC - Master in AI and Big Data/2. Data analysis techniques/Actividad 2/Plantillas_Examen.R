################################################################################
# DISTRIBUCIÓN BINOMIAL ########################################################
################################################################################
total_preguntas <- 10
# Si consideramos que cada pregunta tiene 4 posibles respuestas,
# la probabilidad es 1/4 = 0,25
prob <- 0.25

# a)
dbinom(5, size=total_preguntas, prob = prob)

# b)
# Forma 1
dbinom1 <- dbinom(1, size=total_preguntas, prob = prob)
dbinom2 <- dbinom(2, size=total_preguntas, prob = prob)
dbinom3 <- dbinom(3, size=total_preguntas, prob = prob)
dbinom4 <- dbinom(4, size=total_preguntas, prob = prob)
dbinom5 <- dbinom(5, size=total_preguntas, prob = prob)
dbinom6 <- dbinom(6, size=total_preguntas, prob = prob)
dbinom7 <- dbinom(7, size=total_preguntas, prob = prob)
dbinom8 <- dbinom(8, size=total_preguntas, prob = prob)
dbinom9 <- dbinom(9, size=total_preguntas, prob = prob)
dbinom10 <- dbinom(10, size=total_preguntas, prob = prob)
prob1 <- dbinom1 + dbinom2 + dbinom3 + dbinom4 + dbinom5 + dbinom6 + dbinom7 + dbinom8 + dbinom9 + dbinom10

# Forma 2
prob1 = 1 - dbinom(0, size=total_preguntas, prob = prob)

# c)
# Forma 1
prob5 <-  dbinom5 + dbinom6 + dbinom7 + dbinom8 + dbinom9 + dbinom10

# Forma 2
rango = 5:10
sumatorio = 0
for (x_exitos in 5:10){
  resultado = dbinom(x_exitos, total_preguntas, prob)
  print(resultado)
  sumatorio = sumatorio + resultado
}
################################################################################
# DISTRIBUCIÓN DE POISSON ######################################################
################################################################################
# Lambda = 8 para 100 horas de trabajo, luego lambda = 2 para 25 horas de trabajo
lambda <- 2
prob = dpois(1, lambda)
print(prob)


# Definimos los distintos valores de x
x <- 0:50

# Lambda = 5 REPRESENTACIÓN
plot(dpois(x,5), type = "h", lwd = 2, main = "Función de masa de probabilidad lambda = 5",
     ylab = "P(X=x)", xlab = "Numero de eventos", col = "black")


# lambda = 5
plot(dpois(x,5), type = "h", lwd = 2,
     main = "Función de masa de probabilidad",
     ylab = "P(X = x)", xlab = "Número de éxito")

# lambda = 10
lines(dpois(x,10), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))

# lambda = 15
lines(dpois(x,15), type = "h",
      lwd = 2, col = rgb(0, 1, 0, 0.7))

# lambda = 20
lines(dpois(x,20), type = "h",
      lwd = 2, col = rgb(0, 0, 1, 0.7))

# Add a legend
legend("topright", legend = c("5", "10", "15", "20"),
       title = "Tamaño  Prob", title= expression(lambda),
       lty = 1, col = 1:4, lwd = 2, box.lty = 0)

################################################################################
# DISTRIBUCIÓN NORMAL ##########################################################
################################################################################
# mean: media de la variable normal
# sd: desviacion típica de la variable normal
# lb: límite inferior del área
# ub: límite inferior de área
# acolor: color del area

normal_area <- function(titulo,mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...){
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100)
  
  if(missing(lb)){
    lb <- min(x)
  }
  
  if(missing(ub)){
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "f(x)", main=titulo)
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
  
}

# a) p(x>=70)
mu <- 60
sigma <- 10
x1 = 70
z = (x1-mu)/sigma

pnorm(z, lower.tail = FALSE)

normal_area(titulo="Probabilidad de x>70", mean = mu, sd=sigma, lb=x1, acolor = rgb(0,0,1, alpha=0.5))


# e) P(39<=X<=80)
x1 = 39
x2 = 80
z1 = (x1-mu)/sigma
z2 = (x2-mu)/sigma
pnorm(z2)-pnorm(z1)

normal_area(titulo="Probabilidad de 39<x<80", mean = mu, sd=sigma, b=x1, ub=x2, acolor = rgb(0,0,1, alpha=0.5))


################################################################################
# INTERVALOS DE CONFIANZA ######################################################
################################################################################

install.packages("TeachingDemos")
library(TeachingDemos)
y = c(102, 98, 93, 100, 98, 105, 115, 110, 99, 120,
      115, 130, 100, 86, 95, 103, 105, 92, 99, 134,
      116, 118, 89, 102, 128, 99, 119, 128, 110, 130,
      112, 114, 105, 114, 100, 116, 108, 113, 106, 105,
      120, 106, 110, 100, 106, 117, 109, 108, 105, 106)

alpha = 0.01
media = mean(c)
desEst = 10.809
z.test(y, media, desEst, conf.level = 0.95)



clientes = c(2,10,4,5,1,0,5,9,3,9)
media = mean(clientes)
sqrt(var(clientes))
# Vamos a realizar un t.test para la media de una muestra,
# cuya desviaci?n t?pica es desconocida y el tama?o de la muestra
# es menor que 30 (en este caso 10). El nivel de confianza es del 0.9
t.test( clientes, alternative = ("two.sided"), mu=media, var.equal = FALSE, conf.level = 0.9)
# El intervalo de confianza es 
# [2.758732, 6.841268]

# Calculo del intervalo de confianza por n?meros 
a = 4.8 -1.833*(3.521363/ sqrt(10)) 
b =4.8 +1.833*(3.521363/ sqrt(10))
a
b

# ··············································································

# Desviacion tipica conocida
desviacion <- 0.7

# Media poblacional teorica
mu = 7.8

# Muestra de 10 varillas
datos <- c(8.1, 7.8, 7.9, 8.0, 7.2, 8.4, 8.3, 8.3, 7.7, 7.4)

# Intervalo confianza y contraste de hip?tesis para el tamanio medio poblacionalida
library(TeachingDemos)
alpha <- 0.05
z.test(datos, mu, desviacion, conf.level = 1-alpha)




