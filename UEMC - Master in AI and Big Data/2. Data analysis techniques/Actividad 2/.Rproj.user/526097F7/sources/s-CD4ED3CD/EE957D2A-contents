# Ejercicio 2###################################################################

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


#d)
pbinom(c(3, 6), size = total_preguntas, prob=prob)
# Forma profesor
sumatorio = 0
rango <- 3:6
for (x_exitos in rango){
  resultado = dbinom(x_exitos, total_preguntas, prob)
  print(resultado)
  sumatorio = sumatorio + resultado
}



# Forma profesor
sumatorio = 0

for (x_exitos in 1:10){
  resultado = dbinom(x_exitos, total_preguntas, prob)
  print(resultado)
  sumatorio = sumatorio + resultado
}