# Definimos los distintos valores de x
x <- 0:50

# Lambda = 5
plot(dpois(x,5), type = "h", lwd = 2, main = "Función de masa de probabilidad lambda = 5",
     ylab = "P(X=x)", xlab = "Numero de eventos", col = "black")

# Lambda = 10
plot(dpois(x,10), type = "h", lwd = 2, main = "Función de masa de probabilidad lambda = 10",
     ylab = "P(X=x)", xlab = "Numero de eventos", col = "blue")

# Lambda = 15
plot(dpois(x,15), type = "h", lwd = 2, main = "Función de masa de probabilidad lambda = 15",
     ylab = "P(X=x)", xlab = "Numero de eventos", col = "blue")

# Lambda = 20
plot(dpois(x,20), type = "h", lwd = 2, main = "Función de masa de probabilidad lambda = 20",
     ylab = "P(X=x)", xlab = "Numero de eventos", col = "blue")

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