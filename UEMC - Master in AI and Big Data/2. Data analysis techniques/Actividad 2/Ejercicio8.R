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

# b) p(x<=80)
x1 = 80
z = (x1-mu)/sigma
pnorm(z)

normal_area(titulo="Probabilidad de x<80", mean = mu, sd=sigma, ub=x1, acolor = rgb(0,0,1, alpha=0.5))

# c) P(X<=30)
# Se modifica la función normal_area para que represente la gráfica abarcando 4 sigmas de dispersión en lugar de 3
# y poder empezar el eje X en 20
normal_area2 <- function(titulo,mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...){
  x <- seq(mean - 4 * sd, mean + 4 * sd, length = 100)
  
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
x1 = 30
z = (x1-mu)/sigma
pnorm(z)
normal_area2(titulo="Probabilidad de x<30", mean = mu, sd=sigma, ub=x1, acolor = rgb(0,0,1, alpha=0.5))

# e) P(39<=X<=80)
x1 = 39
x2 = 80
z1 = (x1-mu)/sigma
z2 = (x2-mu)/sigma
pnorm(z2)-pnorm(z1)

normal_area(titulo="Probabilidad de 39<x<80", mean = mu, sd=sigma, b=x1, ub=x2, acolor = rgb(0,0,1, alpha=0.5))