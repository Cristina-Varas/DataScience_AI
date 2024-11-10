# EJERCICIO 6


# Media y desviación típica
mu <- 10
sigma <- 2

# Apartado a
x1=7
z=(x1-mu)/sigma

pnorm(z)

# Apartado b

x1=8
x2=13
z1=(x1-mu)/sigma
z2=(x2-mu)/sigma

pnorm(z2)-pnorm(z1)