#Actividad 1: X -> N(100;16)
##A) Calcular P(X>120)
###Uso de función procomp para calcular FUNCIÓN DE DISTRIBUCIÓN ACUMULADA: P(X <= x)
1 - pnorm(120, mean = 100, sd = 16) #sin tipificar
1- pnorm(1.25, mean = 0, sd = 1) #distribución tipificada
##B) Calcular P(X>120|X>110)
# P(X>120|X>110) = P(X>110|X>120)P(X>120)/P(X>110)