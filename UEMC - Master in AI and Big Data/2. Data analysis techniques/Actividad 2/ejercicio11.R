####################################################
# ACTIVIDAD GRUPAL
#               EJERCICIO 11

clientes = c(2,10,4,5,1,0,5,9,3,9)
media = mean(clientes)
sqrt(var(clientes))
# Vamos a realizar un t.test para la media de una muestra,
# cuya desviación típica es desconocida y el tamaño de la muestra
# es menor que 30 (en este caso 10). El nivel de confianza es del 0.9
t.test( clientes, alternative = ("two.sided"), mu=media, var.equal = FALSE, conf.level = 0.9)
# El intervalo de confianza es 
# [2.758732, 6.841268]

# Calculo del intervalo de confianza por números 
a = 4.8 -1.833*(3.521363/ sqrt(10)) 
b =4.8 +1.833*(3.521363/ sqrt(10))
a
b
