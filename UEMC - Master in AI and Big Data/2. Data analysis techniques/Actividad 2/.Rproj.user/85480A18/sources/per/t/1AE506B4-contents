# TEMA 7: CONTRASTE DE HIPÓTESIS
library(TeachingDemos)

#1. Contraste BILATERAL varianza poblacional sigma^2 conocida MIU = MIU0, MIU!= MIU0
# Si Z_exp < Z_alfa/2 -> se acepta H0
# Si Zexp >= Z_alfa/2 - > se rechaza H0

y = c(4, 13, 8, 12, 8, 15, 14, 7, 8)
media = 10
desviacion = 4
z.test(y,media,desviacion)

#2. Contraste UNILATERAL varianza poblacional sigma^2 conocida MIU <= MIU0, MIU > MIU0
# Si Z_exp < Z_alfa -> se acepta H0
# Si Zexp >= Z_alfa - > se rechaza H0

#3. Contraste UNILATERAL varianza poblacional sigma^2 conocida MIU >= MIU0, MIU < MIU0
# Si Z_exp > -Z_alfa -> se acepta H0
# Si Zexp <= -Z_alfa -> se rechaza H0


#3. Contraste BILATERAL sigma^2 desconocida n>30

#4. Contraste UNILATERALL sigma^2 desconocida n>30


#5. Contraste BILATERAL sigma^2 desconocida n<=30

#6. Contraste UNILATERAL sigma^2 desconocida n<=30

# Una cola
x = c(11, 9, 12, 17, 8, 11, 9, 4, 5, 9, 14, 9, 17, 24, 19, 10, 17, 17, 8, 23, 8, 6, 14, 16, 6, 7, 15, 20, 14, 15)
t.test(x, mu = 11.5, var.equal = FALSE, conf.level = 0.95)

# Dos colas
x = c(11, 9, 12, 17, 8, 11, 9, 4, 5, 9, 14, 9, 17, 24, 19, 10, 17, 17, 8, 23, 8, 6, 14, 16, 6, 7, 15, 20, 14, 15)
t.test(x,alternative = c("two.sided"), mu = 11.5, var.equal = FALSE, conf.level = 0.95)

