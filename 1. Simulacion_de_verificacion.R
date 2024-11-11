# Simulación para la verificación del método ----

## Simulación ensayos de veracidad ----

#se simulan datos con los supuestos de distribución normal con valor medio 98% y desviación estándar 0.5  

set.seed(1)
Recuperacion <- round(rnorm(10, mean = 98, sd =0.5), 2)
Recuperacion

mean(Recuperacion)
sd(Recuperacion)

F <- (100/mean(Recuperacion)) #Factor de corrección para acidez

## Simulación ensayos de repetibilidad ----

#Se simulan los datos de repetibilidad apartir de volumen de hidroxido de sodio consumido con los supuestos de distribución normal con valor medio 1,70 y desviación estándar 0.04

set.seed(1)
V <- round(rnorm(10, mean = 1.70, sd =0.04), 2)
V

#Se determina la acidez corregida con los datos simulados

#para lo cual: 

N <- 0.1 #Normalidad del hidróxido de sodio

M <- 10 #Volumen de muestra de leche

K <- (3*12.011)+(6*1.008)+(3*15.999) #peso equivalenete del ácido láctico

#K corresponde al peso equivalente del ácido láctico C3H6O3
#C : 12.011 sd = 0.002 uniforme  
#H : 1.008 sd = 0.0002 uniforme
#O : 15.999 sd = 0.001 uniforme
#C3H6O3 : 90.078

#se calcula la acidez aplicando la ecuación del método de referencia

A <- (V*N*0.9/M)
A

#Se calcula la acidez corregida

y <- (V*N*K/(1000*M))*100*F
y

#se estima la repetibilidad

sr <- sd(y)


