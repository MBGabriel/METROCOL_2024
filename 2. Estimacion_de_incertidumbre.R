#Estimación de incertidumbre ----

#Se activan los paquetes: metRology y nortest

library(metRology)
library(nortest)

## Etapa 1. Especificación del mensurando ----

#Se define el mensurando con las fuentes identificadas 

Mensurando <- expression((((V*N*K)/(10*M))*F)+sr)

## Etapa 2. Identificación de las fuentes de incertidumbre ----

#Se hace una lista con los valores medios (x) de las fuentes

x <- list(V = 1.705, 
          N = 0.1,
          K = 90.078,
          M = 10,
          F = 1.01972,
          sr = 0)

## Etapa 3. Cuantificación de los componentes ----

#Se hace una lista con las incertidumbres estándar de x

u <- list(0.010/sqrt(3),
          (0.0003/2),
          (0.00390)/sqrt(3),
          (0.02/sqrt(3)),
          (0.00398),
          (0.00275))

#Se hace una lista con el tipo de distribución de las incertidumbres estándar de x

d <- list("unif",
          "norm",
          "unif",
          "unif",
          "norm",
          "norm")

###Etapa 4.  Cálculo de la incertidumbre estándar combinada ----

#Se crea una función incertidumbre para estimar la incertidumbre con los enfoques GUM, Kragten y Monte Carlo

Incertidumbre <- function(Mensurando, x, u, d){
  #estimación de incertidumbre
  u_GUM <- uncert(Mensurando, x, u, method = "GUM") #Estimación de incertidumbre por GUM
  u_Kragten <- uncert(Mensurando, x, u, method="kragten") #Estimación de incertidumbre por Kragten
  set.seed(1)
  u_MonteCarlo <-uncert(Mensurando, x, u, method="MC", distrib = d, B = 10000) #Estimación de incertidumbre por Monte Carlo con 100 iteraciones
  #Impresión de resultados
  print(u_GUM)
  print(u_Kragten)
  print(u_MonteCarlo)
  #Prueba de normalidad Anderson-Darling
  Anderson <- ad.test(u_MonteCarlo$MC$y)
  print(Anderson)
  #Coeficiente de asimetría de Pearson 3(Media-Mediana)/Desviación estándar
  Pearson <- ((3*(mean(u_MonteCarlo$MC$y) - median(u_MonteCarlo$MC$y)))/sd(u_MonteCarlo$MC$y))
  print(c("Coeiciente de asimetria de Pearson:", Pearson))
    #Tabla Resumen
  Resumen <- data.frame("Enfoque" = c("GUM", "Kragten", "Monte Carlo"),
             "y" = c(u_GUM$y, u_Kragten$y, u_MonteCarlo$y), #Valor a reportar
             "u(y)" = c(u_GUM$u.y, u_Kragten$u.y, u_MonteCarlo$u.y), #incertidumbre estándar combinada
             "U(factor de cobertura k=2)" = c(2*u_GUM$u.y, 2*u_Kragten$u.y, 2*u_MonteCarlo$u.y) #incertidumbre expandida para factor de cobertura k=2
             )
  print("Resumen:Estimación de incertidumbre")
  print(Resumen)
  #Resultados gráficos
  Grafico <- {par(mfcol=c(1,3))
    plot.uncertMC(u_MonteCarlo)
    barplot(drop1.uncertMC(u_MonteCarlo))}
  }

# se estima la incertidumbre utilizando la función desarrollada: Incertidumbre

Incertidumbre(Mensurando, x, u, d) 





