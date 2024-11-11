#Declaración de conformidad ----

#Para este caso se define la regla de decisión binaria simple sin zona de seguridad (w = 0)

#Desarrollo de la función de "Declaración" ----

Declaracion <- function(TU, TL, y, u, alfa){
  Probabilidad <- ((pnorm(TU, mean = y, sd = u))-(pnorm(TL, mean = y, sd = u)))
  i <- if (Probabilidad >= (1-alfa)){
    print(c("Declaración de Conformidad", "ISO/IEC Guide 98-4:2012","","El valor:", y, "con incertidumbre +/-:", u, "Para el límite de tolerancia alto (TU):", TU, "y límite de tolerancia bajo (TL):", TL, "Con zona de seguridad (w) = 0","","Declaración de conformidad:", "CONFORME", "Probabilidad de conformidad:", (100*(Probabilidad)), "Nivel de significancia:", alfa))
  } else { 
    print(c("Declaración de Conformidad", "ISO/IEC Guide 98-4:2012","","El valor:", y, "con incertidumbre +/-:", u,"Para el límite de tolerancia alto (TU):", TU, "Y el límite de tolerancia bajo (TL):", TL, "Con zona de seguridad (w) = 0","","Declaración de conformidad:", "NO CONFORME", "Probabilidad de conformidad:", (100*(Probabilidad)), "Nivel de significancia:", alfa))
  }
  grafico <- {pdf("Resultado_Declaracion_Conformidad.pdf", width = 8, height = 6)
    par(mfcol=c(1,1))
    par(mar = c(5,5,4,16))
    plot(y, main = "Declaración de Conformidad" , ylab = "valor", xaxt = "n", pch = 20, xlim = c(0, 2), ylim = c((0.8*TL),(1.2*TU)))
    arrows(1, (y-u), 1, (y+u), angle = 90, code = 3, length = 0.25)
    abline(h = TU, col = "red")
    abline(h = TL, col = "red")
    mtext("Intervalo de tolerancia", side = 4, col = "red", cex = 0.8)
    legend("topright", legend = i, cex = 0.9, xpd = TRUE, inset = c(-0.8,0))
    legend(0, 1.03*TU, legend = "", cex = 0.6, title = "Límite de tolerancia alto (TU)", bty = "n", title.col = "red")
    legend(0, TL, legend = "", cex = 0.6, title = "Límite de tolerancia bajo (TL)", bty = "n", title.col = "red")
    dev.off()
    }
  }

#Aplicación de la función: Declaración

#Se definen los valores TU, TL, y, u y nivel de significancia alfa

TU <- 0.17 #valor normativo máximo o tolerancia máxima según resolución 616 de 2006 para leche cruda
TL <- 0.13 #valor normativo mínimo o tolerancia mínima según resolución 616 de 2006 para leche cruda
y <-  0.15661 #Valor a reportar
u <-  0.00289 #Incertidumbre estándar combinada de y
alfa <- 0.05 #nivel de significancia

#aplicación función de la declaración de conformidad

Declaracion(TU, TL, y, u, alfa)

