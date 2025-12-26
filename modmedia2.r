##! Script: "modmedia2.r"                                         /
##- Sobre:  Ajuste modelo del simple promedio (SMM)              /
##+ Detalles: Emplea estimador de minimos cuadrados             /
##+ Ejemplo: Ajuste con datos de largo de precipitaciones.     /
##-----------------------------------------------------------/ 
##                                                          /
## Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(annualppCities2)
df<-annualppCities2
#?annualppCities2 #ejecutelo en la consola
head(df)
dim(df)
str(df)

##+Estadistica descriptiva
summary(df$pp.anual)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
boxplot(df$pp.anual)
hist(df$pp.anual)

##-Dispersion
plot(df$pp.anual)


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Ajuste del modelo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Estimador de mu
mu.hat <- mean(df$pp.anual)
mu.hat
##- Estimador de varianza del error
var.hat.e <- var(df$pp.anual)
var.hat.e

##- Grafico del modelo ajustado
plot(df$pp.anual,xlab="Identificador de la muestra",ylab="Precipitacion anual (mm)", bty="l",las=1)
abline(h=mean(df$pp.anual), col="red", lwd=3)


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
