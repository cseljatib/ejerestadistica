##! Script: "modmedia1.r"                                         /
##- Sobre:  Ajuste modelo del simple promedio (SP)              /
##+ Detalles: Emplea estimador de minimos cuadrados             /
##+ Ejemplo: Ajuste con datos de largo de peces.               /
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
data(fishgrowth2)
df <- fishgrowth2
#?fishgrowth2 #ejecutelo en la consola
head(df)
dim(df)
str(df)

##+Estadistica descriptiva
summary(df$largo)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
boxplot(df$largo)
hist(df$largo)

##-Dispersion
plot(df$largo)


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Ajuste del modelo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##- Estimador de mu
mu.hat <- mean(df$largo)
mu.hat
##- Estimador de varianza del error
var.hat.e <- var(df$largo)
var.hat.e

##-grafico del modelo ajustado
plot(df$largo,xlab="Identificador de la muestra",ylab="Largo (mm)")
abline(h=mean(df$largo), col="red", lwd=3)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
