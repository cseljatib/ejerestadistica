## Script: "distriestimador.r"                                   /
## Sobre:  Representacion distribucion de variable y estimador  /
## Detalles: Realiza graficos comparativos simples.            /
## Ejemplo: datos dados en el apunte.                         /
##-----------------------------------------------------------/ 
##                                                          /
## Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/
#
##!Definiendo el vector de valores variable Y para la poblacion
y.var<-c(12,7,14,8,9,10) #c(2,6,8,10,10,12)
##Tamanho poblacional
N <- length(y.var)
N

tau.y<-sum(y.var)
mu.y<-mean(y.var)

#+ Graficos de distribucion de la variable
hist(y.var)
boxplot(y.var)
boxplot(y.var,ylab="Variable aleatoria",las=1)

##+ Valores del estimador

y.med<-c(9.5, 13.0, 10.0, 10.5, 11.0, 10.5,  7.5,  8,  8.5, 11,
         11.5, 12,8.5,  9,  9.5)
#- verificando tamaho del vector
length(y.med)

#+ Graficos de distribucion del estimador
hist(y.med,xlab="Media muestral")
boxplot(y.med,ylab="Media muestral",las=1)


##! Comparando las distribuciones
##+(a) una forma seria repetir las lines anteriores, pero seguidas
boxplot(y.var,ylab="Variable aleatoria",las=1) #poblacion
boxplot(y.med,ylab="Media muestral",las=1) #estimador

##+(b) en una misma figura poner las dos distribuciones
op<-par(mfrow = c(1,2))
boxplot(y.var,ylab="Valor de la variable aleatoria",las=1,main="Distribucion poblacion")
boxplot(y.med,ylab="Valor de la media muestral",las=1,main="Distribucion del estimador")
par(op)

##+(c) lo mismo anterior, pero usando histogramas
op<-par(mfrow = c(2,1))
hist(y.var,xlab="Variable aleatoria",las=1,main="Distribucion poblacion")
hist(y.med,xlab="Media muestral",las=1,main="Distribucion del estimador")
##+ note que en esta figura la disposicion es diferente a la
##+ anterior. Por que cree Ud. lo realice de forma diferente aca?
par(op)

message("Fin del script!")
#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
