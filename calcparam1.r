## Script: "calcparam1.r"                                         /
## Sobre:  Calcula parametros                                    /
## Detalles: Calcula el total y el promedio de una variable     /
##  aleatoria.                                                 /
## Ejemplo: datos de ejercicio de los apuntes.                /
##-----------------------------------------------------------/ 
##                                                          /
## Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/
#
#Definiendo el vector de valores para la poblacion
x<-c(17.9,22.5,23.5,22.4,24.4,19.3,16.7,40.4,14.1)
x

##Tamanho poblacional
N <- length(x)
N
##Parametro del total
tau.x <- sum(x)
tau.x
##Parametro de la media
mu.x <- tau.x/N
mu.x
#compare el resultado anterior con
mean(x)


message("Fin del script!")
#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
