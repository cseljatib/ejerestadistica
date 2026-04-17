##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script: "calcparam1.r"                                        ║
##+║ Sobre:  Calcula parametros-1                                  ║
##-║ Detalles: Calcula el total y el promedio de una variable      ║
##-║  aleatoria en una poblacion.                                  ║
## ║                                                               ║
## ║                                                               ║
##!║ Ejemplo: datos de ejercicio de los apuntes.                   ║
##-║-----------------------------------------------------------    ║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝


##! Definiendo el vector de valores para la poblacion
x<-c(62.9,67.5,68.5,67.4,69.4,64.3,61.7,85.4,59.1)
x

##! Tamanho poblacional
N <- length(x)
N
##+ Parametro del total
tau.x <- sum(x)
tau.x
##+ Parametro de la media
mu.x <- tau.x/N
mu.x
##- Compare el resultado anterior con
mean(x)


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
