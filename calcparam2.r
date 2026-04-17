##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script: "calcparam2.r"                                        ║
##+║ Sobre:  Calcula parametros-2                                  ║
##-║ Detalles: Calcula la varianza y ordena una variable           ║
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

##!Definiendo el vector de valores para la poblacion
x<-c(62.9,67.5,68.5,67.4,69.4,64.3,61.7,85.4,59.1)
x

##!Tamanho poblacional
N <- length(x)
N
##+(a) El parametro del total
tau.x <- sum(x)
tau.x
##+(b) El parametro de la media
mu.x <- tau.x/N
mu.x
##-compare el resultado anterior con
mean(x)

##!(c) El parametro de la varianza
(1/N)*sum((x-mu.x)^2) 
# este es el valor correcto

##+compare el resultado anterior con
var(x)
#son iguales? 
#y haga lo mismo con
(1/(N-1))*sum((x-mu.x)^2) 
#ve alguna diferencia?

##!(d) Ordenando los valores de menor a mayor
sort.x<-sort(x)
sort.x
# la mediana, es el parametro que divide en dos 
# a la distribucion de la variable en la poblacion
N
#dado que N es impar (11), entonces es facil encontrar
# la mediana 
N/2
# al buscar la posicion del quinto-elemento en el 
#vector ordenado de la variable aleatoria
pos.mediana<-ceiling(N/2)
pos.mediana

##!y la mediana es 
sort.x[pos.mediana]
##+compare el resultado anterior con
median(x)

message("Fin del script!")
#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
