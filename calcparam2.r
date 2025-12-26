## Script: "calcparam2.r"                                         /
## Sobre:  Calcula parametros-2                                  /
## Detalles: Calcula la mediana y la varianza de una variable   /
##  aleatoria.                                                 /
## Ejemplo: datos de ejercicio de los apuntes.                /
##-----------------------------------------------------------/ 
##                                                          /
## Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/
#
##!Definiendo el vector de valores para la poblacion
x<-c(17.9,22.5,23.5,22.4,24.4,19.3,16.7,40.4,14.1)

##!Tamanho poblacional
N <- length(x)
N
##+Parametro del total
tau.x <- sum(x)
tau.x
##+Parametro de la media
mu.x <- tau.x/N
mu.x
##-compare el resultado anterior con
mean(x)

##!(a) El parametro de la varianza
(1/N)*sum((x-mu.x)^2) 

##+compare el resultado anterior con
var(x)
#son iguales? 
#y haga lo mismo con
(1/(N-1))*sum((x-mu.x)^2) 
#ve alguna diferencia?

##!(b) Ordenando los valores de menor a mayor
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
