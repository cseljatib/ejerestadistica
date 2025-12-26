## Script: "mathexpre1.r"                                         /
## Sobre:  Representando expresiones matematicas                 /
## Detalles: Como realizar calculos de funciones y formulas     /
##  matematicas en R.                                          /
## Ejemplo: datos de ejercicio de los apuntes.                /
##-----------------------------------------------------------/ 
##                                                          /
## Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/
#
#Definiendo el vector de valores, para variable Y
y <- c(5  ,28  ,32  ,40  ,65 ,24 , 78 ,44)
ny <- length(y) #tamanho del vector
ny
#definiendo el vector de valores, para variable X
x <- c(6.6 ,1.4 ,12.4 ,1.2 ,9.2 ,4.3 ,3.7 ,2.7)
nx <- length(y) #tamanho del vector
nx

##========================
###Primera parte: sobre expresiones matematicas
##========================
#(i)
sum(y)
#(ii)
sum(y^2)
#(iii)
(sum(y))^2
#(iv)
m.y<-mean(y) #media aritmetica de Y
m.y
sum((y-m.y)^2) 

#(v)
sum(abs(y-m.y)) 

##========================
###Segunda parte: sobre estadisticos
##========================
#(i)
mean(y)
#(ii)
(1/(ny-1))*sum((y-m.y)^2) 
##compare con
#var(y)
#(iii)
(prod(y))^(1/ny)


####Ahora cambiar la variable aleatoria de interes
##========================
###Primera parte: sobre expresiones matematicas
##========================
#(i)
sum(x)
#(ii)
sum(x^2)
#(iii)
(sum(x))^2
#(iv)
m.x<-mean(x) #media aritmetica de Y
m.x
sum((x-m.x)^2) 

#(v)
sum(abs(x-m.x)) 

##========================
###Segunda parte: sobre estadisticos
##========================
#(i)
mean(x)
#(ii)
(1/(nx-1))*sum((x-m.x)^2) 
##compare con
#var(x)
#(iii)
(prod(x))^(1/nx)

message("Aca termina el script!")
#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute el script!  ║
#║ El profesor     ╔════╝
#╚═════════════════╝
