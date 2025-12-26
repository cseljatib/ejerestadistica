##! Script: "intconf1.r"                                             /
##- Sobre:  Intervalo de confianza para muestreo aleatorio simple   /
##+ Detalles: Realiza un muestreo aleatorio simple sobre una       /
##+  poblacion para calcular estadigrafos (i.e., estadisticos) e  /
##+  intervalo de confianza estadistico del estimador del        /
##+  parametro de la media.                                     /
## Ejemplo: Datos de ingreso per capita global.                /
##------------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/


##! el N dado es
N<-171
N

#!#los valores de la muestra
y <- c(7.18,40.4,0.64,1.71,13.82, 4.33,24.49,18.4)

hist(y)

summary(y)

##!Verificando el tamanho muestral
length(y) ##esto debe ser igual al tamanho muestral
n<- length(y)
n

##+%%%%%%%%%%%%%%%%
##! III. Calculo de estadigrafos
##+%%%%%%%%%%%%%%%%

##-------------
##!1) Media aritmetica
mean(y)
m.y<-mean(y)
m.y
##-tambien se puede calcular como
sum(y)/length(y)
sum(y)/n

##-------------
##!2) Varianza muestral
sum((y-m.y)^2)/(n-1)
var(y)
#tambien se puede calcular como
v.y<-var(y)
m.y
v.y

#-------------
#3) Desviacion estandar
sqrt(v.y)
#tambien se puede calcular como
s.y<-sd(y)

##!-------------
##+4) Coeficiente de variacion, en %
cv.y<-100*s.y/m.y
cv.y

##lo anterior se puede tambien comparar con los resultados
## de la funcion descstat()
library(datana)
descstat(y)

##!-------------
##+5) Error estandar del estimador (i.e., media aritmetica)
#ES(media.y)
##fraccion de muestreo
f<-n/N
f
(1-f)

sqrt( (v.y/n) * (1-f) )
se.my<-sqrt( (v.y/n) * (1-f) )
se.my
#Error estandar del estimador, en %
100*se.my/m.y

##!-------------
##+6) Error o margen de muestreo, para un nivel de significancia
## estadistico de alpha

##(a) nivel de confianza/significancia
conf <- 90 #en %
alpha <- 1 - (conf/100)
alpha

##- (b) valor de t
gl <- n - 1 #grados de libertad
alpha.2 <- alpha/2
ttab <- abs(qt(1 - alpha.2,gl))
ttab #valor de la dist. de t-student a emplear

##- (c) error, o margen de muestreo
se.my*ttab
e.m <-se.my*ttab

#Error de muestreo en %
100*e.m/m.y

##+ 7) Intervalo de confianza, para un nivel de significancia
##+ estadistico de alpha
lim.inf <- m.y-e.m
lim.inf
lim.sup <- m.y+e.m
c(lim.inf,lim.sup)

message("Aca termina el script!")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#*´¨)
#¸.•´¸.•*´¨) ¸.•*¨)
#(¸.•´ (¸.•` ¤ Fin del script
