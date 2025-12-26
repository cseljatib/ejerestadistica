##! Script: "intconf2.r"                                             /
##- Sobre:  Intervalo de confianza para muestreo aleatorio simple   /
##+ Detalles: Realiza un muestreo aleatorio simple sobre una       /
##+ poblacion para calcular estadigrafos (i.e., estadisticos) e   /
##+ intervalo de confianza estadistico del estimador del         /
##+ parametro de la media.                                      /
##! Ejemplo: Datos de edad provenientes de la encuesta CASEN.  /
##------------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/


##+%%%%%%%%%%%%%%%%
##! I. Poblacion
##+%%%%%%%%%%%%%%%%
library(datana)
head(casen)
##- revise la metadata al activar la siguiente linea
#?casen
df<-casen
dim(df)
summary(df$edad)
table(df$comuna)

##- filtro, emplear solo los datos de la comuna de Temuco
df<-subset(df, comuna=="Temuco")

##- verificando las nuevas dimensiones
dim(df)

##- Se asumira que estos datos seran nuestra poblacion
N<-nrow(df)
N

##! Estadistica descriptiva
descstat(df[,c("edad","ytot")])

#variable aleatoria de interes: la edad
hist(df$edad)


##%%%%%%%%%%%%%%%%
## II. Estrategia de muestreo
##%%%%%%%%%%%%%%%%
###Tamanho muestral
n<- 12

## Seleccion de elementos bajo muestreo aleatorio simple.
1:10 ##secuencia de numeros
#como se emplea la funcion sample()?
sample(1:10,3)

# seleccionemos los elementos
elem.muestra <- sample(1:N,size=n,replace=FALSE)
elem.muestra

##dichos elementos corresponden a las siguientes valores de variables
mi.muestra <- df[elem.muestra,]
dim(mi.muestra)
head(mi.muestra)

#los valores de la variable aleatoria para nuestra muestra
y<-mi.muestra$edad
length(y) ##esto debe ser igual al tamanho muestral
y

##+%%%%%%%%%%%%%%%%
##! III. Calculo de estadigrafos
##+%%%%%%%%%%%%%%%%

##-------------
##!1) Media aritmetica
mean(y)
m.y<-mean(y)
m.y
#tambien se puede calcular como
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

##-------------
##!3) Desviacion estandar
sqrt(v.y)
#tambien se puede calcular como
s.y<-sd(y)

##-------------
##! 4) Coeficiente de variacion, en %
cv.y<-100*s.y/m.y
cv.y

#lo anterior se puede tambien comparar con los resultados
# de la funcion descstat()
descstat(y)

##-------------
##!5) Error estandar del estimador (i.e., media aritmetica)
#ES(media.y)
##fraccion de muestreo
f<-n/N
f
(1-f)

sqrt( (v.y/n) * (1-f) )
se.my<-sqrt( (v.y/n) * (1-f) )
se.my
##!Error estandar del estimador, en %
100*se.my/m.y

##-------------
##!6) Error o margen de muestreo, para un nivel de significancia
## estadistico de alpha

##+(a) nivel de confianza/significancia
conf <- 95 #en %
alpha <- 1 - (conf/100)
alpha

##+(b) valor de t
df <- n - 1
alpha.2 <- alpha/2
t.value <- abs(qt(1 - alpha.2,df))
t.value #valor de la dist. de t-student a emplear

##+(c) error, o margen de muestreo
se.my*t.value
e.m <-se.my*t.value

##+Error de muestreo en %
100*e.m/m.y

##!7) Intervalo de confianza, para un nivel de significancia
## estadistico de alpha
lim.inf <- m.y-e.m
lim.inf
lim.sup <- m.y+e.m
c(lim.inf,lim.sup)

message("Aca termina el script!")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#*´¨)
#¸.•´¸.•*´¨) ¸.•*¨)
#(¸.•´ (¸.•` ¤ Fin del script
