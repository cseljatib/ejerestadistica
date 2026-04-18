##!╔════════════════════════════════════════════════════════════════╗
##*║ Script: "phipotesis.r"                                         ║
##+║ Sobre:  Prueba de hipotesis para la estimacion de la media.    ║
##-║ Detalles: Calculo test de hipotesis y decision segun nivel de  ║
##-║  confianza estadistica.                                        ║
## ║                                                                ║
## ║                                                                ║
##*║ Ejemplo: Datos de edad provenientes de la encuesta CASEN.      ║
##-║----------------------------------------------------------------║
## ║                                                                ║
##>║ Profesor: Christian Salas Eljatib                              ║
##+║ E-mail: christian.salas AT uchile DOT cl                       ║
##*║ Web: https://eljatib.com                                       ║
##!╚════════════════════════════════════════════════════════════════╝

##+==================================================
##! I. Datos
##+==================================================
library(datana)
head(casen)
##- revise la metadata al activar la siguiente linea
#?casen
df<-casen
dim(df)
n.all<-nrow(df)
summary(df$edad)
table(df$comuna)

##! Filtro, emplear solo los datos de la comuna de Temuco
df<-subset(df, comuna=="Temuco")
table(df$comuna)

##- verifique dimensiones ahora
dim(df)
##- y comparelas con
n.all


##estadistica descriptiva
descstat(df[,c("edad","ypc")])
hist(df$edad)

##+================================================
##! II. Calculo de estadisticos en base a muestreo
##+================================================
##- Tamanho muestral
n<- 10
##- seleccionemos los elementos
set.seed(12345) 
nobs.dispo<-nrow(df) ## numero de observaciones disponibles
nobs.dispo
##- recuerde que una secuencia se obtiene mediante
1:4 ##> entre 1 al 4 (salto de uno)
##- una lista con la posicion correlativa de los elementos 
list.num.corre<-1:nobs.dispo 
elem.muestra <- sample(list.num.corre,size=n,replace=FALSE)
elem.muestra

##+ dichos elementos corresponden a la siguiente porcion de los datos
mi.muestra <- df[elem.muestra,]
dim(mi.muestra)


##! Variable aleatoria de interes: la edad
#los valores de la variable aleatoria para nuestra muestra
y<-mi.muestra$edad
length(y) ##esto debe ser igual al tamanho muestral
y
##+ estadistica descriptiva de la variable 'y' en la muestra
descstat(y)
##+ histograma de la variable 'y' en la muestra
hist(y)



##+================================================
##! III. Test de hipotesis
##+================================================

##- ---------------------
##- (1) Definir hipotesis
##- ---------------------
##? H0: mu=mu*    ##> hipotesis nula
##? HA: mu != mu* ##> hipotesis alternativa

#+ valor del parametro hipotizado
param.hipo<-mu.ast<-16

##- ---------------------
##- (2) Test estadistico
##- ---------------------
##  t=(estimador-param.hipo)/(SE(estimador))

##+ 2a. Estimador
estimador<-m.y<-mean(y)
estimador  #valor del estimador para la muestra

##+ 2b. Error estandar del estimador
##- desviacion estandar de la variable
s.y<-sd(y)

##asumiendo la poblacion de Temuco son los siguientes N habitantes
N<-300000 
f<-n/N #fraccion de muestreo
f
(1-f) #factor de correccion por poblacion finita

se.ybar<-sqrt( (var(y)/n) * (1-f) )
se.ybar
#versus 
sqrt( var(y) / n ) 


##+ 2c. Calculo del test estadistico
estimador-param.hipo
t.cal<-(estimador-param.hipo)/(se.ybar)
t.cal

##- ---------------------
##- (3) Region de rechazo
##- ---------------------
##+ 3a. nivel de confianza/significancia
conf <- 95 #en %
alpha <- 1 - (conf/100)
alpha

##+ 2b. valor tabular distribucion de t-Student
gl <- n - 1; gl
alpha.2 <- alpha/2
t.tab <- abs(qt(1 - alpha.2,df=gl))
t.tab #valor de la dist. de t-student a emplear

##- ---------------------
##- (4) Tomar decision
##- ---------------------
##+ 4a. Comparar con el valor absoluto del test estadistico calculado
abs.tcal<-abs(t.cal)
c(abs.tcal,t.tab)
#- Decisiones posibles
#+ (i) si |t.cal| >= t.tab(alpha,gl), se rechaza H0
#+ (ii) si |t.cal| < t.tab(alpha,gl), no se rechaza H0

##? entonces, que decision tomaria Usted?

##!══════════════════════════════════════════════════════════════════╗
##+ Tarea                                                            ║
##!══════════════════════════════════════════════════════════════════╝
##- 1. Calcule el test estadistico para un nivel de confianza del 90%
##- 2. Explique su decision para este nuevo test.


##!══════════════════════════════════════════════════════════════════╗
##+ Bonus track                                                      ║
##!══════════════════════════════════════════════════════════════════╝
##- Empleando la funcion logica "if()" en el presente contexto para   
## representar una pregunta condicional
## Ejecute la siguiente linea y obtendra la decision en palabras
if(abs.tcal>=t.tab){"Se rechaza H0"} else {"No se rechaza H0"}

#╔═════════════════╗
#║ Que le parecio? ║
#║ Atte            ║
#║ El profesor     ║
#╚═════════════════╝
