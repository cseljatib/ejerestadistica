##! Script: "prhipotesis1.r"                                        /
##+ Sobre:  Prueba de hipotesis para la estimacion de la media.    /
##- Detalles: Se busca relacionar la prueba de hipotesis          /
##-  con intervalos confidenciales, para realizar inferencia.    /
##! Ejemplo: Datos de edad provenientes de la encuesta CASEN.   /
##------------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/

##+%%%%%%%%%%%%%%%%
##! I. Datos
##+%%%%%%%%%%%%%%%%
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

##+%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##! II. Calculo de estadisticos en base a muestreo
##+%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##- Tamanho muestral
n<- 10
##- seleccionemos los elementos
set.seed(12345)
N<-nrow(df)
N
elem.muestra <- sample(1:N,size=n,replace=FALSE)
elem.muestra

##+ dichos elementos corresponden a las siguientes valores de variables
mi.muestra <- df[elem.muestra,]
dim(mi.muestra)


##! Variable aleatoria de interes: la edad
#los valores de la variable aleatoria para nuestra muestra
y<-mi.muestra$edad
length(y) ##esto debe ser igual al tamanho muestral
y
hist(y)


##!%%%%%%%%%%%%%%%%%%%%%%%%
##+ III. Test de hipotesis
##!%%%%%%%%%%%%%%%%%%%%%%%%

##- ---------------------
##- (1) Definir hipotesis
##- ---------------------
##H0: mu=mu*
##HA: mu != mu*

#+ parametro hipotizado
param.hipo<-mu.ast<-16

##- ---------------------
##- (2) Test estadistico
##- ---------------------
#t=(estimador-param.hipo)/(SE(estimador))

##+ 2a. Estimador
estimador<-m.y<-mean(y)
estimador

##+ 2b. Error estandar del estimador
s.y<-sd(y)
#SE estimador
n
s.y/sqrt(n)

##asumiendo la poblacion de Temuco son 292518 habitantes
N<-292518
f<-n/N
f
(1-f)

se.ybar<-sqrt( (var(y)/n) * (1-f) )
#versus
se.ybar

#calculo del test estadistico
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
##comparar con el valor absoluto del t calculado
abs.tcal<-abs(t.cal)
c(abs.tcal,t.tab)
#- Decisiones posibles
#+ (i) si |t.cal| >= t.tab(alpha,gl), se rechaza H0
#+ (ii) si |t.cal| < t.tab(alpha,gl), no se rechaza H0

##! empleemos ahora la funcion logica "if" para 
## representar una pregunta condicional
## Ejecute la siguiente linea y obtendra la decision en palabras
if(abs.tcal>=t.tab){"Se rechaza H0"}else{"No se rechaza H0"}


##!%%%%%%%%%%%%%%%%%%%%%%%%
##+ IV. Valor-p
##!%%%%%%%%%%%%%%%%%%%%%%%%
##- El valor-p=Probabilidad de encontrar >=|t.calc|?
##+  primero revisemos que hace la funcion pt() de R
pt(1.96,df=10000000000)
## es la probabilidad acumulada hasta un valor de la variable
## aleatoria t para los grados libertad dados.
1-pt(1.96,df=10000000000)
##es la probabilidad de un valor mayor al t.dado, una sola cola
2*(1-pt(1.96,df=10000000000))
##versus la probabilidad de un valor mayor al |t.dado|, dos colas

##+  calculo del valor-p del test estadistico
2*(1-pt(abs.tcal,df=gl))
###este numero corresponde a la probabilidad de obtener un
## valor mayor que el test estadistico calculado, y se conoce como 
## "valor-P" de la prueba de hipotesis.


##!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##+ V. Como se relaciona la prueba de hipotesis con los
##+  intervalos confidenciales?
##!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##+ a. Primero calcule el margen de error 
marg.err <-se.ybar*t.tab

##+ b. Intervalo de confianza, para un nivel de significancia
## estadistico de alpha
lim.inf <- estimador-marg.err
lim.sup <- estimador+marg.err
c(lim.inf,lim.sup)

##! Este intervalo de confianza incluye al valor hipotizado de la
##!  prueba de hipotesis establecida?

message("Fin del script sobre prueba de hipotesis!")

##!══════════════════════════════════════════════════════════════════╗
##+ Tarea para Ud.
##- 1. Empleando la funcion logica "if()", responda a la pregunta
##-  anteriormente planteada.
##- 2. Calcule el intervalo de confianza para un nivel de
##-   significancia de 0.01
##!══════════════════════════════════════════════════════════════════╝

#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute estadistica ║
#║ El profesor     ╔════╝
#╚═════════════════╝
