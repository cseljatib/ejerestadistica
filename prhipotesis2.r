##! Script: "prhipotesis2.r"                                        /
##+ Sobre:  Prueba de hipotesis para la estimacion de la media.    /
##- Detalles: Se busca relacionar la prueba de hipotesis          /
##-  con intervalos confidenciales, para realizar inferencia.    /
##! Ejemplo: Datos provenientes de la encuesta CASEN  para      /
##!  dos comunas.                                              /
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
?casen
df<-casen
dim(df)
summary(df$edad)
table(df$comuna)

##! Filtro, emplear solo los datos de la comuna de Temuco

df<-subset(df, comuna=="Las Condes" |  comuna=="La Pintana")
table(df$comuna)
tapply(df$ypc,df$comuna,summary)

tapply(df$edad,df$comuna,summary)

#verifique dimensiones ahora
dim(df)

##estadistica descriptiva
descstat(df[,c("edad","ytot")])
hist(df$edad)

###Tamanho muestral
n<- 15
# seleccionemos los elementos
set.seed(12345)
N<-nrow(df)
N
elem.muestra <- sample(1:N,size=n,replace=FALSE)
elem.muestra

##dichos elementos corresponden a las siguientes valores de variables
mi.muestra <- df[elem.muestra,]
dim(mi.muestra)


#variable aleatoria de interes: la edad
#los valores de la variable aleatoria para nuestra muestra
y<-mi.muestra$edad
length(y) ##esto debe ser igual al tamanho muestral
y
hist(y)


##%%%%%%%%%%%%%%%%
## Test de hipotesis
##%%%%%%%%%%%%%%%%
##H0: mu=mu*
##HA: mu != mu*
param.hipo<-mu.ast<-18

#test.hipotesis
#t=estimador-param.hipo/(SE(estimador))

#estimador
estimador<-m.y<-mean(y)
estimador
#SE estimador
n
se.ybar<-sd(y)/sqrt(n)
se.ybar
#comparar con
#si N<-600000
f<-n/300000
f
(1-f)

sqrt( (var(y)/n) * (1-f) )
#versus
se.ybar

#calculo del test estadistico
estimador-param.hipo
t.calc<-(estimador-param.hipo)/(se.ybar)
t.calc

#comparar con valor tabular
##(a) nivel de confianza/significancia
conf <- 95 #en %
alpha <- 1 - (conf/100)
alpha

##(b) valor de t
gl <- n - 1; gl
alpha.2 <- alpha/2
t.tab <- abs(qt(1 - alpha.2,df=gl))
t.tab #valor de la dist. de t-student a emplear

##comparar con el t calculado
c(abs(t.calc),t.tab)






##y que hay de los intervalos confidenciales?
marg.err <-se.ybar*t.tab

#) Intervalo de confianza, para un nivel de significancia
## estadistico de alpha
lim.inf <- estimador-marg.err
lim.sup <- estimador+marg.err
c(lim.inf,lim.sup)



###probabilidad de encontrar ese valor de t.calc?
# primero revisemos que hace la funcion pt() de R
pt(1.96,df=10000000000)
# es la probabilidad acumulada hasta un valor de la variable aleatoria t para
## grados libertad dados.
1-pt(1.96,df=10000000000)
2*(1-pt(1.96,df=10000000000))

2*(1-pt(abs(t.calc),df=gl))
###este numero corresponde a la probabilidad de obtener un
## valor mayor que el test estadistico calculado, y se conoce como 
## "valor-P" de la prueba de hipotesis.

message("Fin del script!")
#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute estadistica ║
#║ El profesor     ╔════╝
#╚═════════════════╝
