##! Script: "comparafmp1.r"                                           /
##- Sobre:  Compara ajuste de funciones de probabilidad              /
##+ Detalle: Realiza una comparacion grafica basica                 /
## Ejemplo: Ajuste de las funciones de Poisson y Binomial Negativa /
##  para datos de productividad de papers de estudiantes postgrado/
## --------------------------------------------------------------/ 
##                                                              /
## Profesor: Christian Salas Eljatib                           /
## E-mail: christian.salas AT uchile DOT cl                   /
## Web: https://eljatib.com                                  /
##==========================================================/

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# I. Datos para ejemplo
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
head(papersdocstu)
#Activar siguiente linea para ver metadata
#?papersdocstu
df <- papersdocstu

head(df)
dim(df)

df$y<-df$papers

#Grafico de distribucion
hist(df$y)
table(df$y)
barplot(table(df$y),xlab="Numero de papers publicados",
        ylab="Frecuencia (num. de estudiantes)")
abline(h=0)
##========================



##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# II. Ajuste de modelos
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##Esta seccion se basa en los ejemplos ya vistos sobre 
## ajuste de funciones de masa de probabilidades.

##-================================================
##- II.1 Definiendo las funciones de logaritmo de la verosimilitud
##-================================================
##Se definen las funciones que calculen log.verosimilitud
##! (1) Modelo de Fmp de Poisson
loglike.pois <-function(parametros=parametros,
                        data=data){
  -sum(dpois(data, lambda=parametros[1],log = T))
}
##! (2) Modelo de Fmp Binomial negativa
loglike.binoneg <-function(parametros=parametros,
                           data=data){
  -sum(dnbinom(data, size=parametros[1],mu=parametros[2],log = T))
}

##-================================================
##- II.2 Parametros iniciales para optimizacion
##-================================================
##! (1) Modelo de Fmp de Poisson
candi.pois<-c(1)
candi.pois
##! (2) Modelo de Fmp Binomial Negativo
##valores iniciales para los parametros a ser estimados
candi.binoneg<-c(2,2)
candi.binoneg

##-================================================
##- II.3 Optimizacion
##-================================================
##! (1) Modelo de Fmp de Poisson
optim.loglik.pois<-optim(c(candi.pois[1]),loglike.pois,data=df$y)

optim.loglik.pois
#+ guardando parametros estimados de la funcion ajustada
lambda.mle<-optim.loglik.pois$par
lambda.mle

##! (2) Modelo de Fmp Binomial Negativo
optim.loglik.bneg<-optim(c(candi.binoneg[1],candi.binoneg[2]),
                            loglike.binoneg,data=df$y)

#+ guardando parametros estimados de la funcion ajustada
names(optim.loglik.bneg)
optim.loglik.bneg$par
optim.loglik.bneg$par[1]
size.mle<-optim.loglik.bneg$par[1]
size.mle
mu.mle<-optim.loglik.bneg$par[2]
mu.mle


##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# III. Comparacion grafica de modelos ajustados
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- (a) Representar los datos en terminos de frecuencia relativa
table(df$y)
y.tab<-table(df$y)
y.tab.rel<-y.tab/nrow(df)
#la frecuencia relativa para cada valor de Y observado es
y.tab.rel
#y en porcentaje
100*y.tab.rel
##- (b) Grafico de las frecuencias relativas observadas
plot(y.tab.rel,las=1,ylab="Probabilidad",xlab="Variable aleatoria",
 ylim=c(0,0.35))
##- (c) Probabilidades estimadas, segun modelos ajustados
y.list<-0:19 #valores para los cuales se calculara la probabilidad
##! (c1) Para el modelo de Poisson
probesti.pois<-dpois(y.list,lambda = lambda.mle)
##! (c2) Para el modelo Binomial negativo
probesti.bneg<-dnbinom(y.list, size=size.mle, mu=mu.mle)
##- (d) Superponer las probabilidades estimadas
##! (d1) Para el modelo de Poisson
lines(y.list,probesti.pois, type="b",col="red")
##! (d2) Para el modelo Binomial negativo
lines(y.list,probesti.bneg, type="b",col="blue")
##- (e) Agreguemos una leyenda
legend("topright",c("Observada","Poisson","Binomial negativa"),
         col=c("black","red","blue"),
       lty = c(1,1,1))


message("Aca termina el script!")
#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute el script!  ║
#║ El profesor     ╔════╝
#╚═════════════════╝
