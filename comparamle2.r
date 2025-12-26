##! Script: "comparamle2.r"                                           /
##- Sobre:  Compara ajuste de funciones de probabilidad              /
##+ Detalle: Calcula estadisticos basados en maxima verosimilitud   /
## Ejemplo: Ajuste de las funciones de Poisson y Binomial Negativa /
##  para datos de productividad de papers de estudiantes postgrado/
## --------------------------------------------------------------/ 
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
##!======================================

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

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# IV. Calculo de estadisticos de ajuste
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#- (a) Valor maximizado de la funcion de verosimilitud, modelo Poisson 
optim.loglik.pois
optim.loglik.pois$value
## recuerde que le agregamos un signo negativo para la optimizacion,
## por lo tanto el valor maximizado es de signo contrario
max.ll.pois<-(optim.loglik.pois$value)*-1
max.ll.pois

#- (b) Valor maximizado de la funcion de verosimilitud, modelo Binomial negativo 
optim.loglik.bneg
optim.loglik.bneg$value
## recuerde que le agregamos un signo negativo para la optimizacion,
## por lo tanto el valor maximizado es de signo contrario
max.ll.bneg<-(optim.loglik.bneg$value)*-1
max.ll.bneg

## comparando los valores de la verosimilitud
c(max.ll.pois,max.ll.bneg)

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Criterio de informacion de AKaike (AIC)
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#- Penaliza la verosimilitud segun los parametros 
#! num.parametros Poisson
num.para.pois<-1
#! num.parametros Binomial negativa
num.para.bneg<-2

##+ comparando los valores de la verosimilitud
c(max.ll.pois,max.ll.bneg)
##+comparando el numero de parametros
c(num.para.pois, num.para.bneg)

#AIC para modelo ajustado de Poisson
aic.pois<- -2*max.ll.pois+(2*num.para.pois)
aic.pois

aic.bneg<- -2*max.ll.bneg+(2*num.para.bneg)
aic.bneg

## comparando los valores de AIC
c(aic.pois, aic.bneg)

# entre mas bajo mejor!



##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Criterio de informacion de Bayseiano (BIC)
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#- Penaliza la verosimilitud segun los parametros y tamanho muestral 
#! el tamanho muestral es
n<-nrow(df)
n

#BIC para modelo ajustado de Poisson
bic.pois<- -2*max.ll.pois+(num.para.pois*log(n))
bic.pois

bic.bneg<- -2*max.ll.bneg+(num.para.bneg*log(n))
bic.bneg

## comparando los valores de BIC
c(bic.pois, bic.bneg)

##+ cual de los dos modelos seleccionaria?

#+ Tarea: Preparar una tabla comparativa entre modelos

message("Aca termina el script!")
#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute el script!  ║
#║ El profesor     ╔════╝
#╚═════════════════╝
