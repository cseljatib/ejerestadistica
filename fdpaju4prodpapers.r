## Script: "fdpaju4prodpapers.r"                                     /
## Sobre:  Ajuste de dos funciones de masa de probabilidades        /
## Ejemplo: Ajuste de las funciones de Poisson y Binomial Negativa /
##  para datos de productividad de papers de estudiantes postgrado/
##---------------------------------------------------------------/ 
##                                                              /
## Profesor: Christian Salas Eljatib                           /
## E-mail: christian.salas AT uchile DOT cl                   /
## Web: https://eljatib.com                                  /
##==========================================================/

## Script: "ajumledosfunc.r"                                              /
## Sobre:  Ajuste funcion de masa de probabilidades                /
## Detalles: Emplea estimador "numerico" de maxima verosimilitud, /
##  mediante una aproximacion "operativa" de optimizacion        /
## Ejemplo: Ajuste de funcion de Poisson para datos de          /
##  productividad de papers de estudiantes postgrado.          /
##------------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/

####@@@@@@@@@@@@@@@@@@@@@@@@@@
### I. Datos para ejemplo
library(datana)
df <- papersdocstu

head(df)
dim(df)

df$y<-df$papers

#Grafico de distribucion
hist(df$y)
table(df$y)
barplot(table(df$y),xlab="Numero de papers publicados",
        ylab="Frecuencia (num. de estudiantes)")

####@@@@@@@@@@@@@@@@@@@@@@@@@@
### II. Ajustando el modelo de Fmp de Poisson
#Ajuste mediante maxima verosimilitud

##Maximizar la funcion de maxima verosimilitud de la Fmp Poisson
##Aca se define (por Ud) una funcion que la calcule
loglike.pois <-function(parametros=parametros,
                        data=data){
  -sum(dpois(data, lambda=parametros[1],log = T))
}
##note que tiene un signo negativo antes de la funcion


##en esta estra
##valores iniciales para los parametros a ser estimados
candidatos<-c(1)
candidatos

##?optim
optim.loglik.pois<-optim(c(candidatos[1]),loglike.pois,data=df$y)
optim.loglik.pois

lambda.mle<-optim.loglik.pois$par
lambda.mle


####@@@@@@@@@@@@@@@@@@@@@@@@@@
### III. Ajustando el modelo de Fmp Binomial Negativa
##la funcion binomial negativa en R
dnbinom(1, size=4,mu = 2)
y<-0:9
f.y<-dnbinom(y, size=4,mu = 2)
plot(f.y~y, type="b", ylab = "Pr(Y=y)",las=1)

#Ajuste mediante maxima verosimilitud

##Maximizar la funcion de maxima verosimilitud de la Fmp Binomial Negativa
##Aca se define (por Ud) una funcion que la calcule
loglike.binoneg <-function(parametros=parametros,
                        data=data){
  -sum(dnbinom(data, size=parametros[1],mu=parametros[2],log = T))
}
##la que tiene un signo negativo antes de la funcion
loglike.binoneg(c(1,2),df$y)
loglike.binoneg(c(4,2),df$y)

##valores iniciales para los parametros a ser estimados
candidatos<-c(2,2)
candidatos

##?optim
optim.loglik.binoneg<-optim(c(candidatos[1],candidatos[2]),
                            loglike.binoneg,data=df$y)
optim.loglik.binoneg
names(optim.loglik.binoneg)
optim.loglik.binoneg$par
optim.loglik.binoneg$par[1]
size.mle<-optim.loglik.binoneg$par[1]
size.mle
mu.mle<-optim.loglik.binoneg$par[2]
mu.mle

####@@@@@@@@@@@@@@@@@@@@@@@@@@
### IV. Comparando graficamente ambos modelos ajustados
####=========================
y.tab<-table(df$y)
y.tab.rel<-y.tab/nrow(df)
plot(y.tab.rel,las=1,ylab="Probabilidad",
     xlab="Variable aleatoria",ylim=c(0,0.35))
y.list<-0:19
probesti.poi<-dpois(y.list,lambda = lambda.mle)
lines(y.list,probesti.poi, type="b",col="red")
probesti.binoneg<-dnbinom(y.list, size=size.mle, mu=mu.mle)
lines(y.list,probesti.binoneg, type="b",col="blue")
legend("topright",c("Observada","Poisson","Binomial negativa"),
       col=c("black","red","blue"),
       lty = c(1,1,1))


###
optim.loglik.pois
optim.loglik.pois$value
max.ll.pois<-optim.loglik.pois$value

optim.loglik.binoneg
optim.loglik.binoneg$value
max.ll.binoneg<-optim.loglik.binoneg$value

c(max.ll.pois,max.ll.binoneg)

-2*max.ll.pois+(2*1)

-2*max.ll.binoneg+(2*2)

##Tarea: Calcule la diferencia media entre las frecuencias
## observadas y estimadas por cada modelo de masa de probabilidades.

#@@@@@@@@@@@@@@@@@
#eso es todo estimad@s alumn@s
#disfRuten!
#saludos
#C
#@@@@@@@@@@@@@@@@@