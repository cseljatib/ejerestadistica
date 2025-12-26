##! Script: "ajumle3.r"                                             /
##- Sobre:  Ajuste de una funcion de masa de probabilidades (fmp)  /
##+ Detalles: Emplea estimador "numerico" de maxima verosimilitud,/
##+   mediante una aproximacion "operativa" de optimizacion.     /
##  Ejemplo: Ajuste de funcion de Poisson para datos de         /
##   productividad de papers de estudiantes postgrado.         /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# I. Empleando la funcion de Poisson
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mi.lambda<-2; y<-1
factorial(1)
exp(-2)*(2^1)/factorial(1)

exp(-mi.lambda)*(mi.lambda^y)/factorial(y)

dpois(y,lambda = mi.lambda)
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# II. Graficando la funcion
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#varios valores de Y
y<-0:8
pr.y<-dpois(y,lambda = mi.lambda)
plot(pr.y~y, type="b", ylab = "Pr(Y=y)",las=1)
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# III. Datos para ejemplo
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
##!# IV. Ajustando el modelo de Fmp
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Ajuste mediante maxima verosimilitud

##Maximizar la funcion de maxima verosimilitud de la fmp
##Aca se define (por Ud) una funcion que la calcule
loglike.pois <-function(parametros=parametros,
                        data=data){
  -sum(dpois(data, lambda=parametros[1],log = T))
}
##la que tiene un signo negativo antes de la funcion
loglike.pois(1,df$y)
loglike.pois(4,df$y)

##valores iniciales para los parametros a ser estimados
candidatos<-c(1)
candidatos

##?optim
optim.loglik.pois<-optim(c(candidatos[1]),loglike.pois,data=df$y)
## en caso de recibir un mensaje de advertencia, luego de ejecutado
##  la funcion anterior, no indica un error.   
optim.loglik.pois

names(optim.loglik.pois)
optim.loglik.pois$par
optim.loglik.pois$value

lambda.mle<-optim.loglik.pois$par
lambda.mle
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# V. Graficando el modelo ajustado
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##Note que la funcion de Poisson es
## para variables aleatorias discretas.
##- Primero se calculan las frecuencias relativas
y.tab<-table(df$y)
y.tab.rel<-y.tab/nrow(df) #frec. relativas
y.tab.rel
plot(y.tab.rel,las=1,ylab="Probabilidad",
     xlab="Variable aleatoria",ylim=c(0,0.35))
y.list<-0:19
probesti.poi<-dpois(y.list,lambda = lambda.mle)
lines(y.list,probesti.poi, type="b",col="red")
legend("topright",c("Observada","Poisson"),
       col=c("black","red"),
       lty = c(1,1))
##========================

message("Aca termina el script!")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#*´¨)
#¸.•´¸.•*´¨) ¸.•*¨)
#(¸.•´ (¸.•` ¤ DisfRute el ejemplo!
