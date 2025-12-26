##! Script: "ajumle4.r"                                             /
##- Sobre:  Ajuste de una funcion de masa de probabilidades (fmp)  /
##+ Detalles: Emplea estimador "numerico" de maxima verosimilitud,/
##+   para la funcion Binomial Negativa.                         /
##  Ejemplo: Ajuste de la funcion empleando datos de            /
##   productividad de papers de estudiantes postgrado.         /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# I. Empleando la funcion Binomial Negativa
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#revisar la siguiente funcion
#?dnbinom
## cuantos parametros tiene?
dnbinom(1,size=1, mu = 4)

dnbinom(1,size=1, mu = 4)

##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# II. Graficando la funcion
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#varios valores de Y
y<-0:9
pr.y<-dnbinom(y,size=1, mu = 4)
plot(pr.y~y, type="b", ylab = "Pr(Y=y)",las=1)

pr.y<-dnbinom(y,size=5, mu = 4)
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

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# IV.  Ajustando el modelo de Fmp Binomial Negativa
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Ajuste mediante maxima verosimilitud

##Maximizar la funcion de maxima verosimilitud de la Fmp Binomial Negativa

##- Aca se define (por Ud) una funcion que calcule la funcion
##-  del logaritmo de la verosimilitud
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

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# V. Graficando el modelo ajustado
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##Note que la funcion Binomial Negativa es
## para variables aleatorias discretas.
##- (a) Representemos los datos en terminos de frecuencia relativa
y.tab<-table(df$y)
y.tab.rel<-y.tab/nrow(df)
#la frecuencia relativa para cada valor de Y observado es
y.tab.rel
#y en porcentaje
100*y.tab.rel
##- (b) Grafico de las frecuencias relativas observadas
plot(y.tab.rel,las=1,ylab="Probabilidad",
     xlab="Variable aleatoria",ylim=c(0,0.35))
##- (c) Probabilidades estimadas, segun nuestro modelo ajustado
y.list<-0:19
probesti.bnega<-dnbinom(y.list,size = size.mle, mu=mu.mle)
##- (d) Superponer las probabilidades estimadas 
lines(y.list,probesti.bnega, type="b",col="red")
##- (e) Agreguemos una leyenda
legend("topright",c("Observada","Binomial negativa"),
         col=c("black","red"),
         lty = c(1,1))
##========================

message("Fin del script!")
#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute el ejemplo! ║
#║ El profesor     ╔════╝
#╚═════════════════╝
