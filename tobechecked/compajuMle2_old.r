## Script: "compajuMle2.r"                                             /
## Sobre:  Compara ajuste de funciones ajustadas por el metodo de     /
##  maxima verosimilitud.                                            /
## Detalle: Calcula estadisticos de bondad de ajuste                /
## Ejemplo: Ajuste de las funciones de Poisson y Binomial Negativa /
##  para datos de productividad de papers de estudiantes postgrado/
#----------------------------------------------------------------/ 
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================

####@@@@@@@@@@@@@@@@@@@@@@@@@@
### I. Datos para ejemplo
##@@@Atencion en esta seccion de lectura de los datos
###Atencion: Ud. debe activar la siguiente linea
##df<-read.csv("paperstu.csv")
##y desactivar esta
df <- read.csv(paste(data.dir,"paperstu.csv",sep = "/"))
##@@@Fin a seccion de lectura de los datos

head(df)
dim(df)

df$y<-df$papers
#Grafico de distribucion
hist(df$y)
table(df$y)
barplot(table(df$y),xlab="Numero de papers publicados",
        ylab="Frecuencia (num. de estudiantes)")

####@@@@@@@@@@@@@@@@@@@@@@@@@@
### II. Ajuste de modelos
##(1) Modelo de Fmp de Poisson
##Aca se define (por Ud) una funcion que la calcule
neg.loglike.pois <-function(parametros=parametros,
                        data=data){
  -sum(dpois(data, lambda=parametros[1],log = T))
}
##note que tiene un signo negativo antes de la funcion
##valores iniciales para los parametros a ser estimados
candidatos<-c(1)
candidatos

##?optim
optim.loglik.pois<-optim(c(candidatos[1]),neg.loglike.pois,data=df$y)
optim.loglik.pois

lambda.mle<-optim.loglik.pois$par
lambda.mle

###(2) Modelo de Fmp Binomial Negativa
##Aca se define (por Ud) una funcion que la calcule
loglike.binoneg <-function(parametros=parametros,
                        data=data){
  -sum(dnbinom(data, size=parametros[1],mu=parametros[2],log = T))
}

##valores iniciales para los parametros a ser estimados
candidatos<-c(2,2)
candidatos

##?optim
optim.loglik.binoneg<-optim(c(candidatos[1],candidatos[2]),
                            loglike.binoneg,data=df$y)
optim.loglik.binoneg
size.mle<-optim.loglik.binoneg$par[1]
size.mle
mu.mle<-optim.loglik.binoneg$par[2]
mu.mle

####@@@@@@@@@@@@@@@@@@@@@@@@@@
### III. Comparando graficamente ambos modelos ajustados
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

####@@@@@@@@@@@@@@@@@@@@@@@@@@
### IV. Calculo de estadisticos
####=========================
###
optim.loglik.pois
optim.loglik.pois$value
max.ll.pois<-optim.loglik.pois$value

optim.loglik.binoneg
optim.loglik.binoneg$value
max.ll.binoneg<-optim.loglik.binoneg$value

c(max.ll.pois,max.ll.binoneg)

###AIC
##-2ll+2npara
#num.parametros
num.para.pois<-1
-2*max.ll.pois+(2*num.para.pois)

num.para.binonega<-2
-2*max.ll.binonega+(2*num.para.binonega)

##Tarea: comparar con el estimador
## de los momentos.

#@@@@@@@@@@@@@@@@@
#eso es todo estimad@s alumn@s
#disfRuten!
#saludos
#C
#@@@@@@@@@@@@@@@@@