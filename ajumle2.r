##! Script: "ajumle2.r"                                             /
##- Sobre:  Ajuste de una funcion de masa de probabilidades (fmp)  /
##+ Detalles: Emplea estimador "numerico" de maxima verosimilitud,/
##+   mediante una aproximacion "ilustrativa" de optimizacion.   /
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
y<-0:9
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
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# IV. Ajustando el modelo de Fmp
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##Estimador de maxima verosimilud, solucion numerica
## aproximacion que busca explicar que se optimiza

##Maximizar la funcion de maxima verosimilitud de la fmp
##Aca se define (por Ud) una funcion que la calcule
loglike.pois <-function(parametros=parametros,
 data=data){
  sum(dpois(data, lambda=parametros[1],log = T))
}

loglike.pois(0.1,df$y)
loglike.pois(1,df$y)
loglike.pois(4,df$y)

candi.param<-c(0.1,.5,1,1.5,2,3)
fobj1<-loglike.pois(parametros=candi.param[1],df$y)
fobj2<-loglike.pois(candi.param[2],df$y)
fobj3<-loglike.pois(candi.param[3],df$y)
fobj4<-loglike.pois(candi.param[4],df$y)
fobj5<-loglike.pois(candi.param[5],df$y)
fobj6<-loglike.pois(candi.param[6],df$y)

sum.log.vero<-c(fobj1,fobj2,fobj3,fobj4,fobj5,fobj6)
length(sum.log.vero)
length(candi.param)
plot(sum.log.vero~candi.param, type="b",ylab="Log-verosimilitud funcion probabilidades",
     xlab="Valor del parametro")
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!#V. Probando diferentes valores para el parametro
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## estos valores "candidatos" se usan para que la
## funcion "optimice()" de R encuentre el valor optimo.
candidatos<-seq(.1,9,by=0.05)
length(candidatos)
#?optimize?
## por defecto la funcion optimize() maximiza!
optim.loglik.poiss<-optimize(loglike.pois, 
          candidatos, data = df$y, maximum = T)
optim.loglik.poiss

names(optim.loglik.poiss)
lambda.mle<-optim.loglik.poiss$maximum
lambda.mle

###este valor se puede comparar con el resultado
## obtenido mediante el estimador analitico (i.e., solucion cerrada)
mean(df$y)

message("Fin del script!")
#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute estadistica ║
#║ El profesor     ╔════╝
#╚═════════════════╝
