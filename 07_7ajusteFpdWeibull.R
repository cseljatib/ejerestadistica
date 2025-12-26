## Script: 07_7ajuFdpWeibull.R                       /
## Sobre:  ajuste fdp de Weibull.                   /
## emplea estimacion por maxima verosimilitud, con /
## funcion pre-escrita                            /
#------------------------------------------------/ 
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================

##I. Datos
library(datana)
data(pspLlancahue)
df<-pspLlancahue
head(df)
list.d <- df$dbh
summary(list.d)

library(stats4)


## II. Ajuste

##@@@Atencion en esta seccion sobre cargar un script con funciones
###Ud. debe activar la siguiente linea
#source("fxAjuWeibull.R")
#y desactivar esta
source(paste(r.dir,"/fxAjuWeibull.R",sep=""))
##@@@Fin a seccion de cargado de un script


#ajuste mediante MLE, emplear funcionn fitw2(), como sigue
sum.Weibfit <- fitw2(list.d)
sum.Weibfit

#parametro de forma
alpha.hat <-  sum.Weibfit$par[1]
se.shape <-  attributes(sum.Weibfit$summaryFit)$coef['alpha',2]

#parametro de escala
beta.hat <-sum.Weibfit$par[2]
se.scale<-attributes(sum.Weibfit$summaryFit)$coef['beta',2]



## que implica esto?? ocupemos la expresion de la CDF para
## el modelo de Weibull (eq. 7.15, pag153, del libro)
#por ejemplo, para un diametro de 12cm
1 - exp(-(12/beta.hat)^alpha.hat)
#y otros
1 - exp(-(27.5/beta.hat)^alpha.hat)
1 - exp(-(22.5/beta.hat)^alpha.hat)

## Ahora apliquemos el modelo a los datos observados, 
##  usando la funcion ajustada
w.amp <- 5
dbh.l<-seq(5,85, by=w.amp)
lim.inf <- dbh.l-(w.amp/2)
lim.sup <- dbh.l+(w.amp/2)
probw.sup<-1 - exp(-(lim.sup/beta.hat)^alpha.hat)
probw.inf<-1 - exp(-(lim.inf/beta.hat)^alpha.hat)
probw.cd<-probw.sup-probw.inf
sum(probw.cd)

##lo que resta de probabilidades debe ser asignado
deltaw.prob<-1-sum(probw.cd)
deltaw.prob
#una posibilidad es asignar diferencial uniformemente
deltaw.prob/length(dbh.l)
#otra es asignar diferencial proporcionalmente
pondera.cd<-probw.cd/sum(probw.cd)
sum(pondera.cd)

data.frame(dbh.l,lim.inf,lim.sup,probw.inf,probw.sup,probw.cd,pondera.cd)
addw.dife.prob<-pondera.cd*deltaw.prob
probw.cd<-probw.cd+addw.dife.prob

data.frame(dbh.l,lim.inf,lim.sup,probw.inf,probw.sup,probw.cd)
sum(probw.cd)

#Para visualizar el ajuste del modelo, se puede proceder como sigue
h<-hist(list.d,plot=F)
frec.rel<-h$counts/sum(h$counts)
plot(dbh.l,frec.rel,col="black",type = "o",las=1,bty="l",
       ylab="Frecuencia relativa",xlab="Diametro (cm)")
lines(dbh.l,probw.cd,col="red",type = "o")
  legend("topright",c("Observada","Fdp Weibull"),
         col=c("black","red"),
         lty = c(1,1), pch=c(1,1))

#@@@@@@@@@@@@@@@@@
#eso es todo estimad@s alumn@s
#disfRuten!
#saludos
#C
#@@@@@@@@@@@@@@@@@