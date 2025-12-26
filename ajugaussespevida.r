##! Script: "ajugaussespevida.r"                                    /
##- Sobre:  Ajuste de funcion de densidad de probabilidades (fdp)  /
##+ Detalles: Emplea estimador "numerico" de maxima verosimilitud,/
##+   para la funcion de Gauss.                                  /
##  Ejemplo: Ajuste de la funcion empleando datos de            /
##   esperanza de vida.                                        /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# I. Empleando la funcion de Gauss
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#revisar la siguiente funcion
#?dnorm
## cuantos parametros tiene?
dnorm(10, mean=30, sd = 5)
         
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# II. Graficando la funcion
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#varios valores de Y
y<-1:100
f.y<-dnorm(y, mean=30, sd = 5)
plot(f.y~y, type="l", ylab = "f(y)",las=1)

f.y.b<-dnorm(y, mean=50, sd = 15)
plot(f.y.b~y, type="l", ylab = "f(y)",las=1)

#en un mismo grafico las dos curvas
plot(f.y~y, type="l", ylab = "f(y)",las=1)
lines(y,f.y.b, type="l", col="red")

##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# III. Datos para ejemplo
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
##- revisar metadata al activar siguiente linea
#?lifexpect
data(lifexpect)
df <- lifexpect
head(df)
dim(df)

df$y<-df$life.expectancy

##- Grafico de distribucion
hist(df$y)
hist(df$y, xlim=c(0,100))

boxplot(df$y)


##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# IV.  Ajustando el modelo de Fdp de Weibull
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Ajuste mediante maxima verosimilitud

##Maximizar la funcion de maxima verosimilitud de la Fdp de Gauss

##- Aca se define (por Ud) una funcion que calcule la funcion
##-  del logaritmo de la verosimilitud
loglike.gauss <-function(parametros=parametros,
                        data=data){
  -sum(dnorm(data, mean=parametros[1],sd=parametros[2],log = T))
}
##la que tiene un signo negativo antes de la funcion
loglike.gauss(c(20,2),df$y)
loglike.gauss(c(50,13),df$y)

##valores iniciales para los parametros a ser estimados
summary(df$y)

descstat(df$y)
descstat(df$y,full = TRUE)

candidatos<-c(50,5)
candidatos

##?optim
optim.loglik.gauss<-optim(c(candidatos[1],candidatos[2]),
                            loglike.gauss,data=df$y)
optim.loglik.gauss
names(optim.loglik.gauss)
optim.loglik.gauss$par
optim.loglik.gauss$par[1]
mu.mle<-optim.loglik.gauss$par[1]
mu.mle
sd.mle<-optim.loglik.gauss$par[2]
sd.mle

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# V. Graficando el modelo ajustado
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##- (a) La frecuencia relativa para la muestra
##! dado que es una variable continua, debemos construir la
##!  tabla de frecuencia al crear clases de la variable aleatoria.
summary(df$y)
diff(range(df$y))
40/5
w.amp<-5
breaks.yo<-c(0,seq(5,100,by=w.amp))
breaks.yo
y.class <- findInterval(df$y, breaks.yo)
y.class<-y.class*w.amp
head(cbind(df$y,y.class))
y.class <- factor(y.class); y.class
table(y.class)
sum(table(y.class))
y.tab.rel<-table(y.class)/nrow(df)
sum(y.tab.rel)

plot(y.tab.rel,las=1,ylab="Frecuencia relativa",
     xlab="Esperanza de vida (a\u00f1os)",ylim=c(0,0.35))

#- (b) La probabilidad estimada por el modelo ajustado
50:90
espevida.l<-seq(50,90, by=w.amp)
lim.inf <- espevida.l-(w.amp/2);lim.inf
lim.sup <- espevida.l+(w.amp/2);lim.sup
cbind(lim.inf,lim.sup)

##! Calculando probabilidad acumulada hasta cada limite de cada
##!  clase de edad
##+ revisar la funcion pnorm() y en que se diferencia con dnorm()
prob.sup<-pnorm(lim.sup, mean = mu.mle, sd = sd.mle)
prob.inf<-pnorm(lim.inf,  mean = mu.mle, sd = sd.mle)
prob.cl<-prob.sup-prob.inf

sum(prob.cl) #- note que no suma 1

##!lo que resta de probabilidades debe ser asignado
delta.prob<-1-sum(prob.cl)
delta.prob
##+una posibilidad es asignar diferencial uniformemente
length(espevida.l)
delta.prob/length(espevida.l)
##+otra es asignar diferencial proporcionalmente
pondera.cl.ori<-pondera.cl<-prob.cl/sum(prob.cl)
sum(pondera.cl)

prob.cl.nogood<-prob.cl

data.frame(espevida.l,lim.inf,lim.sup,prob.inf,prob.cl.nogood,pondera.cl)
data.frame(espevida.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cl,pondera.cl)
addcl.dife.prob<-pondera.cl*delta.prob
prob.cl<-prob.cl+addcl.dife.prob

df.h0<-data.frame(espevida.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cl,pondera.cl)
df.h<-data.frame(espevida.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cl.nogood,pondera.cl.ori,addcl.dife.prob,prob.cl)
sum(prob.cl)
head(df.h)
dim(df.h)
tail(df.h)
sum(df.h$prob.cl)


##- (c) graficando lo observado y lo estimado
frobs<-as.numeric(y.tab.rel)
##+ creando una dataframe con las columnas necesarias
df.compfrec<-data.frame(espevida.l,frobs,prob.cl)
df.compfrec

plot(frobs~espevida.l, data=df.compfrec,type = "h",las=1,bty="l",
     ylab="Frecuencia relativa",
     xlab="Esperanza de vida (a\u00f1os)")
lines(df.compfrec$espevida.l,df.compfrec$prob.cl,col="blue",type = "o")
legend("topleft",c("Observada","Fdp de Gauss"),
       col=c("black","blue"),
       lty = c(1,1))#, pch=c(NULL,1))


message("Si ves este mensaje, estamos OK!!")

#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute el ejemplo! ║
#║ El profesor     ╔════╝
#╚═════════════════╝
