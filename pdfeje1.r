##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre:  Aplicando una funcion de densidad de probabilidades   ║
##-║ Detalles: Realiza calculos probabilisticos a partir de los    ║
##-║  parametros dados de una funcion de densidad de probabilidad  ║
## ║  de Weibull.                                                  ║
##!║ Ejemplo: Edad de personas segun nacionalidad en un pais.      ║
##-║-----------------------------------------------------------    ║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝


##+%%%%%%%%%%%%%%%%
##! I. Parametros del modelo de pdf
##+%%%%%%%%%%%%%%%%
##- poblacion 1 -- Sin segregar 
alpha <- 1.761; beta <-   25.743
##- poblacion 2 -- Inmigrantes e hijo(a) de inmigrantes
alpha2 <- 2.5; beta2 <-  14.148 ##12.148
## rango de la variable aleatoria
y<- seq(0.1,80,by=0.01)

##* grafico de densidad
plot(dweibull(y,shape=alpha,scale=beta)~y,type = "l",ylab="densidad de probabilidades",ylim=c(0,0.07))
lines(y, dweibull(y,shape=alpha2,scale=beta2),col = "red")
#lines(y, dweibull(y,shape=2,scale=20),col = "blue")

##? revisar 
##?dweibull
valor.dado<-20
##! CDF (probabilidad acumulada)
pweibull(valor.dado,shape=alpha,scale=beta)


##valores de variable de interes
y.1<-20; y.2<-45


##- Probabilidad de un elemento con un valor de la variable <= y.1?
pweibull(y.1,shape=alpha,scale=beta)

##- Probabilidad de un elemento con un valor de la variable > y.2?
prob.y2<-pweibull(y.2,shape=alpha,scale=beta);prob.y2
##* pero la probabilidad mayor es el complemento
1-prob.y2

##- Probabilidad de un rango (para poblacion 2)
#y.22<-20; y.12<-5;
y.22<-17; y.12<-1;

##*Poblacion 1
prob.y2.1<-pweibull(y.22,shape=alpha,scale=beta);
prob.y2.1

prob.y1.1<-pweibull(y.12,shape=alpha,scale=beta);
prob.y1.1

##? probabilidad del rango 
(prob.y2.1-prob.y1.1)
prob.rango.1<-(prob.y2.1-prob.y1.1)

##*Poblacion 2
prob.y2.2<-pweibull(y.22,shape=alpha2,scale=beta2);
prob.y2.2

prob.y1.2<-pweibull(y.12,shape=alpha2,scale=beta2);
prob.y1.2

##? probabilidad del rango 
(prob.y2.2-prob.y1.2)
prob.rango.2<-(prob.y2.2-prob.y1.2)

##! comparando los rangos
##+ poblacion 1
prob.rango.1

prob.rango.2
