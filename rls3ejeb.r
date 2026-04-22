##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre:  Ajuste modelo de regresion lineal simple (RLS)        ║
##-║ Detalles:  Valores ajustados y residuales                     ║
##-║ Mas detalles: Entre otras cosas, en este ejercicio:           ║
## ║+ para cada observacion se calculan el valor ajustado por      ║
## ║el modelo y su respectivo residual                             ║
## ║+ se realizan diferentes graficos basados en los valores       ║
## ║ajustados y residuales.                                        ║
## ║                                                               ║
##*║ Ejemplo: Datos de gasto social a nivel pais (socioecon).      ║
## ║Relacion entre pobreza y gasto social                          ║
##-║---------------------------------------------------------------║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝


##+================================================
##! I. Datos
##+================================================
library(datana)
data(socioecon)
##?socioecon #ejecutelo en la consola
df <- socioecon

head(df)
dim(df)
str(df)

##-Estadistica descriptiva
summary(df$poverty)
##estadistica descriptiva para dos variables
descstat(df[,c("poverty","socspend")])

##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("poverty","socspend")])

##+================================================
##! II. Graficos de interes
##+================================================
##-Distribucion
boxplot(df$poverty)
hist(df$poverty)

boxplot(df$socspend)
hist(df$socspend)

##-Dispersion
plot(poverty ~ socspend, data=df)

##+================================================
##! III. Ajuste del modelo
##+================================================
mod1<- lm(poverty~socspend,data=df)
summary(mod1)


##! Parametros estimados
##*1. Los coeficientes
##-Guardando los coeficientes en un objeto
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
b0.hat
b1.hat

##*2. La raiz cuadrada de la varianza estimada del error 
sigma.e<-(summary(mod1))$sigma 
sigma.e


##+================================================
##! IV. Valores ajustados y residuales
##+================================================
##-Valor ajustado (o estimado) para cada observacion
df$y.aju <- b0.hat + b1.hat * df$socspend
head(df)

##-Valor residual para cada observacion
df$e.aju <- df$poverty-df$y.aju
head(df)

tail(df)

##+ Valores ajustados de Y con el modelo, empleando la 
## funcion fitted(), comparela la columna con "y.aju"
df$y.aju.b <- fitted(mod1) 
head(df)

##+ Valores residuales de Y con el modelo, empleando la
## funcion residuals(), comparela la columna con "e.aju"
df$e.aju.b <- residuals(mod1)


##+================================================
##! V. Graficos de residuales y relacionados
##+================================================
##-a) Residual versus valor ajustado
plot(e.aju~y.aju, data=df)
abline(h=0)

##-b) Residual versus variable predictora
plot(e.aju~socspend, data=df)
abline(h=0, col="blue")

##-c) Valor observados versus ajustado
plot(poverty~y.aju, data=df)

##* y que tal esto?
plot(poverty~y.aju, data=df)
abline(0,1,col="blue")

##* cambiemos los limites mejor
plot(poverty~y.aju, data=df, ylim=c(0,max(df$poverty,df$y.aju)), xlim=c(0,max(df$poverty,df$y.aju)))
abline(0,1,col="blue")

##!==fin de algunos graficos de evaluacion


##-============================================================
##+ Bonus track                                               
##-═════════════
##? Revise las tendencias graficas con una curva suavizada en
##*a) Residual versus valor ajustado
plotrend(x=df$y.aju,y=df$e.aju,xlab = "Valor ajustado", ylab = "Residual")
abline(h=0,col="blue")

##*b) Valor observados versus ajustado
plotrend(x=df$y.aju,y=df$poverty,xlab = "Valor ajustado", ylab = "Valor ajustado")
##-============================================================


##!════════════════════════════════════════════════════════════════╗
##+ Tarea                                                          ║
##- 1. Que le parecen los graficos realizados? como debieran ser   ║
## en condiciones ideales.                                         ║
##!════════════════════════════════════════════════════════════════╝


##-╔══════════════════════╗
##-║ Estimad@ estudiante: ║
##-║ DisfRute estadistica ║
##-║ El profesor     ╔════╝
##-╚═════════════════╝
