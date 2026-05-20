##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre:  Ajuste modelo de regresion lineal simple (RLS)        ║
##-║ Detalles:  Valores esperados del modelo ajustado              ║
##-║ Mas detalles: Entre otras cosas, en este ejercicio se:        ║
## ║+ calculan los valores esperados para un modelo ajustado.      ║
## ║+ representa lo anterior graficamente sobre la dispersion de   ║
## ║los datos                                                      ║
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
##? revise en la metadata cual es la definicion para "poverty"
##? revise en la metadata cual es la definicion para "socspend"
df <- socioecon

head(df)
dim(df)
str(df)

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
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
b0.hat
b1.hat

##*2. La raiz cuadrada de la varianza estimada del error 
sigma.e<-(summary(mod1))$sigma 
sigma.e

##+================================================
##! IV. Grafico de comportamiento
##+================================================
##- primero revisemos nuevamente el  grafico de dispersion
plot(poverty~socspend, data=df)

##- generando un vector ficticio con la variable predictora
x.ast <- 0:35
x.ast
length(x.ast)
##- generando vector con valor estimado de Y para valores dados de X
## segun el modelo ajustado
##- recuerde que
b0.hat + b1.hat * 20
##- y ahora entonces obtenemos el vector
y.esperado <- b0.hat + b1.hat * x.ast
y.esperado

##- Grafico de dispersion con valor esperado
plot(poverty~socspend, data=df)
lines(x.ast, y.esperado, col="red",lwd=2)
##!==fin del grafico de comportamiento



##!════════════════════════════════════════════════════════════════╗
##+ Tarea                                                          ║
##- 1. Agregue las etiquetas correctas en cada eje, es decir,      ║
## incorporando el nombre y unidades de cada variable.             ║
##- 2. En que se diferencia el grafico de dispersion anterior si   ║
## emplea la sintaxis:                                             ║
## > plot(poverty~socspend, data=df, las=1)                              ║
##!════════════════════════════════════════════════════════════════╝


##-╔══════════════════════╗
##-║ Estimad@ estudiante: ║
##-║ DisfRute estadistica ║
##-║ El profesor     ╔════╝
##-╚═════════════════╝
