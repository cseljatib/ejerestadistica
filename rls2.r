##! Script: "rls2.r"                                              /
##- Sobre:  Ajuste modelo de regresion lineal simple (RLS)       /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##+ Ejemplo: Datos de gasto social a nivel pais.  Relacion     /
##+  entre inequidad versus gasto social                      /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(socioecon)
head(socioecon)
#?socioecon #ejecutelo en la consola
df <- socioecon

head(df)
dim(df)
str(df)


##-Estadistica descriptiva
summary(df$poverty)
##estadistica descriptiva para dos variables
summary(df[,c("gini","socspend")])

##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("gini","socspend")])

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
boxplot(df$gini)
hist(df$gini)

boxplot(df$socspend)
hist(df$socspend)

##-Dispersion
## relacion entre gasto social y inequidad
plot(gini~socspend, data=df)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## III. Ajuste del modelo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mod1<- lm(gini~socspend,data=df)
summary(mod1)


##- Algunos estadisticos de interes
##- RMSE del modelo
(summary(mod1))$sigma 
sigma.e<-(summary(mod1))$sigma 
sigma.e

##+ Int.Conf. coef.estimados
coef(mod1)
confint(mod1, level=0.95) 

##+ Tabla ANOVA del modelo ajustado
anova(mod1)
##! Suma cuadrado del error
deviance(mod1) 

##- compare lo anterior con
(summary(mod1))$sigma^2

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## IV. Grafico de comportamiento
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
##-Guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]

b0.hat
b1.hat
##- generando vector ficticio con la variable predictora
x.fake <- 5:35
x.fake
length(x.fake)
##- generando vector con valor esperado segun el modelo ajustado
y.esperado <- b0.hat + b1.hat * x.fake

##- Grafico de dispersion con valor esperado
plot(gini~socspend, data=df)
lines(x.fake, y.esperado, col="red",lwd=2)
##!==fin del grafico de comportamiento


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## V. Valores ajustados y residuales
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Valor ajustado
df$y.aju <- b0.hat + b1.hat * df$socspend
head(df)

##-Valor residual
df$e.aju <- df$gini-df$y.aju
head(df)

tail(df)

##+ Valores ajustados de Y con el modelo, empleando
##+ funcion fitted(), comparela la columna con "y.aju"
df$y.aju.b <- fitted(mod1) 
head(df)

##+ Valores residuales de Y con el modelo, empleando
##+ funcion residuals(), comparela la columna con "e.aju"
df$e.aju.b <- residuals(mod1)


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
