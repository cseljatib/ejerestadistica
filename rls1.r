##! Script: "rls1.r"                                              /
##- Sobre:  Ajuste modelo de regresion lineal simple (RLS)       /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##+ Ejemplo: Datos de tamanho de peces.                        /
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
data(fishgrowth2)
df <- fishgrowth2
#?fishgrowth2 #ejecutelo en la consola
head(df)
dim(df)
str(df)

##-Estadistica descriptiva
summary(df$largo)
##estadistica descriptiva para dos variables
summary(df[,c("largo","edad")])

##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("largo","edad")])

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
boxplot(df$largo)
hist(df$largo)

boxplot(df$edad)
hist(df$edad)

##-Dispersion
plot(largo ~ edad, data=df)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## III. Ajuste del modelo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mod1<- lm(largo~edad,data=df)
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
t.fake <- 1:8
t.fake
length(t.fake)
##- generando vector con valor esperado segun el modelo ajustado
y.esperado <- b0.hat + b1.hat * t.fake

##- Grafico de dispersion con valor esperado
plot(largo~edad, data=df)
lines(t.fake, y.esperado, col="red",lwd=2)
##!==fin del grafico de comportamiento


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## V. Valores ajustados y residuales
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Valor ajustado
df$y.aju <- b0.hat + b1.hat * df$edad
head(df)

##-Valor residual
df$e.aju <- df$largo-df$y.aju
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
