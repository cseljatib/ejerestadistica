##! Script: "comportarls3.r"                                       /
##- Sobre:  Ajuste de tres modelos de regresion lineal            /
##+ Detalles:  Emplea estimador de minimos cuadrados y grafica   /
##+  comportamiento de cada modelo.                             /
##+ Ejemplo: Datos de tamanho de peces.                        /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos
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

##+ Int.Conf. coef.estimados
coef(mod1)
confint(mod1, level=0.95) 

anova(mod1)
##! Suma cuadrado del error
deviance(mod1) 

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

## valor predicho por el modelo para edad 5?
b0.hat+b1.hat*5

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


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## VI. Ajuste de otro modelo (transformacion de X)
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##y=b0+b1(1/x)
df$raiz.largo<-sqrt(df$largo)
df$ln.edad<-log(df$edad)
plot(raiz.largo~ln.edad, data=df)


mod2<- lm(raiz.largo~ln.edad,data=df)
summary(mod2)

##-Guardando los coeficientes en un objeto
coef(mod2)
coef(mod2)[1]
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]

b0.hat2
b1.hat2

## valor predicho por el modelo2 para edad 5?
b0.hat2+b1.hat2*(1/5)


##- generando vector con valor esperado para cada modelo ajustado
t.fake<-seq(1,8,by=0.01)
y.esperado <- b0.hat + b1.hat * t.fake
y.esperado2 <- b0.hat2 + b1.hat2 * (1/t.fake)

##- Grafico de dispersion con valor esperado
plot(largo~edad, data=df)
lines(t.fake, y.esperado, col="red",lwd=2)
lines(t.fake, y.esperado2, col="blue",lwd=2)
##!==fin del grafico de comportamiento


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## VII. Ajuste de otro modelo (transformacion de Y)
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##ln(y)=b0+b1(x)
df$ln.y<-log(df$largo)
df$inv.edad2<-1/(sqrt(df$edad))
plot(ln.y~edad, data=df)

mod3<- lm(ln.y~inv.edad2,data=df)
summary(mod3)

##-Guardando los coeficientes en un objeto
b0.hat3<-coef(mod3)[1]
b1.hat3<-coef(mod3)[2]

## valor predicho por el modelo2 para edad 5?
b0.hat3+b1.hat3*(5) ##esto predice el ln(largo)
## se debe transformar hacia la variable largo
exp(b0.hat3+b1.hat3*(5))

##- generando vector con valor esperado para cada modelo ajustado
t.fake<-seq(1,8,by=0.01)
y.esperado <- b0.hat + b1.hat * t.fake
y.esperado2 <- (b0.hat2 + b1.hat2 * log(t.fake))^2
y.esperado3 <- exp(b0.hat3 + b1.hat3 *(1/sqrt(t.fake)))

##- Grafico de dispersion con valor esperado
plot(largo~edad, data=df,col="gray",las=1)
lines(t.fake, y.esperado, col="red",lwd=2)
lines(t.fake, y.esperado2, col="blue",lwd=2)
lines(t.fake, y.esperado3, col="green",lwd=2)
##!==fin del grafico de comportamiento


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
