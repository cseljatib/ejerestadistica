##! Script: "rlm1osos.r"                                            /
##- Sobre:  Ajuste y comparacion de tres modelos de regresion      /
##  lineal multiple.                                              /
##+ Detalles:  Emplea estimador de minimos cuadrados y grafica   /
##+  comportamiento de cada modelo.                             /
##+ Ejemplo: Datos variables de crecimiento en Osos.           /
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
data(bearscomp2)
df <- bearscomp2
#?bearsdepu2 #ejecutelo en la consola
head(df)
dim(df)
str(df)

##-Estadistica descriptiva
summary(df$peso)
##estadistica descriptiva para dos variables
summary(df[,c("peso","edad")])

##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("peso","edad","largo","pechoP","cabezaL")])

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
boxplot(df$peso)
hist(df$peso)

boxplot(df$edad)
hist(df$edad)

##La edad en anhos
summary((df$edad/12))

##- ===================================
##!Grafico de dispersion entre dos variables
##- ===================================
plot(peso ~ edad, data=df)
plot(peso ~ largo, data=df)
plot(peso ~ pechoP, data=df)
plot(peso ~ cabezaL, data=df)

##!Dispersion acompanhado por grafico de distribucion
##+ marginal, funcion xyhist() del paquete datana
xyhist(x=df$edad,y=df$peso,xlab="Edad",ylab="Peso")

##- ===================================
##!Grafico de dispersion entre tres variables
##+ es decir, en tres dimensiones (3D)
##- ===================================
##* Note que debe instalar el paquete 'scatterplot3d' 
require(scatterplot3d)
##- Por ejemplo, peso=f(edad, largo)
op<-par(las=1) 
s3d <-scatterplot3d(bearscomp2$edad,bearscomp2$largo,bearscomp2$peso,
                    pch=16, highlight.3d=TRUE,
      type="h",angle=30,xlab="Edad",ylab="Largo (m)",zlab="Peso (kg)")

par(op)
dev.off()

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## III. Ajuste de modelos
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Primer modelo, uno de reg. lineal simple
m1.osos <- lm(peso~edad, data=df)
summary(m1.osos)

##+ Segundo modelo, uno de reg. lineal multiple
m2.osos <- lm(peso~edad+largo, data=df)
summary(m2.osos)

##+ Tercer modelo, uno de reg. lineal multiple
m3.osos <- lm(peso~edad+largo+pechoP, data=df)
summary(m3.osos)


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## IV. Graficos de modelos ajustados 
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##- ============================================= 
##* 1) Grafico de dispersion y el valor ajustado
##- =============================================
##+ Este tipo de grafico solo es posible para modelos
##+que dependen de una variable. 
##! En nuestro caso corresponde al modelo 1
plot(peso~edad,data=df)
abline(m1.osos, col="red", lwd=2)

##- ============================================= 
##* 2) Grafico entre valor observado y el valor ajustado
##- =============================================
##+ Este tipo de grafico es posible para todo tipo de modelos

##! Guardando los valores ajustados 
df$yaju.1 <- fitted(m1.osos)
df$yaju.2 <- fitted(m2.osos)
df$yaju.3 <- fitted(m3.osos)
min.x<-min(df$yaju.1,df$yaju.2,df$yaju.3,df$peso);min.x
max.x<-max(df$yaju.1,df$yaju.2,df$yaju.3,df$peso);max.x
xlim.h<-c(min.x,max.x)

##---------------------- 
##* 2a) Para el modelo 1
plot(peso~yaju.1,data=df,xlim=xlim.h,ylim=xlim.h,main="Modelo 1",
     xlab="Valor ajustado (kg)", ylab="Valor observado (kg)")
abline(0,1, col="blue", lwd=2, lty=2)

##---------------------- 
##* 2b) Para el modelo 2
plot(peso~yaju.2,data=df,xlim=xlim.h,ylim=xlim.h,main="Modelo 2",
     xlab="Valor ajustado (kg)", ylab="Valor observado (kg)")
abline(0,1, col="blue", lwd=2, lty=2)

##---------------------- 
##* 2c) Para el modelo 3
plot(peso~yaju.3,data=df,xlim=xlim.h,ylim=xlim.h,main="Modelo 3",
     xlab="Valor ajustado (kg)", ylab="Valor observado (kg)")
abline(0,1, col="blue", lwd=2, lty=2)

##? existen mas graficos de evaluacion que se revisan
##? mas adelante.

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## V. Revisando mas salidas: tabla ANOVA 
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##- Modelo 1, compare ambas salidas
summary(m1.osos)
anova(m1.osos)

##? hay elementos comunes en ambas? cuales?

##- Modelo 2, compare ambas salidas
summary(m2.osos)
anova(m2.osos)

##? hay elementos comunes en ambas? cuales?

##- Modelo 3, compare ambas salidas
summary(m3.osos)
anova(m3.osos)

##? hay elementos comunes en ambas? cuales?

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
