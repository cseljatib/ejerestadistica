##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Ajuste de un modelo de regresion lineal simple y       ║
## ║ otro multiple.                                                ║
##-║ Detalles:  Emplea estimador de minimos cuadrados              ║
##-║ Mas detalles:  grafica comportamiento de cada modelo.         ║
## ║                                                               ║
## ║                                                               ║
##*║ Ejemplo: Datos de variables de crecimiento de                 ║
## ║ osos (bearscomp2).                                            ║
##-║---------------------------------------------------------------║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝

##- ===================================
##! I. Datos para ejemplo
##- ===================================
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

## creemos una nueva variable: edad al cuadrado
df$e2<-df$edad^2 
##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("peso","edad","e2")])

##- ===================================
##! II. Graficos de interes
##- ===================================
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
plot(peso ~ e2, data=df)

#note que lo mismo anterior se logra con
plot(peso ~ I(edad^2), data=df)

##- ===================================
##! III. Ajuste de modelos
##- ===================================
##+ Primer modelo, uno de reg. lineal simple
m1.rls<- lm(peso~edad, data=df)
summary(m1.rls)

##+ Segundo modelo, uno de reg. lineal multiple
m2.rlm <- lm(peso~edad+e2, data=df)
summary(m2.rlm)


##-======================================================
##! IV. Valores esperados para ambos modelos
##-======================================================

##! Guardando los valores ajustados
## guardando los coeficientes en un objeto
coef(m1.rls)
b0.hat<-coef(m1.rls)[1]
b1.hat<-coef(m1.rls)[2]

range(df$edad)
## grafico de comportamiento-modelo 1
x.ast <- 5:180
length(x.ast)
y.espemod1 <- b0.hat + b1.hat * x.ast
plot(peso~edad, data=df)
lines(x.ast, y.espemod1, col="red",lwd=2)

##+ valor esperado MLR
coef(m2.rlm)
b0.hat2<-coef(m2.rlm)[1]
b1.hat2<-coef(m2.rlm)[2]
b2.hat2<-coef(m2.rlm)[3]

y.espemod2 <- b0.hat2 + b1.hat2 * x.ast + b2.hat2 * x.ast^2

##+======================================================
##! IV. Grafico de comportamiento para los dos modelos
##+======================================================
plot(peso~edad, data=df)
lines(x.ast, y.espemod1, col="red",lwd=2)
lines(x.ast, y.espemod2, col="blue",lwd=2, lty=2)


#-╔═════════════════╗
#-║ Fin del script! ║
#-║ Atte.           ║
#-║ El profesor     ║
#-╚═════════════════╝
