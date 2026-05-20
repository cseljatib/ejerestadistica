##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Ajuste de modelo de regresion lineal multiple (RLM).   ║
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

##- ============================================
##! I. Datos para ejemplo
##- ============================================
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

##- ============================================
##! II. Graficos de interes
##- ============================================

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

##!Dispersion acompanhado por grafico de distribucion
##+ marginal, funcion xyhist() del paquete datana
xyhist(x=df$edad,y=df$peso,xlab="Edad (meses)",ylab="Peso (Kg)")
xyhist(x=df$edad,y=df$largo,xlab="Edad (meses)",ylab="Largo (m)")

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
      type="h",angle=30,xlab="Edad (meses)",ylab="Largo (m)",zlab="Peso (kg)")

par(op)
dev.off()

#- ===================================
##+## III. Ajuste de modelos
#- ===================================
##+ Primer modelo, uno de reg. lineal simple
m1 <- lm(peso~edad, data=df)
summary(m1)

##+ Ahora ajustemos un modelo de reg. lineal multiple
# peso_i=beta_0+beta_1(edad)+beta_2(largo)
m2 <- lm(peso~edad+largo, data=df)
summary(m2)

#- =======================================
##+## IV. Graficos de modelos ajustados 
#- =======================================

##- ============================================= 
##* 1) Grafico de dispersion y el valor ajustado RLS
##- =============================================
##+ Este tipo de grafico solo es posible para modelos
##+que dependen de una variable. 
##! En nuestro caso corresponde al modelo 1
plot(peso~edad,data=df)
abline(m1, col="red", lwd=2)

##- ============================================= 
##* 2) Grafico en 3D del modelo RLM ajustado
##- =============================================
library(scatterplot3d)
migraf<-scatterplot3d(df$edad,df$largo,df$peso,type="h")
migraf$plane3d(m2, lty.box = "solid",col="red")

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## V. Revisando mas salidas: tabla ANOVA 
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##- Modelo 1, compare ambas salidas
summary(m1)
anova(m1)

##? hay elementos comunes en ambas? cuales?

##- Modelo 2, compare ambas salidas
summary(m2)
anova(m2)

##? hay elementos comunes en ambas? cuales?


##? ================================================
## Tarea/bonus
## 1. realice el grafico de valor estimado por el modelo de RLM,
## siguiendo un procedimiento paso a paso, y no con una funcion
## preprogramada en R, como ya se realizo.
##* a) definiendo los rangos de los valores de variables
## predictoras a ser ocupadas para obtener los valores estimados de
## la variable respuesta
x  <- seq(min(df$edad),max(df$edad),length.out = 100);
y  <- seq(min(df$largo),max(df$largo),length.out = 100);
##* b) crear una dataframe con todas las posibles combinaciones de x e
## y
dfz<-expand.grid(x,y)
names(dfz)<-c("x","y")
##* c) calcular el valor ajustado de Z, basado en los valores de X e Y
dfz$z<- coef(m2)[1] + coef(m2)[2]*dfz$x + coef(m2)[3]*dfz$y
head(dfz)    
scatterplot3d(dfz$x,dfz$y,dfz$z,highlight.3d = FALSE,color = "steelblue")
##? ================================================

#-╔═════════════════╗
#-║ Fin del script! ║
#-║ Atte.           ║
#-║ El profesor     ║
#-╚═════════════════╝
