##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Ajuste del modelo de regresion lineal simple (RLS)     ║
##-║ Detalles:  Emplea estimador de minimos cuadrados              ║
##-║ Mas detalles:  Emplea estimador de minimos cuadrados, pero    ║
## ║  mediante algebra matricial.                                  ║
## ║                                                               ║
## ║                                                               ║
##*║ Ejemplo: Datos de gasto social a nivel pais (socioecon).      ║
## ║Relacion entre pobreza y gasto social                          ║
##-║---------------------------------------------------------------║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝


##+ ============================================
##! I. Datos para ejemplo
##+ ============================================
library(datana)
data(socioecon)
##?socioecon #ejecutelo en la consola
##? revise en la metadata cual es la definicion para "poverty"
##? revise en la metadata cual es la definicion para "socspend"
df <- socioecon

head(df)
dim(df)
str(df)


##+ ============================================
##! II. Representacion matricial
##+ ============================================
yvec <- df$poverty #vector de la variable respuesta
yvec[1:7] #veamos las primeras siete observaciones

n<-length(yvec)
n

##- y la matriz  X
xvec <-  df$socspend
Xmat <- cbind(rep(1, n), xvec)
Xmat[1:10,] #veamos las primeras diez observaciones

##> Ahora tenemos todo lo necesario para calcular el vector
##> de parametros estimados mediante formula matricial. 
##+ Ojo que debemos cargar la  libreria MASS (no es necesario
## instalarla, pues viene con R por defecto), para algebra matricial 
library(MASS)
beta.hat.vec <- ginv(t(Xmat) %*% Xmat) %*% (t(Xmat) %*% yvec)
beta.hat.vec

## revise las dimensiones de
dim(beta.hat.vec)
q<-nrow(beta.hat.vec) #numero de coeficientes

b0.hat<-beta.hat.vec[1];b0.hat
b1.hat<-beta.hat.vec[2];b1.hat

##+ Ahora calcular la varianza de los residuales
##- (1) calcular la matriz "hat"
Hmat<- Xmat  %*% ginv(t(Xmat) %*% Xmat) %*% (t(Xmat))
beta.hat.vec

Imat<- diag(n)

##- (2) la varianza
var.hat.e<- ( t(yvec)  %*% (Imat - Hmat) %*% yvec) /(n-(q+1))
var.hat.e
var.hat.e<-as.numeric(var.hat.e)
var.hat.e
sqrt(var.hat.e) #desv. estandar de los residuales del modelo

###

##! ============================================
##+ III. Varianza de los parametros estimados
##! ============================================
##+ 
##- (1) calcular la matriz covarianza-varianza
##* primero
mat.xxinv<- (ginv(t(Xmat) %*% Xmat))
mat.xxinv
mat.cov.betas.ols<- var.hat.e * mat.xxinv
mat.cov.betas.ols


##- (2) la varianza de los beta.ols
var.betas.ols<- diag(mat.cov.betas.ols)
var.betas.ols

##- (3) el SE
se.betas.ols<- sqrt(var.betas.ols)
se.betas.ols

##? ===========================================================
## Tarea/preguntas
##* 1. Compare resultados anteriores con el que se obtiene al aplicar
##* la funcion lm() de R
mod.lm<- lm(poverty~socspend,data=df)
summary(mod.lm)
coef(mod.lm)
(summary(mod.lm))$sigma
## 2. Por ejemplo compare los objetos
beta.hat.vec
coef(mod.lm)
##? ===========================================================

#*╔═════════════════╗
#*║ Fin del script! ║
#*║ Atte.           ║
#*║ El profesor     ║
#*╚═════════════════╝
