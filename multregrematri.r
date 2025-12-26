##! Script: "multregrematri.r"                                      /
##- Sobre:  Ajuste de un modelo de regresion lineal multiple (RLM) /
##+ Detalles:  Emplea estimador de minimos cuadrados, pero        /
##   mediante algebra matricial.                                 /
##+ Ejemplo: Datos de crecimiento de osos.                      /
## ------------------------------------------------------------/ 
##                                                            /
## Profesor: Christian Salas Eljatib                         /
## E-mail: christian.salas AT uchile DOT cl                 /
## Web: https://eljatib.com                                /
##========================================================/


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(bearsdepu2)
df <- bearsdepu2
#?bearsdepu2 #ejecutelo en la consola
head(df)
dim(df)
str(df)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ II. Representacion matricial
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##+ Se necesita la  librer\'ia MASS,  para definir vector 'yvec' 
library(MASS)
yvec <- df$peso #vector de la variable respuesta
yvec[1:7] #veamos las primeras siete observaciones

n<-length(yvec)

##- y la matriz  X
x1vec <-  df$edad
x2vec <-  df$largo
Xmat <- cbind(rep(1, n), x1vec, x2vec)
Xmat[1:10,] #veamos las primeras diez observaciones

##> Ahora tenemos todo lo necesario para calcular el vector
##> de parametros estimados mediante formula matricial
beta.hat.vec <- ginv(t(Xmat) %*% Xmat) %*% (t(Xmat) %*% yvec)
beta.hat.vec

## revise las dimensiones de
dim(beta.hat.vec)
q<-nrow(beta.hat.vec) #numero de coeficientes
q

b0.hat<-beta.hat.vec[1];b0.hat
b1.hat<-beta.hat.vec[2];b1.hat
b2.hat<-beta.hat.vec[3];b2.hat

##+ Ahora calcular la varianza de los residuales
##- (1) calcular la matriz "hat"
Hmat<- Xmat  %*% ginv(t(Xmat) %*% Xmat) %*% (t(Xmat))

Imat<- diag(n)

##- (2) la varianza
var.hat.e<- ( t(yvec)  %*% (Imat - Hmat) %*% yvec) /(n-(q+1))
var.hat.e
var.hat.e<-as.numeric(var.hat.e)
var.hat.e
sqrt(var.hat.e) #desv. estandar de los residuales del modelo

###

##! compare resultado anterior con el que se obtiene al aplicar
##! la funcion lm() de R
mlr.lm<- lm(peso~edad+largo,data=df)
summary(mlr.lm)

coef(mlr.lm)

(summary(mlr.lm))$sigma 


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ III. Varianza de los parametros estimados
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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

##! compare resultado anterior con el que se obtiene al aplicar
##! la funcion lm() de R
summary(mlr.lm)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
