##! Script: "regrematri.r"                                        /
##- Sobre:  Ajuste del modelo de regresion lineal simple (RLS)   /
##+ Detalles:  Emplea estimador de minimos cuadrados, pero      /
##   mediante algebra matricial.                                /
##+ Ejemplo: Datos de tamanho de peces.                        /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(fishgrowth2)
df <- fishgrowth2
#?fishgrowth2 #ejecutelo en la consola
head(df)
dim(df)
str(df)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ II. Representacion matricial
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##+ Se necesita la  librer\'ia MASS,  para definir vector 'yvec' 
library(MASS)
yvec <- df$largo #vector de la variable respuesta
yvec[1:7] #veamos las primeras siete observaciones

n<-length(yvec)
n

##- y la matriz  X
xvec <-  df$edad
Xmat <- cbind(rep(1, n), xvec)
Xmat[1:10,] #veamos las primeras diez observaciones

##> Ahora tenemos todo lo necesario para calcular el vector
##> de parametros estimados mediante formula matricial
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

##! compare resultado anterior con el que se obtiene al aplicar
##! la funcion lm() de R
mod.lm<- lm(largo~edad,data=df)
summary(mod.lm)

coef(mod.lm)

(summary(mod.lm))$sigma 


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
summary(mod.lm)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
