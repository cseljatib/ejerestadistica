## Script: 02_5ajuModHd.R
## Sobre?: Calculo de estadisticos de validacion, para dos modelos h-d
##  Entre otras cosas, se: 
##   + calcula valores predichos y errores.
##   + calcula estadisticos RMSD, DIFA y DA.
##   + emplea la dataframe "idahohd2"
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================
library(datana)
data(idahohd2)
df <- idahohd2

data("biomass2")
df<-biomass2

#datos
#?idahohd2 #ejecutelo en la consola
head(df)
dim(df)
str(df)
##cuadro de estadistica descriptiva
descstat(df[,c("d","h")])

#Ajuste modelo 1
#h=b0+b1d
mod1<- lm(h~d, data=df)
summary(mod1)
b0.hat1<-coef(mod1)[1]
b1.hat1<-coef(mod1)[2]

#Ajuste modelo 2
#h=b0+b1(1/d)
df$inv.d <- 1/df$d
mod2<- lm(h~inv.d, data=df)
summary(mod2)
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
b0.hat2
b1.hat2



#valor ajustado, de cada modelo
df$aju1 <- b0.hat1 + b1.hat1 * (df$d)   #modelo 1
df$aju2 <- b0.hat2 + b1.hat2 * (1/df$d) #modelo 2

#valor del error de prediccion, en m. 
df$e.aju1 <- df$h - df$aju1 #modelo 1
df$e.aju2 <- df$h - df$aju2 #modelo 2
head(df)

head(df)

###########################
#Estadisticos de validacion
##los siguientes es usando la terminologia ocupada en el paper
# de Salas et al 2010 (Remote Sensing of Environment)
###########################

n <- nrow(df)
mean.h <- mean(df$h)
mean.h


#ad, o diferencia agregada (DA)
ad.mod1 <-  mean(df$e.aju1)
ad.mod1

ad.mod2 <-  mean(df$e.aju2)
ad.mod2

#rmsd
rmsd.mod1 <- sqrt(sum(df$e.aju1^2)/n)
rmsd.mod1

rmsd.mod2 <- sqrt(sum(df$e.aju2^2)/n)
rmsd.mod2

##aad, o dif. media absoluta (DIFA)
aad.mod1 <-  mean(abs(df$e.aju1))
aad.mod1
aad.mod2 <-  mean(abs(df$e.aju2))
aad.mod2

#calculo de estad. de validacion en %
##modelo 1
100*ad.mod1/mean.h
100*rmsd.mod1/mean.h
100*aad.mod1/mean.h
##modelo 2
100*ad.mod2/mean.h
100*rmsd.mod2/mean.h
100*aad.mod2/mean.h


##Tarea sugerida:
## 1. prepare un cuadro en una hoja a mano, y escriba los estadisticos de
##    validacion para cada modelo (cada fila un modelo).
## 3. compare ambos modelos, basado en los estadisticos de
##    validacion calculados.

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝