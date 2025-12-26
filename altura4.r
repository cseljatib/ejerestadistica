##! Script: "altura4.r"                                           /
##- Sobre:  Comparacion de tres modelos lineales simple (RLS)    /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##* Ejemplo: Datos de altura-diametro (data= biomass2).        /
##? Mas detalles: Entre otras cosas, el este ejercicio se:    /
##   + calcula valores predichos, y errores.                 /
##   + calcula estadisticos RMSD, DIFA y DA.                /
##! -------------------------------------------------------/ 
##                                                        /
##> Profesor: Christian Salas Eljatib                    /
## E-mail: christian.salas AT uchile DOT cl             /
## Web: https://eljatib.com                            /
##!===================================================/


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(biomass2)
df <- biomass2
#?biomass2 #ejecutelo en la consola
head(df)
dim(df)

##-Estadistica descriptiva
descstat(df[,c("dap","atot")])


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! II. Graficos de interes
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
hist(df$atot)
hist(df$dap)

##-Dispersion
plot(atot~dap, data=df)
plot(atot~dap, data=df, xlab="Diametro (cm)",
ylab="Altura (m)")
plot(atot~dap, data=df)



##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste del modelo 1
##  h=b0+b1*d
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mod1<- lm(atot~dap, data=df)
summary(mod1)
#guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]

#valor ajustado
df$aju <- #b0.hat+b1.hat*df$dap
    fitted(mod1)
head(df)
#valor residual 
df$e.aju <- df$atot - df$aju 
head(df)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Calculo de estadisticos de prediccion
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
n <- nrow(df)
mean.h <- mean(df$atot)
mean.h

##los siguientes es usando la terminologia ocupada en el paper
# de Salas et al 2010 (Remote Sensing of Environment)

#ad, o diferencia agregada (DA)
ad <-  mean(df$e.aju)
ad

#rmsd
rmsd <- sqrt(sum(df$e.aju^2)/n)
rmsd

##aad, o dif. media absoluta (DIFA)
aad <-  mean(abs(df$e.aju))
aad

#calculo de estad. de validacion en %
100*ad/mean.h
100*rmsd/mean.h
100*aad/mean.h

##- Lo mismo anterior se logra empleando la funcion valesta()
## del paquete `datana`
valesta(y.obs=df$atot,y.pred=df$aju)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Ajuste de otro modelo
## ln(h)=b0+b1*e^(-0.03d)
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Creando la variable Y necesaria para el modelo 3
df$ln.h<-log(df$atot)
df$exp.d<-exp(-0.03*df$dap)
plot(ln.h~exp.d, data=df)

descstat(df[,c("dap","exp.d","atot","ln.h")])
mod2<- lm(ln.h~exp.d, data=df)
summary(mod2)
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
b0.hat2
b1.hat2

df$aju2<-exp(fitted(mod2))

valesta(y.obs=df$atot,y.pred=df$aju2)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Tarea sugerida:
## 1. realice un grafico entre el error de prediccion y el valor observado
## 2. realice un grafico entre el error de prediccion y el valor ajustado
## 3. realice un grafico entre el error de prediccion y el diametro
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
