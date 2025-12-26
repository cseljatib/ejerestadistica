##! Script: "comportarls2.r"                                        /
##+ Sobre:  Comportamiento de modelos ajustados, con diferentes    /
##    transformaciones                                            /
##- Detalles: Ajusta y comparacion tres modelos de regresion     /
## lineal simple (SLR), describiendo el valor esperado de cada  /
##  modelo.                                                    /
##* Ejemplo: Ajuste con datos de area vs. peso de hojas.      /
##-----------------------------------------------------------/ 
##                                                          /
##> Profesor: Christian Salas Eljatib                      /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(leafw2)
df <- leafw2
dim(df)
head(df)
str(df)

##- Variables transformadas

df$r.peso <- sqrt(df$peso)
df$inv.rpeso <- 1 / sqrt(df$peso)
df$ln.peso <- log(df$peso)

descstat(df)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
op <- par(las=1,mfrow=c(2,2))

plot(area~peso, data=df,
     ylab="Area foliar (cm^2)",
     xlab="Peso foliar (gr)")

plot(area~ln.peso, data=df,
     ylab="Area foliar (cm^2)",
     xlab="ln[Peso foliar] (ln[gr])")

plot(area~r.peso, data=df,
     ylab="Area foliar (cm^2)",
     xlab="sqrt[Peso foliar] (sqrt[gr])")

plot(area~inv.rpeso,data=df,
     ylab="Area foliar (cm^2)",
     xlab="inv.sqrt(Peso foliar (inv.sqrt[g]))")

par(op)


#ajuste modelos 
mod1<- lm(area~peso, data=df)
mod2<- lm(area~r.peso,data=df)
mod3<- lm(area~ln.peso,data=df)
mod4<- lm(area~inv.rpeso,data=df)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)


x.fake <- seq(0.04,0.2,by=0.01)

##! guardando los coeficientes en un objeto
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
y.ajumod1 <- b0.hat + b1.hat * x.fake

b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
y.ajumod2 <- b0.hat2 + b1.hat2 * sqrt(x.fake)

b0.hat3<-coef(mod3)[1]
b1.hat3<-coef(mod3)[2]
y.ajumod3 <- b0.hat3 + b1.hat3 * log(x.fake)

b0.hat4<-coef(mod4)[1]
b1.hat4<-coef(mod4)[2]
y.ajumod4 <- b0.hat4 + b1.hat4 * (1/sqrt(x.fake))

##! Grafico de comportamiento-modelos

max.y<-max(y.ajumod1,y.ajumod2,y.ajumod3,y.ajumod4,df$area)
min.y<-min(y.ajumod1,y.ajumod2,y.ajumod3,y.ajumod4,df$area)

plot(area~peso, ylim=c(min.y,max.y),data=df,las=1)
lines(x.fake, y.ajumod1, col="red",lwd=2)
lines(x.fake, y.ajumod2, col="black",lwd=2,lty=2)
lines(x.fake, y.ajumod3, col="blue",lwd=2)
lines(x.fake, y.ajumod4, col="green",lwd=2)

##=============
## Guardando graficos realizados como archivos
##=============
###active las siguientes 2 lineas, si quiere guardar el grafico como un archivo de imagen pdf
#dev.print(pdf,"compara4Modelos.pdf") #
#dev.off()

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
