##! Script: "comportarls1.r"                                        /
##+ Sobre:  Comportamiento de modelos ajustados, con diferentes    /
##    transformaciones                                            /
##- Detalles: Ajusta y comparacion tres modelos de regresion     /
## lineal simple (SLR), describiendo el valor esperado de cada  /
##  modelo.                                                    /
##* Ejemplo: Ajuste con datos de altura de arboles.           /
##-----------------------------------------------------------/ 
##                                                          /
##> Profesor: Christian Salas Eljatib                      /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/


library(datana)
data(idahohd2)
df <- idahohd2
df$h<-df$atot;df$d<-df$dap
#?idahohd2 #ejecutelo en la consola
head(df)
dim(df)
summary(df$h)
summary(df$h)
summary(df[,c("d","h")])
head(df)
str(df)

##cuadro de estadistica descriptiva, usando una funcion
# especifica
df1<- df[,c("d","h")]
head(df1)
descstat(df1)
head(df)

##creando una nueva variable
df$inv.d <- 1/df$d
head(df,2)
#df
tail(df)
hist(df$h)
hist(df$d)
plot(h~d, data=df)
plot(h~d, data=df, xlab="Diametro (cm)",
ylab="Altura (m)")
plot(h~d, data=df)
#compare grafico anterior, con el siguiente
plot(h~d, data=df, las=1)
plot(h~d, data=df, las=1, col="blue")
plot(h~d, data=df, xlab="Diametro (cm)",
     ylab="Altura (m)", las=1, col="blue")

plot(h~d, data=df)
#Ajuste modelo 1
#h=b0+b1*d
mod1<- lm(h~d, data=df)
summary(mod1)

#guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]

#grafico de comportamiento-modelo 1
d.fake <- 10:110
length(d.fake)
h.ajumod1 <- b0.hat + b1.hat * d.fake
plot(h~d, data=df)
lines(d.fake, h.ajumod1, col="red",lwd=2)


#Ajuste modelo 2
#h=b0+b1*(1/d)
plot(h~inv.d, data=df)
mod2<- lm(h~inv.d, data=df)
summary(mod2)
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
b0.hat2
b1.hat2
h.ajumod2 <- b0.hat2 + b1.hat2 * (1/d.fake)

#Ajuste modelo 3
df$inv.rd <- 1/(sqrt(df$d)+10)
plot(h~inv.rd, data=df)
mod3<- lm(h~inv.rd, data=df)
summary(mod3)
b0.hat3<-coef(mod3)[1]
b1.hat3<-coef(mod3)[2]
b0.hat3
b1.hat3
h.ajumod3 <- b0.hat3 + b1.hat3 * (1/(sqrt(d.fake)+10))


##grafico de comportamiento para ambos modelos
plot(h~d, data=df,xlab="Diametro (cm)",
     ylab="Altura (m)", las=1, col="gray")
lines(d.fake, h.ajumod1, col="black", lwd=2, lty=2)
lines(d.fake, h.ajumod2, col="red", lwd=2, lty=1)
lines(d.fake, h.ajumod3, col="blue", lwd=2, lty=1)

legend("bottomright",c("Mod1","Mod2","Mod3"), title="Modelo",
       col = c("black","red","blue"), lty=c(2,1,1), lwd=c(2,2,2))

##=============
## Guardando graficos realizados como archivos
##=============

###active las siguientes 2 lineas, si quiere guardar el grafico como un archivo de imagen pdf
#dev.print(pdf,"comportaModHd.pdf") #
#dev.off()

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
