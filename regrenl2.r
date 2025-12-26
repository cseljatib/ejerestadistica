##! Script: "regrenl2.r"                                          /
##- Sobre:  Ajuste de modelo de regresion no lineal              /
##+ Detalles:  Emplea datos de crecimiento de osos              /
##+ Aplicacion con modelo de crecimiento logistico             /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
df <- bearscomp2
descstat(df[,c("edad","peso")])

#ajuste modelo de crecimiento logistico
logis.nls <- nls(peso~alpha/ (1 + exp(b0+b1*edad)),
                 data = df,    # trace=T,
                 start = list(alpha=220,b0=1, b1=-0.05))

#Note que para el valor inicial de alpha utilizado
# es el maximo de la variable respuesta.
summary(logis.nls)


#modelo de comportamiento
t.fake <- seq(8,177,by=0.1)  

y.creci.logist <-predict(logis.nls,
                         data.frame(edad=t.fake))

plot(peso~edad,data=df, bty="l", 
     xlab="Edad (num.meses)",ylab="Peso (Kg)")
lines(t.fake,y.creci.logist, 
      lty=1, lwd = 2, col="blue")

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
