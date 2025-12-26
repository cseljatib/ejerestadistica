##! Script: "intpredslr.r"                                          /
##+ Sobre:  Intervalos de confianza para valores ajustados y       /
##  predichos.                                                    /
##- Detalles: Muestra intervalos mencionados a partir
## de un modelo de regresion lineal simple (SLR).               /
##  modelo.                                                    /
##* Ejemplo: Ajuste con datos de Ozono.                       /
##-----------------------------------------------------------/ 
##                                                          /
##? Profesor: Christian Salas Eljatib                       /
##> E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/

library(datana)
df<-datana::airnyc2
dim(df)
descstat(df[,c("ozono","rad.solar")])
n<-nrow(df);n
##Note que hay observaciones con valores vacios
na.oz<-nrow(airnyc2[is.na(airnyc2$ozono),]);na.oz

df.h1<-df[!is.na(df$ozono),]
dim(df.h1)
df.h<-df.h1[!is.na(df.h1$rad.solar),]
dim(df.h)
descstat(df.h[,c("ozono","rad.solar")])
naju<-nrow(df.h);naju
mod.oz<-lm(ozono~rad.solar, data=df.h)
summary(mod.oz)

##! ======================================================
##+ Intervalos de conf. de estimacion y de prediccion
##! ======================================================

x1<-100;x2<-150;x3<-200

x.ast<- data.frame(rad.solar =c(100, 150, 200))
x.ast

##El objeto {\tt x.ast} es una dataframe, y contiene 
##la variable predictora para el modelo de interes, y
##  dicha variable  tiene el mismo nombre que la variable
## predictora del modelo ajustado.

##+(1) Los valores estimados por el modelo para la variable respuesta
## se obtienen mediante la funcion predict() 
y.estimados<-predict(mod.oz,newdata = x.ast)
y.estimados

##+(2) Los intervalos confidenciales  se obtienen mediante
## funcion predict(), con opcion interval, igual a confidence
int.conf.estimacion<-predict(mod.oz,x.ast,interval="confidence")
int.conf.estimacion

##+(3) Los intervalos confidenciales  se obtienen mediante
## funcion predict(), con opcion interval, igual a prediction
int.conf.predicc<-predict(mod.oz,x.ast,interval="prediction")
int.conf.predicc

##! Grafico
rad.solar<-7:335

a <- predict(mod.oz, data.frame(rad.solar), interval="confidence")
b <-  predict(mod.oz,data.frame(rad.solar),interval="prediction")
ylim.h<-range(a,b)
ylim.h[2]<-ylim.h[2]*1.1

plot(ozono ~ rad.solar, data=df.h, bty="l",
     xlim=range(rad.solar), ylim=ylim.h,  
     las=1)
abline(mod.oz,lwd=2) #(tmp.lm)

lines(rad.solar, sort(a[,2]), lty=2, col="blue",lwd=2)
lines(rad.solar, sort(a[,3]), lty=2, col="blue",lwd=2)

lines(rad.solar, sort(b[,2]), lty=3, col="red",lwd=2)
lines(rad.solar, sort(b[,3]), lty=3, col="red",lwd=2)

legend("topleft",
       c("Estimado","Int.Conf. ajustado","Int.Conf. prediccion"),       
       lty=c(1,2,3),
       col=c("black","blue","red"))


##- ==========================
##+ Pregunta bonus
##- ==========================
##! (1) Compare el modelo ajustado mod.oz, con uno que emplee
## la dataframe airnyc2, sin depurar. 
##! (2) En que se diferencia la depuracion de valores perdidos
##  realizados en el script, con la aplicacion de la siguiente
##  sintaxis
##  > new.df<-na.omit(airnyc2)
##! (3) como modifica el nivel de confianza? Por ejemplo, para un
## 90% de confianza estadistica emplear
##  > predict(.....,level=0.90)
##! (4) Si desea revisar los valores especificos de los errores
## estandar, proceder como 
##  > predict(.....,se.fit=TRUE)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
