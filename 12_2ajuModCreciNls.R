## Script: 12_2ajuModCreciNls.R
## Sobre?: Ajuste de modelo no-lineal de crecimiento en altura
##
## Ejemplo con el ajuste del modelo logistico
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================
library(datana)
df <- ptaeda2
head(df)
summary(df)

plot(altura~edad, data=df)
library(lattice)
xyplot(altura~edad|as.factor(arbol),data=df, type="b")

xyplot(altura~edad,groups=as.factor(arbol),data=df, type="b")

#ajuste del modelo logistico
logis.nls <- nls(altura ~
 alpha/(1 + exp(b0 + b1 * edad)), data = df,
start = list(alpha = 20, b0 = 1, b1 = -0.05), trace=T)

summary(logis.nls)
coef(logis.nls)

t.fake <- seq(3,30,by=0.1)
yhat<-predict(logis.nls, newdata=data.frame(edad=t.fake))

plot(altura~edad, data=df)
lines(t.fake,yhat,col="red",lwd=3)
abline(h=20,lty=2,col="blue")


##compare el ajuste de este modelo con uno lineal simple

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝