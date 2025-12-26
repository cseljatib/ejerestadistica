## Script: 12_3ajuModCreciNls.R
## Sobre?: Ajuste de modelo no-lineal de crecimiento en altura
##
## Ejemplo con el ajuste del modelo logistico y el de Michaelis-Menten
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

#ajuste del modelo de Michaelis-Menten
mm.nls <- nls(altura~
 (alpha*edad)/(beta+edad), 
  data = df,
start = list(alpha = 170, beta=0.5),trace=T)

summary(mm.nls)
coef(mm.nls)

##- Calcule el RMSD y DA de ambos modelos.
##- Produzca un grafico que compare los valores esperados vs. diametro
## para ambos modelos (i.e., dos curvas)
##- Compare ambos modelos no-lineales, cual es el mejor?


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝