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

df <- read.csv("..//data/leafw2.csv", header=T)
dim(df)
head(df)
str(df)
library(datana)

##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("peso","area")])

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


#ajuste modelo 
# area =b0+b1(1/sqrt(peso))
hoja.rls <- lm(area~inv.rpeso, data=df)
summary(hoja.rls)

df$fit.slr <- fitted(hoja.rls)
df$sr.rls <- rstudent(hoja.rls)    

#graficos de diagnostico
op <- par(las=1,mfrow=c(2,2))
plot(area~inv.rpeso,data=df,
     ylab="Area foliar (cm^2)",
     xlab="inv.sqrt(Peso foliar (inv.sqrt[g]))")

abline(reg=hoja.rls,col="red",lwd=2)   

plot(sr.rls~inv.rpeso, data=df,
     col=ifelse(abs(sr.rls)<2, "green", "red"), 
     cex=ifelse(abs(sr.rls)<2, 1, 1.0), 
     ylab=expression("Residual estudentizado " (cm^2)), 
     xlab="inv.sqrt(Peso foliar (inv.sqrt[g]))")
abline(h=c(-2,0,2), col="black", lty=2)

plot(sr.rls~fit.slr, data=df, col=ifelse(abs(sr.rls)<2, "green", "red"), 
     cex=ifelse(abs(sr.rls)<2, 1, 1.00),
     ylab=expression("Residual estudentizado " (cm^2)), 
     xlab="Valor ajustado")
abline(h=c(-2,0,2), col="black", lty=2)

# assess normality with qq plot
res_qq4 <- qqnorm(df$sr.rls)#, col="blue")
qqline(df$sr.rls, col="red")
shapiro.test(df$sr.rls)

par(op)


##=============
## Guardando graficos realizados como archivos
##=============

###active las siguientes 2 lineas, si quiere guardar el grafico como un archivo de imagen pdf
#dev.print(pdf,"compara3ModelosHd.pdf") #
#dev.off()

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
