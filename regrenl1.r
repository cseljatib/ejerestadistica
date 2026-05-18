##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre:  Ajuste modelo de regresion no-lineal (RNL)            ║
##-║ Detalles:  Emplea estimador de minimos cuadrados.             ║
##-║ Mas detalles: Entre otras cosas, en este ejercicio se:        ║
## ║+ revisan como se logra el ajuste a partir de valores iniciales║
## ║ que deben ser datos a los parametros a ser estimados.         ║
## ║predictora                                                     ║
## ║                                                               ║
##*║ Ejemplo: Datos de area y peso de hojas (leafw2).              ║
##-║---------------------------------------------------------------║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(leafw2)
df <- leafw2
dim(df)
head(df)
str(df)

df$r.peso <- sqrt(df$peso)
df$inv.rpeso <- 1 / sqrt(df$peso)
df$ln.peso <- log(df$peso)


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

##! Ajuste modelo regresion lineal
## con variable predictora transformada
##? area_i=b0+b1(1/sqrt(peso_i))
m1 <- lm(area~inv.rpeso, data=df)
summary(m1)

df$fit.slr <- fitted(m1)
df$sr.rls <- rstandard(m1)    

##! Graficos para evaluar el ajuste
op <- par(las=1,mfrow=c(2,2))

##- Valor esperado del modelo segun los datos de las variables
##-  respuesta versus predictora
plot(area~inv.rpeso,data=df,
     ylab="Area foliar (cm^2)",
     xlab="inv.sqrt(Peso foliar (inv.sqrt[g]))")

abline(reg=m1,col="red",lwd=2)   

range(df$peso)

##- Valor esperado del modelo segun los datos de las variables
##-  respuesta versus predictora
plot(area~inv.rpeso,data=df,
     ylab="Area foliar (cm^2)",
     xlab="inv.sqrt(Peso foliar (inv.sqrt[g]))")


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

##========================
##! Ajuste modelo no-lineal
#=========================
hoja.rnl <- nls(area~b0 + b1*peso^b2,  data = df,
                start = list(b0=50, b1=-8, b2=-0.5))
summary(hoja.rnl)

sigma.e.rnl <- summary(hoja.rnl)$sigma
sigma.e.rnl

df$fit.rnl <- fitted(hoja.rnl)
##* residuos "crudos"
df$resi.rnl <- residuals(hoja.rnl)
head(df)
##* residuos estandarizados
df$sr.rnl <- df$resi.rnl/sigma.e.rnl
head(df)
x.ast <- seq(0.04,.20,length=100)  


op <- par(mfrow=c(2,2), las=1,bty="l")

plot(area~peso,data=df,
     ylab="Area foliar (cm^2)",
     xlab="Peso foliar (g)")

lines(x.ast,predict(hoja.rnl, data.frame(peso=x.ast)), 
      lty=1, lwd = 2, col="red")

plot(sr.rnl~peso, data=df,
     col=ifelse(abs(sr.rnl)<2, "green", "red"), 
     cex=ifelse(abs(sr.rnl)<2, 1, 1.00),
     ylab=expression("Residual estudentizado " (cm^2)), 
     xlab="Peso foliar (g)")

abline(h=c(-2,0,2), col="black", lty=2)

plot(sr.rnl~fit.rnl, data=df, col=ifelse(abs(sr.rnl)<2, "green", "red"), 
     cex=ifelse(abs(sr.rnl)<2, 1, 1.00),
     ylab=expression("Residual estudentizado " (cm^2)), 
     xlab="Valor ajustado")
abline(h=c(-2,0,2), col="black", lty=2)

# assess normality with qq plot
res_qq4 <- qqnorm(df$sr.rnl)#, col="blue")
qqline(df$sr.rnl, col="red")
shapiro.test(df$sr.rls)
par(op) 

##efecto de los parametros iniciales
# en el ajuste del modelo de regresion no-lineal
hoja.rnl2 <- nls(area~b0 + b1*peso^b2, 
                 data = df,
                 start = list(b0=1, b1=-10, b2=-0.5),
                 trace=TRUE)
hoja.rnl3 <- nls(area~b0 + b1*peso^b2, 
                 data = df,
                 start = list(b0=10, b1=-10, b2=-1),
                 trace=TRUE)
hoja.rnl4 <- nls(area~b0 + b1*peso^b2, 
                 data = df,
                 start = list(b0=1000, b1=-10, b2=-1),
                 trace=TRUE)
##compare los resultados anteriores con
# emplear los siguientes parametros iniciales
# start = list(b0=0, b1=-10, b2=-5)


#modelo de regresion lineal multiple
#area=b0+b1(peso)+b2(ln(peso))
hoja.rlm<- lm(area~peso+I(log(peso)), data=df)
summary(hoja.rlm)

df$fit.rlm <- fitted(hoja.rlm)
df$sr.rlm <- rstudent(hoja.rlm)    

pred.area.rlm <-predict(hoja.rlm,
                        data.frame(peso=x.ast))

op <- par(mfrow=c(2,2),las=1 )

plot(area~peso,data=df,
     ylab=expression("Area foliar " (cm^2)),
     xlab="Peso foliar (gr)")
lines(x.ast,pred.area.rlm, 
      lty=1, lwd = 2, col="red")

plot(sr.rlm~peso, data=df,
     col=ifelse(abs(sr.rlm)<2, "green", "red"), 
     cex=ifelse(abs(sr.rlm)<2, 1, 1.0),
     ylab="Residual estudentizado", 
     xlab="Peso foliar (gr)")
abline(h=c(-2,0,2), col="black", lty=2)

plot(sr.rlm~fit.rlm, data=df, col=ifelse(abs(sr.rls)<2, "green", "red"), 
     cex=ifelse(abs(sr.rls)<2, 1, 1.00),
     ylab="Residual estudentizado", 
     xlab="Valor ajustado")
abline(h=c(-2,0,2), col="black", lty=2)

# assess normality with qq plot
res_qq4 <- qqnorm(df$sr.rlm)#, col="blue")
qqline(df$sr.rlm, col="red")
#cat("Shapiro - Wilk test of normality\n")
shapiro.test(df$sr.rlm)
par(op)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
