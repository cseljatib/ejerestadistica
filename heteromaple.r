##! Script: "heteromaple.r"                                        /
##- Sobre:  Heterocedasticidad en regresion                       /
##+ Detalles:  Emplea estimador de minimos cuadrados, pero       /
##   mediante algebra matricial.                                /
##+ Ejemplo: Datos de biomasa de follaje de Mapple.            /
## -----------------------------------------------------------/ 
##                                                           /
##* Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl                /
##> Web: https://eljatib.com                              /
##=======================================================/


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
df<-maple
head(df)
dim(df)
str(df)
n.sm<-nrow(df); n.sm

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ II. Ajuste del modelo 
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
m1.sm <- lm(leaf~dbh, data=df)
summary(m1.sm)


##+ Residuales

df$res <- residuals(m1.sm)  # almacena residuales
df$studres <- rstudent(m1.sm)  # almacena residuales studentizados
df$yaju <- fitted(m1.sm)  # almacena valores ajustados

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ III. Graficando los residuales para detectar algun patron
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

ylim.h<-max(abs(df$studres))*1.1
min.x.h<-min(df$yaju)*0.01; max.x.h<-max(df$yaju)*1.1
plot(studres~yaju, data=df, ylim=c(-ylim.h,ylim.h),
     xlim=c(min.x.h,max.x.h),
     col=ifelse(abs(df$studres)<3, "black", "purple"), 
     cex=ifelse(abs(df$studres)<3, 1.5, 2.0), 
     ylab=list(expression("Residual estudentizado")),
        xlab="Valor ajustado (kg)") 
abline(h=0, col="red")
smoothfit(x=df$yaju, y=df$studres)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ IV. Prueba de hipotesis para evaluar homocedasticidad
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##-(i) Definiendo cuatro grupos 
nrow(df)
##* usando la funcion assigncl() del paquete datana
df<-assigncl(data=df,variable = "yaju")
head(df)
table(df$yaju.class)

##-(ii) Aplicando el test estadistico de Levene
#library(car)
leveneTest(df$res, df$yaju.class)

##-(iii) Aplicando el test estadistico de Bartlett
bartlett.test(df$res, df$yaju.class)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
