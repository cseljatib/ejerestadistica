##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Ajuste de un modelo de regresion lineal simple y       ║
## ║ otro multiple.                                                ║
##-║ Detalles:  Emplea estimador de minimos cuadrados              ║
##-║ Mas detalles:  grafica comportamiento de cada modelo.         ║
## ║                                                               ║
## ║                                                               ║
##*║ Ejemplo: Datos de variables de crecimiento de                 ║
## ║ osos (casen).                                                 ║
##-║---------------------------------------------------------------║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝

##- ===================================
##! I. Datos para ejemplo
##- ===================================
library(datana)
head(casen)
##- revise la metadata al activar la siguiente linea
#?casen
df<-casen
dim(df)
n.ori<-nrow(df);n.ori

descstat(data=df,y=c("edad","ytot","ytotcor"))
descstat(data=subset(df,edad>=18),y=c("edad","ytot","ytotcor"))
descstat(data=subset(df,activ=="Ocupados"),y=c("edad","ytot","ytotcor"))
descstat(data=subset(df,edad >=18 & ytotcor>0),y=c("edad","ytot","ytotcor"))
descstat(data=subset(df,edad >=18 & ytotcor>0&activ=="Ocupados"),y=c("edad","ytot","ytotcor"))

dfx<-subset(df,edad >=18 & ytotcor>0&activ=="Ocupados")
boxplot(dfx$ytotcor)
df.ori<-df

##- filtro, emplear solo los datos para adultos con ingresos
df<-subset(df.ori, edad >=18 & ytotcor>0&activ=="Ocupados")
dim(df)

conteo.comunas <- table(df$comuna);conteo.comunas

# al menos 10 observaciones
comunas.validas <- names(conteo.comunas[conteo.comunas >= 4])
comunas.validas

# filtrando los datos 
df <- subset(df, as.character(comuna) %in% comunas.validas)
dim(df)
str(df)
sum(table(df$comuna)) == nrow(df) # Tiene que coincidir


##? variable aleatoria de interes: la edad
df$vary<-df$ytotcor

##! grafico de distribucion
boxplot(df$vary)

hist(df$vary)

library(lattice)
histogram(~vary,data=df)


##* todo junto
histbxp(df$vary,varlab = "Ingreso total ($/mes)")

##! Estadistica descriptiva
descstat(data=df,y=c("vary"))
descstat(data=df,y=c("vary"),full = TRUE,eng=FALSE)


##! analizemos una variable categorica
table(df$sexo)
##creando una nueva variable
extractLeft("1. Hombre",1)
as.numeric(extractLeft("1. Hombre",1))

df$sex.code<-as.numeric(extractLeft(df$sexo,1))
table(df$sex.code)
head(df)
deleteLeft("1. Hombre",3)
deleteLeft("2. Mujer",3)
df$sexo<-deleteLeft(df$sexo,3)
table(df$sexo)

color.aca <- c("blue","red")
boxplot(vary ~sexo, data=subset(df,vary<3000000), col =color.aca, ylab="Ing. total ($/mes)")

descstat(data=df,y=c("vary"),factvar = "sexo")



##- ===================================
##! III. Ajuste de modelo 1 -- Factor como predictor
##- ===================================
##+ Primer modelo, solo el factor como predictor
#Identica sintaxis que para ajuste de modelos de regresion lineal
# para lo cual empleamos la funcion lm().
m1 <- lm(vary ~ sexo, data=df)
summary(m1)

##- Valor esperado del modelo ajustado

##! Guardando los valores ajustados
## guardando los coeficientes en un objeto
coef(m1)
b0.hat<-coef(m1)[1]
b1.hat<-coef(m1)[2]

##para el nivel "Mujer" del factor "sexo"
b0.hat+b1.hat*1
##y para el nivel "Hombre" del factor "sexo"
b0.hat+b1.hat*0


##+ Cuadro "analisis de varianza del modelo ajustado"
anova(m1)

##* Que tan bueno es este modelo
100*summary(m1)$sigma/mean(df$vary)

predstat(obs=df$vary,pre=fitted(m1),want.percent = T)

##- ===================================
##! IV. Ajuste de modelo 2 -- Una variable continua Factor como predictor
##- ===================================
##+ Primer modelo, solo la edad como predictor
m2 <- lm(vary ~ edad, data=df)
summary(m2)

##- Valor esperado del modelo ajustado
b0.hat2<-coef(m2)[1]
b1.hat2<-coef(m2)[2]

##para una edad de 30 anhos
b0.hat2+b1.hat2*30
##para una edad de 50 anhos
b0.hat2+b1.hat2*50

##* Que tan bueno es este modelo
predstat(obs=df$vary,pre=fitted(m2),want.percent = T)

##+ Cuadro "analisis de varianza del modelo ajustado"
anova(m2)



##- ===================================
##! IV. Ajuste de modelo 3 -- Una variable continua y factor como predictores
##- ===================================
##* Analicemos graficamente

require(lattice)
histogram(~vary|sexo, data=df)

xyplot(vary~edad| sexo, data=df)

xyplot(vary~edad,groups = sexo, data=df,auto.key = TRUE)


##otro grafico
col.list <- rep(0, length(df$sexo))
col.list[df$sexo=="Hombre"] <- "red"
col.list[df$sexo=="Mujer"] <- "blue"
plot(vary~edad, data=df, col=col.list, ylab="Ingreso ($/mes)",xlab="Age")
abline(m2, col = "green", lwd=4)
legend('bottomright',unique(df$sexo),col=unique(col.list),pch=1)

##+ Edad y el factor "Sexo" como predictores
m3 <- lm(vary ~ edad+sexo, data=df)
summary(m3)


##- Valor esperado del modelo ajustado
b0.hat3<-coef(m3)[1]
b1.hat3<-coef(m3)[2]
b2.hat3<-coef(m3)[3]

##? para una edad de 30 anhos
##  ..... para mujer?
b0.hat3+b1.hat3*30+b2.hat3*1
##  ..... para hombre?
b0.hat3+b1.hat3*30+b2.hat3*0

##? para una edad de 50 anhos
##  ..... para mujer?
b0.hat3+b1.hat3*50+b2.hat3*1
##  ..... para hombre?
b0.hat3+b1.hat3*50+b2.hat3*0



##* Que tan bueno es este modelo
predstat(obs=df$vary,pre=fitted(m3),want.percent = T)


##+ Cuadro "analisis de varianza del modelo ajustado"
anova(m3)


##! test de F-parcial
anova(m2,m3)

##+======================================================
##! Grafico de comportamiento para este modelo
##+======================================================
coef <- coefficients(m3)
plot(df$edad, df$vary, col=col.list, ylab="Ingreso ($/mes)", xlab="Edad (años)")
abline(coef["(Intercept)"] + coef["sexoMujer"],
coef["edad"], col = "red")
abline(coef["(Intercept)"],
coef["edad"], col = "blue")
legend('topright',unique(df$sexo),col=unique(col.list),pch=1)



##- ===================================
##! V. Ajuste de modelo 3b -- otra variante (varpred continua y factor)
##- ===================================
##+ Otra variante: Edad y el factor "Sexo" como predictores
m3b <- lm(vary ~ edad:sexo, data=df)
summary(m3b)


##- Valor esperado del modelo ajustado
b0.hat3b<-coef(m3b)[1]
b1.hat3b<-coef(m3b)[2]
b2.hat3b<-coef(m3b)[3]

##? para una edad de 30 anhos
##  ..... para mujer?
b0.hat3b+b1.hat3b*30*0+b2.hat3b*30*1
##  ..... para hombre?
b0.hat3b+b1.hat3b*30*1+b2.hat3b*30*0

##? para una edad de 50 anhos
##  ..... para mujer?
b0.hat3b+b1.hat3b*50*0+b2.hat3b*50*1
##  ..... para hombre?
b0.hat3b+b1.hat3b*50*1+b2.hat3b*50*0


##* Que tan bueno es este modelo
predstat(obs=df$vary,pre=fitted(m3b),want.percent = T)


##! test de F-parcial
anova(m2,m3b)


##+ Ultima variante: Edad y el factor "Sexo" como predictores
m3c <- lm(vary ~ edad*sexo, data=df)
summary(m3c)


##- Valor esperado del modelo ajustado
b0.hat3c<-coef(m3c)[1]
b1.hat3c<-coef(m3c)[2]
b2.hat3c<-coef(m3c)[3]
b3.hat3c<-coef(m3c)[4]

##? para una edad de 30 anhos
##  ..... para mujer?
b0.hat3c + b1.hat3c*30 + b2.hat3c*1 + b3.hat3c*30*1
##  ..... para hombre?
b0.hat3c + b1.hat3c*30 + b2.hat3c*0 + b3.hat3c*30*0

##? para una edad de 50 anhos
##  ..... para mujer?
b0.hat3c + b1.hat3c*50 + b2.hat3c*1 + b3.hat3c*50*1
##  ..... para hombre?
b0.hat3c + b1.hat3c*50 + b2.hat3c*0 + b3.hat3c*50*0

##* Que tan bueno es este modelo
predstat(obs=df$vary,pre=fitted(m3c),want.percent = T)

summary(m3c)
##! test de F-parcial
anova(m2,m3c)

coef <- coefficients(m3c)
plot(vary~edad, data=df, col=col.list, ylab="Ingreso ($/mes)", xlab="Edad (años)")
abline(coef["(Intercept)"] + coef["sexoMujer"],
coef["edad"]+coef["edad:sexoMujer"], col = "red",lwd=4)
abline(coef["(Intercept)"],
coef["edad"], col = "blue",lwd=2)
legend('topleft',unique(df$sexo),col=unique(col.list),pch=1)

#-╔═════════════════╗
#-║ Fin del script! ║
#-║ Atte.           ║
#-║ El profesor     ║
#-╚═════════════════╝
