##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Modelo de regresion con dos variable predictoras: una  ║
## ║  categorica o factor, y la otra continua.                     ║
##-║ Detalles:  La variable respuesta es continua.                 ║
##-║ Mas detalles:  Ajuste del modelo con variable dummy como      ║
## ║  predictor.                                                   ║
## ║                                                               ║
##*║ Ejemplo: Datos del peso de osos (bearscomp2)                  ║
##*║                                                               ║
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
df <- datana::bearscomp2
head(df)

tapply(df$peso,df$sexo,length)

is.factor(df$sexo.name)
df$sexo.nombre<-as.factor(df$sexo.nombre)
means.g <- tapply(df$peso,df$sexo.nombre,mean)
means.g

sds.g <- tapply(df$peso,df$sexo.nombre,sd)
sds.g

ns.g <- tapply(df$peso,df$sexo.nombre,length)
ns.g

boxplot(df$peso ~df$sexo.nombre, ylab="Peso (Kg)",
xlab="Sexo")

descstat(data=df,y=c("peso"),factvar = "sexo.nombre")

##? variable aleatoria de interes: la edad
df$vary<-df$peso

##! grafico de distribucion
boxplot(df$vary)

hist(df$vary)

library(lattice)
histogram(~vary,data=df)


##* todo junto
histbxp(df$vary,varlab = "Peso (Kg)")

##! Estadistica descriptiva
descstat(data=df,y=c("vary"))
descstat(data=df,y=c("vary"),full = TRUE,eng=FALSE)


##! analizemos una variable categorica
table(df$sexo.nombre)
##creando una nueva variable

color.aca <- c("blue","red")
boxplot(vary ~sexo, data=df, col =color.aca, ylab="Peso (kg)")

descstat(data=df,y=c("vary"),factvar = "sexo.nombre")



##- ===================================
##! II. Ajuste de modelo 1 -- Factor como predictor
##- ===================================
##+ Primer modelo, solo el factor como predictor
#Identica sintaxis que para ajuste de modelos de regresion lineal
# para lo cual empleamos la funcion lm().
m1 <- lm(vary ~ sexo.nombre, data=df)
summary(m1)

##- Valor esperado del modelo ajustado

##! Guardando los valores ajustados
## guardando los coeficientes en un objeto
coef(m1)
b0.hat<-coef(m1)[1]
b1.hat<-coef(m1)[2]

##para el nivel "Hembra" del factor "sexo.nombre"
b0.hat+b1.hat*1
##y para el nivel "Macho" del factor "sexo.nombre"
b0.hat+b1.hat*0


##+ Cuadro "analisis de varianza del modelo ajustado"
anova(m1)

##* Que tan bueno es este modelo
100*summary(m1)$sigma/mean(df$peso)

##+ Calculo de estadisticos de prediccion
predstat(obs=df$peso,pre=fitted(m1),want.percent = T)


##- ===================================
##! IV. Ajuste de modelo 2 -- Una variable continua como predictor
##- ===================================
##+ Primer modelo, solo la edad como predictor
plot(vary ~ edad, data=df)
m2 <- lm(vary ~ edad, data=df)
summary(m2)

##- Valor esperado del modelo ajustado
b0.hat2<-coef(m2)[1]
b1.hat2<-coef(m2)[2]

##para una edad de 48 meses
b0.hat2+b1.hat2*48
##para una edad de 50 meses
b0.hat2+b1.hat2*50

##* Que tan bueno es este modelo
predstat(obs=df$vary,pre=fitted(m2),want.percent = T)

##+ Cuadro "analisis de varianza del modelo ajustado"
anova(m2)

##- ===================================
##! IV. Ajuste de modelo 3 -- dos variables predictoras:
## una continua y otro factor
##- ===================================
##* Analicemos graficamente

require(lattice)
histogram(~vary|sexo.nombre, data=df)

xyplot(vary~edad| sexo, data=df)

xyplot(vary~edad,groups = sexo.nombre, data=df,auto.key = TRUE)


##+ Edad y el factor "Sexo" como predictores
m3 <- lm(vary ~ edad+sexo.nombre, data=df)
## la sintaxis es la de un modelo de reg. lineal multiple
summary(m3)


##- Valor esperado del modelo ajustado
b0.hat3<-coef(m3)[1]
b1.hat3<-coef(m3)[2]
b2.hat3<-coef(m3)[3]

##? un oso de 48 meses
##  ..... para hembra?
b0.hat3+b1.hat3*48+b2.hat3*1
##  ..... para macho?
b0.hat3+b1.hat3*48+b2.hat3*0

##? un oso de 50 meses
##  ..... para hembra?
b0.hat3+b1.hat3*50+b2.hat3*1
##  ..... para macho?
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
plot(df$edad, df$vary, ylab="Peso (kg)", xlab="Edad (años)")
abline(coef["(Intercept)"] + coef["sexo.nombreHembra"],
coef["edad"], col = "red")
abline(coef["(Intercept)"],
       coef["edad"], col = "blue")
legend('topleft',legend = unique(df$sexo.nombre),col=c("blue", "red"),pch=1)



##- ===================================
##! V. Ajuste de modelo 3b -- otra variante (varpred continua y factor)
##- ===================================
##+ Otra variante: Edad y el factor "Sexo" como predictores
m3b <- lm(vary ~ edad:sexo.nombre, data=df)
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
m3c <- lm(vary ~ edad*sexo.nombre, data=df)
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
plot(vary~edad, data=df, ylab="Peso (kg)", xlab="Edad (años)")
abline(coef["(Intercept)"] + coef["sexo.nombreHembra"],
coef["edad"]+coef["edad:sexo.nombreHembra"], col = "red",lwd=4)
abline(coef["(Intercept)"],
coef["edad"], col = "blue",lwd=2)
legend('topleft',legend = unique(df$sexo.nombre),col=c("blue", "red"),pch=1)

#-╔═════════════════╗
#-║ Fin del script! ║
#-║ Atte.           ║
#-║ El profesor     ║
#-╚═════════════════╝
