##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Modelo de regresion con dos variable predictoras: una  ║
##+║  categorica o factor, y la otra continua.                     ║
##-║ Detalles:  La variable respuesta es continua.                 ║
##-║ Mas detalles:  Se ajustan modelos con variables predictoras:  ║
## ║ * (1) solo un factor; (2) solo una variable continua          ║
## ║ * (3) un factor y una variable continua                       ║
## ║ * (4 y 5) dos variantes mas del modelo (3)                    ║
## ║ * Es fundamental aca demostrar que hay varias formas en que   ║
## ║ una variable factor pueden ser incorporadas en un modelo.     ║
## ║ * Ademas, se comparan los modelos anteriores con el mejor     ║
## ║ modelo de RLM previamente propuesto en un ejercicio anterior. ║
## ║                                                               ║
##*║ Ejemplo: Datos de la variable "peso" (biomasa) de osos en     ║
##*║ Norteamerica (bearscomp2)                                     ║
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

is.factor(df$sexo.nombre)
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

##* grafico de distribucion
boxplot(df$vary)

hist(df$vary)

library(lattice)
histogram(~vary,data=df)


##* todo junto
histbxp(df$vary,varlab = "Peso (Kg)")

##* Estadistica descriptiva
descstat(data=df,y=c("vary"))
descstat(data=df,y=c("vary"),full = TRUE,eng=FALSE)


##* analizando una variable categorica
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
##! III. Ajuste de modelo 2 -- Una variable continua como predictora
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

xyplot(vary~edad| sexo.nombre, data=df)

xyplot(vary~edad,groups = sexo.nombre, data=df,auto.key = TRUE)

##+ Edad y el factor "sexo.nombre" como predictores
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
col.list <- rep(0, length(df$sexo.nombre))
col.list[df$sexo.nombre=="Macho"] <- "red"
col.list[df$sexo.nombre=="Hembra"] <- "blue"

coef <- coefficients(m3)
plot(df$edad, df$vary, col=col.list, ylab="Peso (kg)", xlab="Edad (meses)")
abline(coef["(Intercept)"] + coef["sexo.nombreHembra"],
coef["edad"], col = "red")
abline(coef["(Intercept)"],
coef["edad"], col = "blue")
legend('topright',levels(df$sexo.nombre),col=col.list,pch=1)
## note que aca solo varian los interceptos entre los niveles del
##  factor.

##Note que en el modelo anteriormente ajustado, lo que cambia entre
## los niveles del factor es el intercepto.

##- =============================================================
##! V. Otras variantes empleando las mismas variables predictoras
## que el modelo anterior
##- =============================================================

##- ===================================
##! a) El modelo anterior (m3), es una variante del modelo que
## emplea un factor y una variable continua, en donde lo que difiere
## entre los niveles del factor es el intercepto del modelo con el
## cual se obtiene el valor esperado.
##- ===================================

##- ============================================================
##! b) Ajuste de modelo con pendientes diferentes entre niveles
## del factor
##- ============================================================
##+ recuerde, las variables predictoras son: Edad y el factor "Sexo.Nombre"
m3b <- lm(vary ~ edad:sexo.nombre, data=df)
summary(m3b)


##- Valor esperado del modelo ajustado
b0.hat3b<-coef(m3b)[1]
b1.hat3b<-coef(m3b)[2]
b2.hat3b<-coef(m3b)[3]

##? oso de 48 meses
##  ..... para Hembra?
b0.hat3b+b1.hat3b*48*0+b2.hat3b*48*1
##  ..... para Macho?
b0.hat3b+b1.hat3b*48*1+b2.hat3b*48*0


##? oso de 80 meses
##  ..... para Hembra?
b0.hat3b+b1.hat3b*80*0+b2.hat3b*80*1
##  ..... para Macho?
b0.hat3b+b1.hat3b*80*1+b2.hat3b*80*0


##* Que tan bueno es este modelo
predstat(obs=df$vary,pre=fitted(m3b),want.percent = T)


##! test de F-parcial (entre modelo 2 y 3b)
anova(m2,m3b)


##! Comparacion de SCE de ambos modelos con mismo numero de parametros
anova(m3,m3b)

##- =================================================================
##! b) Ajuste de modelo con interceptos y pendientes diferentes entre
## niveles del factor
##- =================================================================
##+ recuerde, las variables predictoras son: Edad y el factor "Sexo.Nombre"
m3c <- lm(vary ~ edad*sexo.nombre, data=df)
summary(m3c)


##- Valor esperado del modelo ajustado
b0.hat3c<-coef(m3c)[1]
b1.hat3c<-coef(m3c)[2]
b2.hat3c<-coef(m3c)[3]
b3.hat3c<-coef(m3c)[4]

##? un oso de 48 meses 
##  ..... para Hembra?
b0.hat3c + b1.hat3c*48 + b2.hat3c*1 + b3.hat3c*48*1
##  ..... para hombre?
b0.hat3c + b1.hat3c*48 + b2.hat3c*0 + b3.hat3c*48*0

##? un oso de 80 meses 
##  ..... para Hembra?
b0.hat3c + b1.hat3c*80 + b2.hat3c*1 + b3.hat3c*80*1
##  ..... para hombre?
b0.hat3c + b1.hat3c*80 + b2.hat3c*0 + b3.hat3c*80*0

##* Que tan bueno es este modelo
predstat(obs=df$vary,pre=fitted(m3c),want.percent = T)

summary(m3c)

##! test de F-parcial
anova(m2,m3b,m3c)

anova(m3,m3b)

##+======================================================
##! Grafico de comportamiento para modelo con interceptos y
## pendientes diferentes entre niveles del factor.
##+======================================================
coef <- coefficients(m3c)
plot(vary~edad, data=df, col=col.list, ylab="Peso (kg)", xlab="Edad (meses)")
abline(coef["(Intercept)"] + coef["sexo.nombreHembra"],
coef["edad"]+coef["edad:sexo.nombreHembra"], col = "red",lwd=4)
abline(coef["(Intercept)"],
coef["edad"], col = "blue",lwd=2)
legend('topleft',levels(df$sexo.nombre),col=col.list,pch=1)


##- =============================================================
##! VI. Nuestro "mejor" modelo de RLM hasta ahora, ver ejercicios
## anteriores, pero solo con variables predictoras continuas
##- =============================================================

m4 <- lm(vary~edad+pechoP, data=df)
summary(m4)

##* test de F-parcial (tiene sentido aca? ojo!!)
anova(m4,m3c)

##* Estadisticos predictivos del modelo
predstat(obs=df$vary,pre=fitted(m4),want.percent = T)

##- =========================================================
##! VII. Nuevo modelo: el "hasta ahora mejor" modelo de dos
## predictores continuos pero incluyendo al factor en estudio
##- ==========================================================

##+ Variante con interceptos distintos entre niveles del factor
m5 <- lm(vary~edad+pechoP+sexo.nombre, data=df)
summary(m5)

##* test de F-parcial
anova(m4,m5)

##* Estadisticos predictivos del modelo
predstat(obs=df$vary,pre=fitted(m5),want.percent = T)

##+ Variantes con pendientes distintos entre niveles del factor
##- (a) en la edad
m6a <- lm(vary~edad:sexo.nombre+pechoP, data=df)
summary(m6a)

##* test de F-parcial
anova(m4,m6a)

##* comparando la Suma cuadrado del error de variantes de modelos
## con un predictor factor
anova(m5,m6a)

##(b) en el perimetro del pecho
m6b <- lm(vary~edad+pechoP:sexo.nombre, data=df)
summary(m6b)

##* test de F-parcial
anova(m4,m6b)


##* Estadisticos predictivos de estos dos ultimos modelos
predstat(obs=df$vary,pre=fitted(m6a),want.percent = T)
predstat(obs=df$vary,pre=fitted(m6b),want.percent = T)

##* comparando la Suma cuadrado del error de variantes de modelos
## con un predictor factor
anova(m5,m6b)


##+ Variante con pendientes distintos entre niveles del factor
##-  en ambas variables predictoras
m7 <- lm(vary~edad:sexo.nombre+pechoP:sexo.nombre, data=df)
summary(m7)

##* test de F-parcial
anova(m4,m6a,m7)
anova(m4,m6b,m7)

##+ Variante con pendientes distintos entre niveles del factor
##-  en ambas variables predictoras
m8 <- lm(vary~sexo.nombre+edad:sexo.nombre+pechoP:sexo.nombre, data=df)
summary(m8)

##* test de F-parcial
anova(m4,m6a,m7,m8)
anova(m4,m6b,m7,m8)


##* Estadisticos predictivos de estos dos ultimos modelos
predstat(obs=df$vary,pre=fitted(m7),want.percent = T)
predstat(obs=df$vary,pre=fitted(m8),want.percent = T)


##? ================================================================
## Tarea/preguntas:
## 1. en que se parecen los modelos m3 y m3b?
## 2. en que se diferencian los modelos m3 y m3b, con respecto al
## modelo m3c?
## 3. en que se diferencian los modelos m6a y m6b, con respecto al
## modelo m5?
## 4. en que se diferencian los modelos m7 y m8, con respecto al
## modelo m6a?
## 5. Cual modelo selecciona como el mejor? Fundamente
##? ================================================================


#-╔═════════════════╗
#-║ Fin del script! ║
#-║ Atte.           ║
#-║ El profesor     ║
#-╚═════════════════╝
