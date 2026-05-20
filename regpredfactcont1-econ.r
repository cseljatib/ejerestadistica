##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Modelo de regresion con dos variable predictoras: una  ║
## ║  categorica (o factor) y la otra continua.                    ║
##-║ Detalles:  La variable respuesta es continua.                 ║
##-║ Mas detalles:  Se ajustan modelos con variables predictoras:  ║
## ║ * (1) solo un factor; (2) solo una variable continua          ║
## ║ * (3) un factor y una variable continua                       ║
## ║ * Es fundamental aca demostrar el concepto de la variable     ║
## ║ dummy o indicadora.                                           ║
## ║                                                               ║
##*║ Ejemplo: Datos de ingreso total corregido segun               ║
##*║ encuesta (casen).                                             ║
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


##? Variable aleatoria de interes: la edad
df$vary<-df$ytotcor

##* Grafico de distribucion
boxplot(df$vary)

hist(df$vary)

library(lattice)
histogram(~vary,data=df)


##* Todo junto
histbxp(df$vary,varlab = "Ingreso total ($/mes)")

##* Estadistica descriptiva
descstat(data=df,y=c("vary"))
descstat(data=df,y=c("vary"),full = TRUE,eng=FALSE)


##* Analizar una variable categorica
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
##! II. Ajuste de modelo 1 -- Factor como predictor
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

##+ Calculo de estadisticos de prediccion
predstat(obs=df$vary,pre=fitted(m1),want.percent = T)


##- ===================================
##! III. Ajuste de modelo 2 -- Una variable continua como predictor
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
##! IV. Ajuste de modelo 3 -- dos variables predictoras:
## una continua y otro factor
##- ===================================
##* Analicemos graficamente

require(lattice)
histogram(~vary|sexo, data=df)

xyplot(vary~edad| sexo, data=df)

xyplot(vary~edad,groups = sexo, data=df,auto.key = TRUE)

##+ Edad y el factor "Sexo" como predictores
m3 <- lm(vary ~ edad+sexo, data=df)
## la sintaxis es la de un modelo de reg. lineal multiple
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
col.list <- rep(0, length(df$sexo))
col.list[df$sexo=="Hombre"] <- "red"
col.list[df$sexo=="Mujer"] <- "blue"

coef <- coefficients(m3)
plot(df$edad, df$vary, col=col.list, ylab="Ingreso ($/mes)", xlab="Edad (años)")
abline(coef["(Intercept)"] + coef["sexoMujer"],
coef["edad"], col = "red")
abline(coef["(Intercept)"],
coef["edad"], col = "blue")
legend('topright',unique(df$sexo),col=unique(col.list),pch=1)


##? ================================================================
## Tarea/preguntas:
## 1. Que le parecen los valores esperados para cada nivel del
## factor en relacion a la dispersion de los datos observados?.
##? ================================================================

#*╔══════════════════════╗
#*║ Estimad@ estudiante: ║
#*║ DisfRute el ejemplo! ║
#*║ El profesor     ╔════╝
#*╚═════════════════╝
