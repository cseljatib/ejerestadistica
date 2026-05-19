##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Modelo de regresion con variable predictora categorica ║
## ║ o factor.                                                     ║
##-║ Detalles:  La variable respuesta es continua.                 ║
##-║ Mas detalles:  Ajuste del modelo con variable dummy como      ║
## ║  predictor.                                                   ║
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
##! II. Ajuste con un Factor como variable predictora
##- ===================================
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

##? ===============
## 1. Que le parece el modelo?
## 2. en comparacion a otros modelos vistos, como considera usted las
## capacidades predictivas del modelo ajustado
##- -----------------------------------------------------------

##+ ========================================
## Bonus
##? como luce matricialmente el modelo ajustado?
model.matrix(m1) # valores de variables dummy


#-╔═════════════════╗
#-║ Fin del script! ║
#-║ Atte.           ║
#-║ El profesor     ║
#-╚═════════════════╝
