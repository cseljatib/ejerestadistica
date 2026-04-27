##!╔═══════════════════════════════════════════════════════════════╗
##+║ Sobre:  Ajuste modelo de regresion lineal simple (RLS)        ║
##-║ Detalles:  Emplea estimador de minimos cuadrados.             ║
##-║ Mas detalles: Entre otras cosas, en este ejercicio se:        ║
## ║+ revisan los parametros estimados del modelo.                 ║
## ║+ usa el modelo para predecir dado un valor para la variable   ║
## ║predictora                                                     ║
## ║                                                               ║
##*║ Ejemplo: Datos de gasto social a nivel pais (socioecon).      ║
## ║Relacion entre pobreza y gasto social                          ║
##-║---------------------------------------------------------------║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝



##+================================================
##! I. Datos
##+================================================
library(datana)
data(socioecon)
##?socioecon #ejecutelo en la consola
##? revise en la metadata cual es la definicion para "poverty"
##? revise en la metadata cual es la definicion para "socspend"
df <- socioecon

head(df)
dim(df)
str(df)

##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("poverty","socspend")])

##+================================================
##! II. Graficos de interes
##+================================================
##-Distribucion
boxplot(df$poverty)
boxplot(df$poverty,las=1,ylab = "Pobreza (% de la población)")
hist(df$poverty)

boxplot(df$socspend)
hist(df$socspend,las=1,xlab = "Gasto social (% del PIB)")

##-Dispersion
plot(poverty ~ socspend, data=df)
plot(poverty ~ socspend, data=df,las=1,ylab = "Pobreza (% de la población)",
     xlab = "Gasto social (% del PIB)")
plot(poverty ~ socspend, data=df,las=1,ylab = "Pobreza (% de la población)",
     xlab = "Gasto social (% del PIB)",xlim=c(0,max(df$socspend)),
     ylim = c(0,20))

##+================================================
##! III. Ajuste del modelo
##+================================================
mod1<- lm(poverty~socspend,data=df)
summary(mod1)


##! Parametros estimados
##*1. Los coeficientes
coef(mod1)

##*2. La varianza del error
## su raiz cuadrada, es decir, error estandar
(summary(mod1))$sigma
## y la varianza
(summary(mod1))$sigma^2


##+================================================
##! IV. Guardando algunos objetos de interes
##+================================================

##*1. Los coeficientes
##-Guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]

b0.hat
b1.hat

##*2. El error estandar de los residuales
##- RMSE del modelo
(summary(mod1))$sigma 
sigma.e<-(summary(mod1))$sigma 
sigma.e


##+================================================
##! V. Usando el modelo ajustado para estimar el valor de
## la variable respuesta dado un valor de la variable predictora (X).
##+================================================
##? si la variable predictora fuera de 20, i.e., 20 % del presupuesto total, cual seria  
## el valor estimado de la variable respuesta (i.e., poverty) segun
## el modelo ajustado?

##- ya tenemos los objetos necesarios guardados, y son numeros, e.g.
b0.hat*0.5
b1.hat*1.5
##- por lo tanto solo debemos reemplazar en el modelo los valores
## estimados para los coeficientes, por lo tanto 
b0.hat+b1.hat*20

b0.hat+b1.hat*21

b0.hat+b1.hat*22

##-╔══════════════════════╗
##-║ Estimad@ estudiante: ║
##-║ DisfRute estadistica ║
##-║ El profesor     ╔════╝
##-╚═════════════════╝
