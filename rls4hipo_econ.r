##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre:  Prueba de hipotesis en regresion lineal simple        ║
##-║ Detalles:  Valores ajustados y residuales                     ║
##-║ Mas detalles: Ajusta modelo y revisa test de hipotesis        ║
## ║+ Emplea diferentes nivels de significancia                    ║
## ║+ Calcula intervalos confidenciales                            ║
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
hist(df$poverty)

boxplot(df$socspend)
hist(df$socspend)

##-Dispersion
plot(poverty ~ socspend, data=df)

##+================================================
##! III. Ajuste del modelo
##+================================================
mod1<- lm(poverty~socspend,data=df)
summary(mod1)

##-Guardando los coeficientes en un objeto
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
b0.hat
b1.hat

##+================================================
##! IV. Pruebas de hipotesis 
##+================================================
##-1. revisar el valor de t para la variable predictora
summary(mod1)


##a) primero revisar el error estandar del estimador de beta_1
se.b1.hat<-summary(mod1)$coef[,"Std. Error"][1]
se.b1.hat

##b) segundo, calcular este cuociente
b1.hat/se.b1.hat

##c) comparelo con el valor calculado para el test estadistico de t 
summary(mod1)$coef[,"t value"]

##d) que puede concluir?

##-2. Int.Conf. coef.estimados
coef(mod1)
confint(mod1, level=0.95) ##> fijando el nivel de confianza
confint(mod1, level=0.90) 
confint(mod1, level=0.99)


##-╔══════════════════════╗
##-║ Estimad@ estudiante: ║
##-║ DisfRute estadistica ║
##-║ El profesor     ╔════╝
##-╚═════════════════╝
