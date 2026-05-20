##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre: Modelo de regresion con variable predictora categorica ║
## ║ o factor.                                                     ║
##-║ Detalles:  La variable respuesta es continua.                 ║
##-║ Mas detalles:  Ajuste del modelo con variable dummy como      ║
## ║  predictor.                                                   ║
## ║                                                               ║
##*║ Ejemplo: Datos de variables de crecimiento de                 ║
## ║ osos (bearscomp2).                                            ║
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

##- ===================================
##! II. Ajuste con un Factor como variable predictora
##- ===================================
#Identica sintaxis que para ajuste de modelos de regresion lineal
# para lo cual empleamos la funcion lm().
m1 <- lm(peso ~ sexo.nombre, data=df)
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
