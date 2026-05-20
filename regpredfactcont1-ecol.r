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

