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

###Ajuste de modelo con variable categorica
m1.b <- lm(peso ~ sexo.nombre, data=df)
summary(m1.b)

anova(m1.b)

###
bw.colint <- bearscomp2[,c('peso','edad','cabezaL','cabezaA','largo','pechoP')]
head(bw.colint)

pairs(bw.colint)

cor(bw.colint)

###estos modelos los ajustamos antes
m1 <- lm(peso~edad, data=df) #un slr
m2 <- lm(peso~edad+largo, data=df) 
m3 <- lm(peso~edad+largo+pechoP, data=df) 
m4 <- lm(peso~edad+pechoP, data=df) 
summary(m1)
summary(m2)
summary(m3)
summary(m4)

anova(m1,m2)#primero el modelo con mayor RSS
anova(m4,m3)#primero el modelo con mayor RSS


###comparemos el mejor modelo anterior, con el
## que usa solo un factor
m5 <- lm(peso~sexo.nombre, data=df) #un slr

anova(m5,m4)

m6 <- lm(peso~sexo.nombre+edad+pechoP, data=df) 
summary(m6)

anova(m6,m5)

###sobre como altera el valor esperado un factor como predictor
col.list <- rep(0, length(df$sexo.nombre))
col.list[df$sexo.nombre=="Macho"] <- "blue"
col.list[df$sexo.nombre=="Hembra"] <- "red"
plot(df$edad, df$peso, col=col.list, ylab="Peso (Kg)", xlab="Edad (yr)" )
abline(m1, col = "green", lwd=2)

legend("bottomright",levels(df$sexo.nombre),col=unique(col.list),pch=1)

###altera interceptos
m1.a <- lm(peso~edad+sexo, data=df) 
summary(m1.a)

###altera pendientes
m1.b <- lm(peso~sexo.nombre:edad, data=df) 
summary(m1.b)

###altera pendiente e intercepto
m1.c <- lm(peso~sexo.nombre*edad, data=df) 
summary(m1.c)

