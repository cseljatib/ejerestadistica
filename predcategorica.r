
library(datana)
df <- datana::bearsdepu2
head(df)

tapply(df$peso,df$sexo,length)
df[df$sexo==1,"sexo.name"]="Macho"
df[df$sexo==2,"sexo.name"]="Hembra"

is.factor(df$sexo.name)
df$sexo.name<-as.factor(df$sexo.name)
means.g <- tapply(df$peso,df$sexo.name,mean)
means.g

sds.g <- tapply(df$peso,df$sexo.name,sd)
sds.g

ns.g <- tapply(df$peso,df$sexo.name,length)
ns.g

boxplot(df$peso ~df$sexo.name, ylab="Peso (Kg)",
xlab="Sexo")

###Ajuste de modelo con variable categorica
m1.b <- lm(peso ~ sexo.name, data=df)
summary(m1.b)

anova(m1.b)

###
bw.colint <- bearsdepu2[,c('peso','edad','cabezaL','cabezaA','largo','pechoP')]
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
m5 <- lm(peso~sexo.name, data=df) #un slr

anova(m5,m4)

m6 <- lm(peso~sexo.name+edad+pechoP, data=df) 
summary(m6)

anova(m6,m5)

###sobre como altera el valor esperado un factor como predictor
col.list <- rep(0, length(df$sexo.name))
col.list[df$sexo.name=="Macho"] <- "blue"
col.list[df$sexo.name=="Hembra"] <- "red"
plot(df$edad, df$peso, col=col.list, ylab="Peso (Kg)", xlab="Edad (yr)" )
abline(m1, col = "green", lwd=2)

legend("bottomright",levels(df$sexo.name),col=unique(col.list),pch=1)

###altera interceptos
m1.a <- lm(peso~edad+sexo, data=df) 
summary(m1.a)

###altera pendientes
m1.b <- lm(peso~sexo.name:edad, data=df) 
summary(m1.b)

###altera pendiente e intercepto
m1.c <- lm(peso~sexo.name*edad, data=df) 
summary(m1.c)

