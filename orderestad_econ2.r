##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre:  Calculo de estadisticos de orden                      ║
##-║ Detalles: Cuartiles, quintiles y deciles.                     ║
##!║ Ejemplo: Datos de ingreso provenientes de la encuesta CASEN.  ║
##-║-----------------------------------------------------------    ║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝


##+%%%%%%%%%%%%%%%%
##! I. Poblacion
##+%%%%%%%%%%%%%%%%
library(datana)
head(casen)
##- revise la metadata al activar la siguiente linea
#?casen
df<-casen
dim(df)
n.ori<-nrow(df)


descstat(data=df,y=c("edad","ytot","ytotcor"))
table(df$activ)

##! filtros impuestos
mayoria.edad<-18 #anhos
ing.total.min<-0 #debe ser mayor a este valor
acti.filtro<-"Ocupados"

##- aplicando las condiciones para esta sub-poblacion 
df<-subset(df, edad >=18 & ytotcor>0&activ=="Ocupados")
##- verificando las nuevas dimensiones
dim(df)

##- Se asumira que estos datos seran nuestra poblacion
N<-nrow(df)
N

names(df)

##! Estadistica descriptiva
descstat(data=df,y=c("edad","ypc","ytotcor"))


##*variable aleatoria de interes: ingreso total corregido
##! Distribucion
hist(df$ytotcor)
boxplot(df$ytotcor)


##%%%%%%%%%%%%%%%%
##> II. Estadisticos de orden
##%%%%%%%%%%%%%%%%
##! los cuartiles
descstat(data=df,y=c("ytotcor"),full=TRUE,eng=FALSE)
## ahi aparecen como percentil 25, mediana y percentil 75
##* tambien lo puede obtener mediante 
cuartiles<-quantile(df$ytotcor)
cuartiles

##* entonces, el primer cuartil es 
cuartiles[2]
##* el segundo cuartil es 
cuartiles[3]
##* y el tercer cuartil es 
cuartiles[4]
##- note que los valores extremos de los resultados son el valor
##  minimo y maximo respectivamente

##! Los quintiles
quintiles<-quantile(df$ytotcor, probs = seq(0.2,.8,by=0.2))
quintiles
##* entonces, el primer quintil es 
quintiles[1]
##* el segundo quintil es 
quintiles[2]
##* el tercero quintil es 
quintiles[3]
##* y el quinto quintil es 
quintiles[4]


##! Los deciles
deciles<-quantile(df$ytotcor, probs = seq(0.1,.9,by=0.1))
deciles
##* entonces, el primer decil es 
deciles[1]
##* el segundo decil es 
deciles[2]
##* y asi sucesivamente
##* y el noveno decil es 
deciles[9]

##- note que el octavo decil debe ser igual al cuarto quintil
