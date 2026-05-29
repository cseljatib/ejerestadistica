##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script academico                                              ║
##+║ Sobre:  Intervalo de confianza para muestreo aleatorio simple ║
##-║ Detalles: Realiza un muestreo aleatorio simple sobre una      ║
##-║  poblacion para calcular estadisticos e intervalo de confianza║
## ║  estadistico del estimador del parametro de la proporcion de  ║
## ║  votantes en una eleccion politica                            ║
## ║                                                               ║
##!║ Ejemplo: Datos de eleccion politica, presidenciales en        ║
## ║  Chile (president).                                           ║
##-║-----------------------------------------------------------    ║
## ║                                                               ║
##>║ Profesor: Christian Salas Eljatib                             ║
##+║ E-mail: christian.salas AT uchile DOT cl                      ║
##*║ Web: https://eljatib.com                                      ║
##!╚═══════════════════════════════════════════════════════════════╝

##+%%%%%%%%%%%%%%%%
##> I. Datos
##+%%%%%%%%%%%%%%%%
library(datana)
head(president)
##- revise la metadata al activar la siguiente linea
#?president
df<-president
dim(df)
unique(df$candidato)
df<-subset(df, candidato!="TOTALES")
dim(df)
head(df)
##? Ojo: estos datos no consideran los votos emitidos en el extranjero
table(df$region)

##*estableciendo un codigo unico de mesa receptora de sufragio
unique(df$candidato)
unique(df$circu.elec)
length(unique(df$mesas.fusionadas))

df$mesa.id<-paste(df$circu.elec,df$local,df$no.mesa,df$mesas.fusionadas,sep="-")
head(df)
listado.mesas.tot<-unique(df$mesa.id)
##-  el numero total de mesas receptoras de sufragio es
num.mesas.tot<-length(listado.mesas.tot);num.mesas.tot

##*estableciendo un codigo-numerico unico de mesa receptora de sufragio
df$mesa.id.num <- as.numeric(factor(df$mesa.id, levels = listado.mesas.tot))

## revisando la codificacion
head(df[, c("mesa.id", "mesa.id.num","candidato")],8)
tail(df[, c("mesa.id", "mesa.id.num","candidato")],8)

df.pais<-df

##+ %%%%%%%%%%%%%%%%%%%%
##> II. La poblacion 
##+ %%%%%%%%%%%%%%%%%%%%
##- filtro, emplear solo los datos de la comuna de Maipu
df<-subset(df.pais, comuna=="MAIPU")

##- verificando
## dimensiones
dim(df)
## listado de mesas
listado.mesas<-unique(df$mesa.id)
## numero total de mesas receptoras de sufragio es
num.mesas<-length(listado.mesas);num.mesas

num.mesas.tot ##a nivel pais
100*num.mesas/num.mesas.tot

head(df[, c("mesa.id", "mesa.id.num","candidato","votos.tricel")],8)
tail(df[, c("mesa.id", "mesa.id.num","candidato","votos.tricel")],8)

##+ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##> III. Parametro de interes: proporcion de votos 
##+ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##! Total de votos por candidato (incluyendo votos en blanco y nulos)
votos.opcion<-tapply(df$votos.tricel,df$candidato,sum)
votos.opcion

votos.opcion/1000000 #en millones de votos

##+ total de votos
vot.totales<-sum(votos.opcion)

vot.totales/1000000 #en millones de votos

##! Proporcion de votos por candidato (incluyendo votos en blanco y nulos)
prop.votos.opcion<-votos.opcion/vot.totales
prop.votos.opcion

##Si la variable aleatoria Y fuera si ocurre que una persona vota
## por el candidato 1, entonces el parametro "p(Y)" es igual a
prop.votos.opcion[1]

param.p<-prop.votos.opcion[1];param.p
##Si la variable aleatoria Y fuera si ocurre que una persona vota
## por el candidato 2, entonces el parametro "p(Y)" es igual a
prop.votos.opcion[2]

##%%%%%%%%%%%%%%%%
##> IV. Estrategia de muestreo
##%%%%%%%%%%%%%%%%
##! Tamanho muestral (elemento muestral: mesa)
num.mesas  # a nivel de esta poblacion (comuna definida anteriormente)
N<- num.mesas
n<- 10

100*n/N
## Seleccion de elementos bajo muestreo aleatorio simple.
1:10 ##secuencia de numeros
#como se emplea la funcion sample()?
sample(1:10,3)

#set.seed(1234)
#* seleccionemos los elementos
pos.elem.muestra <- sample(1:N,size=n,replace=FALSE)
pos.elem.muestra

elem.muestra<-listado.mesas[pos.elem.muestra]
elem.muestra
##dichos elementos corresponden a las siguientes valores de variables
mi.muestra <- subset(df, mesa.id %in% elem.muestra)
dim(mi.muestra)
head(mi.muestra)

##- verificando
## listado de mesas seleccionadas
unique(mi.muestra$mesa.id)
##comparar lo siguiente
cbind(sort(unique(mi.muestra$mesa.id)),sort(elem.muestra))
## numero de mesas receptoras de sufragio en la muestra
num.mesas.m<-length(unique(mi.muestra$mesa.id));num.mesas.m


##+%%%%%%%%%%%%%%%%
##! III. Calculo de estadisticos
##+%%%%%%%%%%%%%%%%

votos.opcion.m<-tapply(mi.muestra$votos.tricel,mi.muestra$candidato,sum)
votos.opcion.m

##+ total de votos (en la muestra)
vot.totales.m<-sum(votos.opcion.m)
vot.totales.m

##! Proporcion de votos por candidato (incluyendo votos en blanco y nulos)
prop.votos<-votos.opcion.m/vot.totales.m
prop.votos

##Si la variable aleatoria Y fuera si ocurre que una persona vota
## por el candidato 1, entonces el estimador del parametro "p(Y)" es igual a
prop.votos[1]

param.p.hat<-prop.votos[1]
#comparelo con 
param.p


##-------------
##!2) Varianza muestral
#tambien se puede calcular como
v.y<-param.p.hat*(1-param.p.hat)
v.y


##-------------
##!3) Desviacion estandar
sqrt(v.y)


##-------------
##!5) Error estandar del estimador (i.e., media aritmetica)
#ES(media.y)
##fraccion de muestreo
f<-n/N
f
(1-f)


se.phat<-sqrt( (v.y/n) * (1-f) )
se.phat
##!Error estandar del estimador, en %
100*se.phat/param.p.hat

##-------------
##!6) Error o margen de muestreo, para un nivel de significancia
## estadistico de alpha

##+(a) nivel de confianza/significancia
conf <- 95 #en %
alpha <- 1 - (conf/100)
alpha

##+(b) valor de t
df <- n - 1
alpha.2 <- alpha/2
t.value <- abs(qt(1 - alpha.2,df))
t.value #valor de la dist. de t-student a emplear

##+(c) error, o margen de muestreo
se.phat*t.value
e.m <-se.phat*t.value

##+Error de muestreo en %
100*e.m/param.p.hat

##!7) Intervalo de confianza, para un nivel de significancia
## estadistico de alpha
lim.inf <- param.p.hat-e.m
lim.inf
lim.sup <- param.p.hat+e.m
c(lim.inf,lim.sup)

message("Aca termina el script!")
#+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#+´¨)
#+¸.•´¸.•*´¨) ¸.•*¨)
#+(¸.•´ (¸.•` ¤ Fin del script
