##!╔═════════════════════════════════════════════════════════════╗
##*║ Script academico                                            ║
##+║ Sobre:  Calcula parametro del total                         ║
##-║ Detalles: Calcula el parametro del total de una variable    ║
##-║  aleatoria en una poblacion.                                ║
## ║                                                             ║
##!║ Ejemplo: Datos de eleccion politica, presidenciales en      ║
## ║  Chile (president).                                         ║
##-║-----------------------------------------------------------  ║
## ║                                                             ║
##>║ Profesor: Christian Salas Eljatib                           ║
##+║ E-mail: christian.salas AT uchile DOT cl                    ║
##*║ Web: https://eljatib.com                                    ║
##!╚═════════════════════════════════════════════════════════════╝

##+%%%%%%%%%%%%%%%%
##! I. Poblacion
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

df0<-df
names(df0)
df0$candidato<-as.factor(df0$candidato)
levels(df0$candidato)
##* cambiar nombre de los niveles del factor
etiqueta.opcion.voto<-c("Boric","Kast","Blanco","Nulo")
etiqueta.opcion.voto
levels(df0$candidato)<-etiqueta.opcion.voto
levels(df0$candidato)

##+ generando los votos por candidato como columnas
df <- reshape(df0[,c("region","comuna","electores","mesa.id.num","candidato","votos.tricel")], 
  idvar = "mesa.id.num",# define las filas
  timevar = "candidato", #define nuevas columnas
  v.names = "votos.tricel", #variable que rellenara los valores
     direction = "wide")
dim(df)
head(df)
names(df) <- gsub("votos.tricel\\.", "", names(df))

df.pais<-df

##+ %%%%%%%%%%%%%%%%%%%%
##! II. La poblacion 
##+ %%%%%%%%%%%%%%%%%%%%
##- Emplear todos los datos (recuerde, no considera votos en el extranjero)
df<-df.pais

##- verificando
## dimensiones
dim(df)
## listado de mesas
listado.mesas<-unique(df$mesa.id)
## numero total de mesas receptoras de sufragio es
num.mesas<-length(listado.mesas);num.mesas

head(df)
##! Total de votos por candidato y/o tipo de voto
vot.opcion<-colSums(df[,etiqueta.opcion.voto],na.rm = TRUE)
vot.opcion

##* Si la variable aleatoria "Y" fuera el numero de votos que obtuvo
##* el candidato 1 en una mesa, entonces el parametro "tau(Y)" es igual a
vot.opcion[1]

vot.opcion[1]/1000000 ##en millones de votos

##* Si la variable aleatoria "Y" fuera el numero de votos que obtuvo
##* el candidato 2 en una mesa, entonces el parametro "tau(Y)" es igual a
vot.opcion[2]

vot.opcion[2]/1000000 ##en millones de votos

##+====================================================
##? Tarea/bonus
##+====================================================
##-(1) Proporcion de votos por opcion
##primero: definir votos totales sufragados
N<-vot.totales<-sum(vot.opcion)
N  ##>ojo: esto no considera los votos emitidos en el extranjero

##segundo: la proporcion por candidato y/o tipo de voto
prop.votos.opcion<-vot.opcion/N
prop.votos.opcion

##y en porcentaje
100*prop.votos.opcion

##+====================================================

##-╔═════════════════╗
##-║ Fin del script! ║
##-║ Atte.           ║
##-║ El profesor     ║
##-╚═════════════════╝
