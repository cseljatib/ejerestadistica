##!╔═══════════════════════════════════════════════════════════════╗
##*║ Script: "calcparam3.r"                                        ║
##+║ Sobre:  Calcula parametros-3                                  ║
##-║ Detalles: Calcula el parametro del total de una variable      ║
##-║  aleatoria en una poblacion.                                  ║
## ║                                                               ║
## ║                                                               ║
##!║ Ejemplo: Datos de eleccion politica.                          ║
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
head(president)
##- revise la metadata al activar la siguiente linea
#?president
df<-president
dim(df)
head(df)
table(df$region)

##- filtro, emplear solo los datos de la Region de Coquimbo
df<-subset(df, region=="DE COQUIMBO")

##- verificando las nuevas dimensiones
dim(df)

##- Se asumira que estos datos seran nuestra poblacion

##* establezcamos un numero unico de mesa receptora de sufragio
unique(df$candidato)
unique(df$circu.elec)
df$mesa.id<-paste(df$circu.elec,df$local,df$no.mesa,df$mesas.fusionadas,sep="-")
listado.mesas<-unique(df$mesa.id)
##-  el numero total de mesas receptoras de sufragio es
num.mesas<-length(listado.mesas);num.mesas

##? esto es para verificar, que solo cuatro opciones maximo debe
## existir por mesa receptora
table(df$region,df$mesa.id)

##generando un set de datos con el numero de electores habilitados
## para votar por mesa
df.elec<-unique(df[,c("mesa.id","electores")])
head(df.elec)
sum(df.elec$electores)
N<-sum(df.elec$electores)
N


##! Total de votos por candidato (incluyendo votos en blanco y nulos)
votos.opcion<-tapply(df$votos.tricel,df$candidato,sum)
votos.opcion
    
##+ total de votos
vot.totales<-sum(votos.opcion)
vot.totales

## proporcion de votos, del total
100*(vot.totales)/N

##! Razon de votos por candidato (incluyendo votos en blanco y nulos)
razon.votos.opcion<-votos.opcion/N
razon.votos.opcion


message("Fin del script!")
#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
