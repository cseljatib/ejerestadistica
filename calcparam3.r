##!╔═════════════════════════════════════════════════════════════╗
##*║ Script academico                                            ║
##+║ Sobre:  Calcula parametro de una proporcion                 ║
##-║ Detalles: Calcula el parametro de la proporcion de elementos║
##-║  con ocurrencia de una variable aleatoria en la poblacion.  ║
##-║  Por ejemplo, si ocurre que una persona en la poblacion vote║
## ║  por un candidato en especifico.                            ║
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
##! I. Datos
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

##! Total de votos por candidato (incluyendo votos en blanco y nulos)
votos.opcion<-tapply(df$votos.tricel,df$candidato,sum)
votos.opcion

votos.opcion/1000000 #en millones de votos

##+ total de votos
vot.totales<-sum(votos.opcion)
vot.totales

vot.totales/1000000 #en millones de votos

##! Proporcion de votos por candidato (incluyendo votos en blanco y nulos)
prop.votos.opcion<-votos.opcion/vot.totales
prop.votos.opcion

##Si la variable aleatoria Y fuera si ocurre que una persona vota
## por el candidato 1, entonces el parametro "p(Y)" es igual a
prop.votos.opcion[1]

##Si la variable aleatoria Y fuera si ocurre que una persona vota
## por el candidato 2, entonces el parametro "p(Y)" es igual a
prop.votos.opcion[2]



##+====================================================
##? Tarea/bonus
##+====================================================
##-(1) Calcular participacion electoral
##primero: generar una data.frame con el numero de electores habilitados
## para votar por mesa
df.elec<-unique(df[,c("mesa.id","electores")])
dim(df.elec)
head(df.elec)
tail(df.elec)
N.pot<-sum(df.elec$electores)
N.pot

N.pot/1000000  ##en millones de personas

## proporcion de votantes efectivos del padron total (potencial)
vot.totales/N.pot
100*(vot.totales/N.pot)

##-(2) Comunas ordenadas segun cantidad de votos
comunas.orde.porvotos<-sort(tapply(df$votos.tricel, df$comuna, sum),decreasing = TRUE)
comunas.orde.porvotos[1:19]

hist(comunas.orde.porvotos, xlab = "Numero de votos sufragados (nivel comunal)")

miles.votos<-comunas.orde.porvotos/1000
hist(miles.votos, xlab = "Votos a nivel comunal (en miles de votos)")

histbxp(y=miles.votos,varlab = "Votos a nivel comunal (en miles de votos)")

descstat(miles.votos,full = TRUE, eng = FALSE)
##+====================================================

##-╔═════════════════╗
##-║ Fin del script! ║
##-║ Atte.           ║
##-║ El profesor     ║
##-╚═════════════════╝
