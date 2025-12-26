##! Script: "ajumle1.r"                                            /
##- Sobre:  Ajuste de una funcion de masa de probabilidades (fmp) /
##+ Detalles: Emplea estimador analitico de maxima verosimilitud./
##  Ejemplo: Ajuste de funcion de Poisson para datos de         /
##    productividad de papers de estudiantes postgrado.        /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/


##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# I. Sobre la funcion de Poisson
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##(a) Matematicamente se escribe como
##pr.y= (e^lambda lambda^y)/y!
#repasemos algunas funciones matematicas
exp(1)
factorial(3)
#ahora escribamos la funcion, para Y=3 y lamba=1 
exp(-1)*(1^3)/factorial(3)
#podemos guardar los valores de Y y lambda en objetos
y<-3
mi.lambda<-1
#y aplicar a la expresion matematica
exp(-mi.lambda)*(mi.lambda^y)/factorial(y)
#este resultado obtenido es una probabilidad

##(b) Ocupemos una funcion ya escrita en R, que
## calcula la misma expresion matematica que escribimos antes.
#La funcion se llama "dpois()", revise ayuda sobre ella con
?dpois
#ahora ocupemos la funcion
dpois(y,lambda = mi.lambda)
# y comparemos con el resultado anterior.
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# II. Graficando la funcion
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Demosle valores tentativos a la variable Y
y<-0:9
#revise el objeto creado "y"
y
#calculemos el valor de la funcion para cada valor
# de la variable Y, y guardemos los resultados en
# un objeto que llamaremos "pr.y"
pr.y<-dpois(y,lambda = mi.lambda)
#revise el objeto creado "pr.y"
pr.y
#ahora grafiquemos, mediante la funcion "plot()"
plot(pr.y~y)
#mejoremos el grafico anterior con
plot(pr.y~y, ylab = "Pr(Y=y)",type="b", las=1)
#compare ambos graficos, y preguntes: Que cambio?
#Dado que la variable aleatoria es discreta, el grafico debiera mejor mediante
plot(pr.y~y, ylab = "Pr(Y=y)",type="h", las=1)
#compare ambos graficos, y vea que cambio. 
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# III. Datos para ejemplo
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
head(papersdocstu)
#Activar siguiente linea para ver metadata
#?papersdocstu
df <- papersdocstu

head(df)
dim(df)

df$y<-df$papers
#Grafico de distribucion
hist(df$y)
table(df$y)
barplot(table(df$y),xlab="Numero de papers publicados",
        ylab="Frecuencia (num. de estudiantes)")
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# IV. Ajuste de la funcion 
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#segun lo visto en clases, podemos derivar una solucion analitica
# para el estimador de maxima verosimilud para la funcion de Poisson.
#Hablar de solucion analitica, implica que existe una "formula" que
# nos entrega el resultado. En este caso es la media aritmetica
mean(df$y)
#y ahora el estimador MLE del parametro lambda lo guardamos en el objeto
lambda.mle<-mean(df$y)
lambda.mle
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# V. Grafico de la funcion ajustada
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- (a) Representemos los datos en terminos de frecuencia relativa
table(df$y)
y.tab<-table(df$y)
y.tab.rel<-y.tab/nrow(df)
#la frecuencia relativa para cada valor de Y observado es
y.tab.rel
#y en porcentaje
100*y.tab.rel
##- (b) Grafico de las frecuencias relativas observadas
plot(y.tab.rel,las=1,ylab="Probabilidad",xlab="Variable aleatoria",
 ylim=c(0,0.35))
##- (c) Probabilidades estimadas, segun nuestro modelo ajustado
y.list<-0:19
probesti.poi<-dpois(y.list,lambda = lambda.mle)
##- (d) Superponer las probabilidades estimadas 
lines(y.list,probesti.poi, type="b",col="red")
##- (e) Agreguemos una leyenda
legend("topright",c("Observada","Poisson"),
         col=c("black","red"),
         lty = c(1,1))
##========================

##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##!# VI. Utilizando el modelo ajustado
##+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##Pregunta: Cual es la Pr(Y<=2)?
#resultado: Pr(Y=0)+Pr(Y=1)+Pr(Y=2)
pr.y.0<-dpois(0,lambda =lambda.mle )
pr.y.1<-dpois(1,lambda =lambda.mle )
pr.y.2<-dpois(2,lambda =lambda.mle )
py2<-pr.y.0+pr.y.1+pr.y.2
py2
100*py2
##Pregunta: Cual es la Pr(Y>2)?
#resultado: 1-Pr(Y<=2)
(1-py2)
100*(1-py2)


message("Aca termina el script!")
#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute el script!  ║
#║ El profesor     ╔════╝
#╚═════════════════╝
