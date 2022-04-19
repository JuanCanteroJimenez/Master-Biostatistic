
#
data <- read.csv("separaciones.csv",header = TRUE)
data <- data[,-1]
h <- subset(data, 1:nrow(data)%%2 == 1)
#Se seleccinan los casos impares haciendo uso de la función 
#subset y de una expresión logica, 1:nrow(data)%%2 == 1, que
# divide las distintas posiciones entre 2 y se queda con el 
#resto del cociente y devuelve True si este es 1
h2 <- subset(data, 1:nrow(data)%%5 == 0)
##Se seleccionan los casos que son multiplos de 5 haciendo u
#so de la función subset de una expresión logica similar a l
#a anterior, 1:nrow(data)%%5 == 0, que divide las distintas 
#posiciones entre 5 y se queda con el resto del 

#
n_h <- 100 
# variable n_h que representa el número de hombre

n_m <- 40 
#ariable n_m que representa el número de mujeres

muestra <- c(rnorm(n_h, 165, 5), rnorm(n_m,160,10))
#_m,160,10))#se generan n_h datos que siguen una N(165, 5) y
# n_m datos que siguen una N(160,10), que se concatenan y se
# asignan al vector muestra. 

grupos <- as.factor(c(rep("hombre",n_h),rep("mujer",n_m)))
#rep("mujer",n_m)))# se genera un factor que recoge los dist
#itnos grupos de la muestra, para esto se crea un vector con
#catenando n_h veces la palabra hombre y n_m veces la palabr
#a mujer, y este se pasa a factor. 


#
datos.alturas <- data.frame(alturas=muestra,
                            grupos=grupos)
#s)#se crea un data.frame con las variables alturas, que pos
#ee los datos del vector muestra, y grupos que posee los dat
#os del factor grupos. 

write.table(datos.alturas, "datosalt.txt")
#")#se convierte el dataframe creado al archivo .txt, datosa
#lt.txt, con la función write.table.

datos.alturabis<-read.table("datosalt.txt")
#t")#se leen dichos datos con la función read.table

datos.alturas <- cbind(datos.alturas, logaritmo=log(datos.alturas$alturas)) 
#garitmo=log(datos.alturas$alturas)) #se añade la variable l
#ogaritmos al dataframe datos.alturas haciendo uso de la fun
#ción cbind(), estas variable contienen el resultado de apli
#car la función log() a datos.alturas$alturas

datos.alturas

#

#tadístico pedido en cada caso del vector datos.alturas$altu
#ras y datos.alturas$logaritmo haciendo uso de la función de
#finida para cada uno de ellos. Para el calculo de estos est
#adísticos por grupos, se hace uso de la función

#
median(datos.alturas$alturas)
median(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, median)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, median)

#
sd(datos.alturas$alturas)
sd(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, sd)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, sd)

#
max(datos.alturas$alturas)
max(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, max)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, max)

#

EnvStats::skewness(datos.alturas$alturas)
EnvStats::skewness(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, EnvStats::skewness)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, EnvStats::skewness)

#
e1071::kurtosis(datos.alturas$alturas)
e1071::kurtosis(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, e1071::kurtosis)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, e1071::kurtosis)

#

dataset <-haven::read_sav("ambiente.sav")
#)#Se importa el dataset ambiente.sav haciendo uso de la fun
#ción read_sav alojada en el paquete haven. 

medias_ph <- by(dataset$PH, dataset$PROVIN,mean)
#IN,mean)#Se calculan las medias de la variable PH por provi
#ncias hacienod uso de la función by()



#


#

data_array <- as.array(rep(0,nrow(data)))
#)#Se crea un array vacio con un tamaño igual al número de o
#bservaciones del dataframe data. 

dim(data_array) <- unlist(lapply(data, function(x){
  length(unique(x))
}))[1:ncol(data)-1]
#siones de dicho array, para esto se hace uso de la función 
#dim() que permite definir las dimensiones del array mediant
#e un vector. Este vector se creará mediante un lapply que r
#ecorre las variables del dataset, y devuelve la
dimnames(data_array) <- list(Provincias=unique(as.character(data$Provincias)),
                             
                             Tipo.de.disolución.matrimonial=unique(data$Tipo.de.disolución.matrimonial),Periodo=unique(data$Periodo)) 
#olución.matrimonial=unique(data$Tipo.de.disolución.matrimon
#ial),Periodo=unique(data$Periodo)) #Se definen los nombres 
#de las dimensiones del array data_array, mediante la funció
#n dimnames, que toma una lista de vectores como

for (x in 1:nrow(data)){
  data_array[as.character(data[x,1]),as.character(data[x,2]),as.character(data[x,3])] <- data[x,4]
}
#ante un bucle for que recorre las distintas observaciones d
#e data, extrayendo el valor de cada variable. El valor de l
#as tres primeras variables, Provincias, Tipo de disolución 
#y Periodo serán usados para definir la posición
data_array


#
media_periodo <- sapply(dimnames(data_array)$Periodo, function(x,y){
  mean(y[,"Total",x],na.rm=TRUE)
},y=data_array)
#pando los datos por periodo, para esto se hace uso de la fu
#nción sapply que recorre un vector con los distintos period
#os presentes en el dataset, y gracias a este valor se extra
#e los valores necesarios del array y se realiza

media_provincia <- sapply(dimnames(data_array)$Provincia, function(x,y){
  mean(y[x,"Total",],na.rm=TRUE)
},y=data_array)
#o en la línea anterior


#
varianza_provincia <- sapply(dimnames(data_array)$Provincia, function(x,y){
  var(y[x,"Total",],na.rm=TRUE)
},y=data_array)
#do en el apartado a



#
cuartiles_periodo <- sapply(dimnames(data_array)$Periodo, function(x,y){
  quantile(y[,"Total",x],na.rm=TRUE,c(0.25,0.50,0.75))
},y=data_array)
#do en el apartado a.



#
for_plot <- as.data.frame.table(data_array[,"Total",])

# separaciones por periodo y provincia. 


par(cex=0.3)
#uente, esto es conveniente debido a que la leyenda posee mu
#cha información.

interaction.plot(x.factor=for_plot$Periodo,
                 trace.factor=for_plot$Provincias,
                 response=log(for_plot$Freq),
                 legend = TRUE,
                 type="l",
                 col=colors()[2:length(unique(for_plot$Provincias))+1],
                 lty=1,
                 xpd=TRUE,
                 lwd=1,
                 xlab="Periodo",
                 ylab="log(Separaciones)",
                 trace.label = "Provincias")
#as")#Se crea un plot de la evolución temporal media por pro
#vincia del total de separaciones, nulidades y divorcios por
# año y por comunidad autónoma, para esto se hace uso de la 
#función interaction.plot que toma como valores 
title(main="Logaritmo de la tasa de variación media del numero de separaciones por provincia y año")
#ación media del numero de separaciones por provincia y año"
#)#Se añade título al plot.


