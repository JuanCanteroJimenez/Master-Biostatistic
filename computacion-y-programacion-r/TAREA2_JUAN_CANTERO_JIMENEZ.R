### ejercicio 1 ###
data <- read.csv("separaciones.csv",header = TRUE)
data <- data[,-1]
h <- subset(data, 1:nrow(data)%%2 == 1)#Se seleccinan los casos impares haciendo uso de la función subset y de una expresión logica, 1:nrow(data)%%2 == 1, que divide las distintas posiciones entre 2 y se queda con el resto del cociente y devuelve True si este es 1.
h2 <- subset(data, 1:nrow(data)%%5 == 0)#Se seleccionan los casos que son multiplos de 5 haciendo uso de la función subset de una expresión logica similar a la anterior, 1:nrow(data)%%5 == 0, que divide las distintas posiciones entre 5 y se queda con el resto del cociente y devuelve True si este es 0. 
### ejercicio 2 ###
n_h <- 100 #se asigna el entero 100 a la variable n_h que representa el número de hombre
n_m <- 40 #se asigna el entero 40 a la variable n_m que representa el número de mujeres
muestra <- c(rnorm(n_h, 165, 5), rnorm(n_m,160,10))#se generan n_h datos que siguen una N(165, 5) y n_m datos que siguen una N(160,10), que se concatenan y se asignan al vector muestra. 
grupos <- as.factor(c(rep("hombre",n_h),rep("mujer",n_m)))# se genera un factor que recoge los distitnos grupos de la muestra, para esto se crea un vector concatenando n_h veces la palabra hombre y n_m veces la palabra mujer, y este se pasa a factor. 
### ejercicio 3 ###
datos.alturas <- data.frame(alturas=muestra,
                            grupos=grupos)#se crea un data.frame con las variables alturas, que posee los datos del vector muestra, y grupos que posee los datos del factor grupos. 
write.table(datos.alturas, "datosalt.txt")#se convierte el dataframe creado al archivo .txt, datosalt.txt, con la función write.table.
datos.alturabis<-read.table("datosalt.txt")#se leen dichos datos con la función read.table
datos.alturas <- cbind(datos.alturas, logaritmo=log(datos.alturas$alturas)) #se añade la variable logaritmos al dataframe datos.alturas haciendo uso de la función cbind(), estas variable contienen el resultado de aplicar la función log() a datos.alturas$alturas
datos.alturas
### ejercicio 4 ###
#Comentarios generales, se calcula el estadístico pedido en cada caso del vector datos.alturas$alturas y datos.alturas$logaritmo haciendo uso de la función definida para cada uno de ellos. Para el calculo de estos estadísticos por grupos, se hace uso de la función tapply que permite aplicar una función a los grupos definidos por el factor datos.alturas$grupos.
#mediana
median(datos.alturas$alturas)
median(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, median)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, median)
#desviación típica
sd(datos.alturas$alturas)
sd(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, sd)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, sd)
#máximo
max(datos.alturas$alturas)
max(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, max)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, max)
#coeficiente de asimetria

EnvStats::skewness(datos.alturas$alturas)
EnvStats::skewness(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, EnvStats::skewness)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, EnvStats::skewness)
#curtosis
e1071::kurtosis(datos.alturas$alturas)
e1071::kurtosis(datos.alturas$logaritmo)
tapply(datos.alturas$alturas, datos.alturas$grupos, e1071::kurtosis)
tapply(datos.alturas$logaritmo, datos.alturas$grupos, e1071::kurtosis)
### ejercicio 5 ### 

dataset <-haven::read_sav("ambiente.sav")#Se importa el dataset ambiente.sav haciendo uso de la función read_sav alojada en el paquete haven. 
medias_ph <- by(dataset$PH, dataset$PROVIN,mean)#Se calculan las medias de la variable PH por provincias hacienod uso de la función by()

### ejercicio 6 ###

### De data.frame a array

data_array <- as.array(rep(0,nrow(data)))#Se crea un array vacio con un tamaño igual al número de observaciones del dataframe data. 
dim(data_array) <- unlist(lapply(data, function(x){
  length(unique(x))
}))[1:ncol(data)-1]#Se definen las dimensiones de dicho array, para esto se hace uso de la función dim() que permite definir las dimensiones del array mediante un vector. Este vector se creará mediante un lapply que recorre las variables del dataset, y devuelve la longitud del vector de datos únicos de cada columna.
dimnames(data_array) <- list(Provincias=unique(as.character(data$Provincias)),
                             
                             Tipo.de.disolución.matrimonial=unique(data$Tipo.de.disolución.matrimonial),Periodo=unique(data$Periodo)) #Se definen los nombres de las dimensiones del array data_array, mediante la función dimnames, que toma una lista de vectores como valor de entrada para fijar el nombre de las dimensiones. 

for (x in 1:nrow(data)){
  data_array[as.character(data[x,1]),as.character(data[x,2]),as.character(data[x,3])] <- data[x,4]
}#Se rellena la variable data_array mediante un bucle for que recorre las distintas observaciones de data, extrayendo el valor de cada variable. El valor de las tres primeras variables, Provincias, Tipo de disolución y Periodo serán usados para definir la posición de la variable Total en el array, correspondiente a cada observación. 
data_array

#a
media_periodo <- sapply(dimnames(data_array)$Periodo, function(x,y){
  mean(y[,"Total",x],na.rm=TRUE)
},y=data_array)#Se obtiene la media agrupando los datos por periodo, para esto se hace uso de la función sapply que recorre un vector con los distintos periodos presentes en el dataset, y gracias a este valor se extrae los valores necesarios del array y se realiza su media.

media_provincia <- sapply(dimnames(data_array)$Provincia, function(x,y){
  mean(y[x,"Total",],na.rm=TRUE)
},y=data_array)#Equivalente a lo descrito en la línea anterior
#b
varianza_provincia <- sapply(dimnames(data_array)$Provincia, function(x,y){
  var(y[x,"Total",],na.rm=TRUE)
},y=data_array)#Equivalente a lo realizado en el apartado a

#c
cuartiles_periodo <- sapply(dimnames(data_array)$Periodo, function(x,y){
  quantile(y[,"Total",x],na.rm=TRUE,c(0.25,0.50,0.75))
},y=data_array)#Equivalente a lo realizado en el apartado a.

#d
for_plot <- as.data.frame.table(data_array[,"Total",])
#Se genera un data.frame con el total de separaciones por periodo y provincia. 

par(cex=0.3)#Se ajusta el tamaño de la fuente, esto es conveniente debido a que la leyenda posee mucha información.
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
                 trace.label = "Provincias")#Se crea un plot de la evolución temporal media por provincia del total de separaciones, nulidades y divorcios por año y por comunidad autónoma, para esto se hace uso de la función interaction.plot que toma como valores de entrada, x.factor que indica la variable que agrupa el eje de las x, trace.factor que indica la otra variable de agrupación, y finalmente response en el que se indican los datos a agrupar.
title(main="Logaritmo de la tasa de variación media del numero de separaciones por provincia y año")#Se añade título al plot.

