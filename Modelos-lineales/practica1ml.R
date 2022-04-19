### Primero se cargan los datos
load("FicheroDatosP1.Rdata")
### Se realiza una primera exploración visual de los datos
head(datos)
#### Se puede observar que el banco cuenta con 16 variables cuantitavias así 
## como de dos variables cualitativas que permiten identificar el individuo,
### en este caso "CodProv" y "NombreProv"
### Para facilitar el trabajo se procederá a separar este banco de datos en variables cualitativas y cuantitativas. La referencia del individuo en el caso del banco de datos cuantitativo se almacenará en el nombre de la fila:
all(rownames(datos) == datos$CodProv) # se comprueba que el nombre de las filas concuerde con la variable "CovProv"
datos_num <- datos[, which(colnames(datos) != "CodProv" & colnames(datos) != "NombreProv")]
head(datos_num)
codigo_nombre <- datos[, which(colnames(datos) == "CodProv" | colnames(datos) == "NombreProv")]

##### Descripción numérica univariante #####
# se crea una función que proporciona algunos estadísticos discriptivos univariantes, se ha decidido añadir un test de normalidad shapiro para comprovar si la variable puede ser descrita por una distribución normal. El test shapiro toma como hipotesis nula que los datos provienen de una distribución normal, así pvalores por encima del nivel de significación indican que se acepta la hipotesis nula de normalidad. 
describe_custom <- function(data){
  require(e1071)
  result <- apply(data, 2, function(x){
    
    
    c(media=mean(x),
      mediana=median(x),
      varianza = var(x),
      des_tipic = sd(x),
      skew = e1071::skewness(x),
      kurto = e1071::kurtosis(x),
      maximo = max(x),
      minimo = min(x),
      rango = max(x)- min(x),
      quantile(x , 0.25 ),
      quantile(x, 0.50),
      quantile(x, 0.75),
      shapiro_pvalor = shapiro.test(x)$p.value)
    
  })
  return(result)
}
numeric_des <- describe_custom(datos_num)

numeric_des["media",]
#Si se observa el vector de media se podrá observar como los datos se encuentran en escalas muy diferentes, principalmente la variable PobTot2018 que esta en el rango del millon. Para poder comparar las distintas variables entre sí se decide realizar una tipificación de las variables. Esta tipificación debe de entenderse como un cambio de variables. 

z_value <- function(data){
  mean_sd <- rbind(media=apply(data, 2, mean), sdd = apply(data, 2, sd))
  data_num_z <- t(apply(data, 1, function(x,y){
    (x - y["media", ])/(y["sdd",])
  }, y = mean_sd))
  return(data_num_z)
}
head(z_value(datos_num))
numeric_des_z <- describe_custom(z_value(datos_num))
numeric_des_z["media",]
#Como se puede observar todas las variables poseen la misma escala. Ahora si se podrá realizar una correcta comparación entre las distintas variables. Cabe destacar que se omitira la comparación de medias y desviaciones típicas pues al tipificar todas estas pasan a ser 0 y 1 respectivamente. 
numeric_des_z["skew",]
#El coeficiente de asimetria aporta una medida de como se distribuyen los datos en función de la media. Así valores iguales a 0 indican una distribución simétrica, valores superiores indican que existe una mayor proporción de valores con una valor numérico superior a la media e inferiores indican que existe una mayor proporción de valores.
#Las siguientes variables se corresponden con el segundo caso mencionado anteriormente.
numeric_des_z["skew",][which(numeric_des_z["skew",] > 0)]
#Las siguientes variables se corresponden con el tercer caso mencionado anteriormente.
numeric_des_z["skew",][which(numeric_des_z["skew",] < 0)]
#Ninguna de las variables posee una distribución totalmente simétrica
numeric_des_z["skew",][which(numeric_des_z["skew",] == 0)]
#Es necesario mencionar que existen mas variables con valores desplazados hacia la derecha de la media, además, este desplazamiento hacia la derecha es especialmente llamativo, se encuentra por encima de la unidad, en las siguientes variables
numeric_des_z["skew",][which(numeric_des_z["skew",] > 1)]
#En el caso de las variables desplazadas hacia la izquierda de la media, solo existe una, con un coeficiente de asimetria superior a 1 en balor absoluto. 
numeric_des_z["skew",][which(numeric_des_z["skew",] < -1)]

#A continuación se observara el coeficiente de kurtosis, en general este coeficiente da una medida de la proporción de valores cercanos a la media y en las colas. Un valor igual a 0 indica una proporción similar a una distribución normal, valores superiores indican que existen tanto valores muy cercanos a la media como en los extremos, y valores menores a cero indican que existe mayor proporción de puntos medios. 
numeric_des_z["kurto",]
### Así las siguientes variables poseen kurtosis positiva
numeric_des_z["kurto",][which(numeric_des_z["kurto",] > 0)]
### Mientras que las siguientes variables poseen kurtosis negativa
numeric_des_z["kurto",][which(numeric_des_z["kurto",] < 0)]
### No existe ninguna variable con kurtosis 0
numeric_des_z["kurto",][which(numeric_des_z["kurto",] == 0)]
### Como se puede observar existen mas comunidades con una kurtosis positiva que negativa. Y en general el valor absoluto de las kurtosis positivas es superior al de las kurtosis negativas. 

#A continuación se analizará el rango entre máximos y mínimos, esto da una medida de el grado de compactación de los valores. 
numeric_des_z["rango",]
# Como se puede observar la variable más compacta es la EsperanzaVidaH2018 mientras que la más dispersa es la PorcPobExtranjera_2018. 

#Por último se comprobara el resultad del test shapiro-wilks realizado. Valores superiores a 0.05, nivel de significancia escogido, indican que no se recha la hipotesis nula de normalidad, mientras que valores inferiores indican que se rechaza esta. 
numeric_des_z["shapiro_pvalor",]
numeric_des_z["shapiro_pvalor",][which(numeric_des_z["shapiro_pvalor",] > 0.05)]
#En dichas variables no se rechaza la hipotesis nula de normalidad. 
numeric_des_z["shapiro_pvalor",][which(numeric_des_z["shapiro_pvalor",] < 0.05)]
#En dichas variables se rechaza la hipotesis nula de normalidad. 

#Por último se realizará una inspección visual de la distribución de cada variable. Para esto se hace uso de la función multiplehist.
multiplehist <- function(data, densiti=FALSE){
  require(ggplot2)
  data <- as.data.frame(data)
  var_name <- names(data)
  
  gathered <- data.frame(list(variables=rep(var_name, rep(nrow(data),ncol(data))) ,values=do.call(unlist, list(x=data,use.names=FALSE))), stringsAsFactors = TRUE)
  
  
  
  if (densiti){
    h <- ggplot(data=gathered) + aes(x = values) + geom_histogram(aes(y=..density..)) + geom_density( alpha=0.4, fill="#FF6666") + facet_wrap( ~ variables, scales = "free")
    print(h)
  }else{
    h2 <- ggplot(data=gathered) + aes(x = values) + geom_histogram()  + facet_wrap( ~ variables, scales = "free")
    print(h2)
  }
  
  
}
multiplehist(z_value(datos_num), densiti = TRUE)
#Si se vuelven a observar los valores de kurtosis y asimetria
numeric_des_z["skew",]
#Se puede observar como la variable PorcPobExtranjera_2018 posee una gran cola hacia la derecha, algo que contrasta con el coeficiente de asimetria encontrado, esta conclusión se puede obtener para las distintas variables. 
numeric_des_z["kurto",]
#Si atendemos tambien a la variable PorcPobExtranjera_2018 se puede entender el coeficiente de kurtosis encontrado, como se puede observar existen valores muy cercanos a la media, así como varios valores extremos. 






##### Realiza un análisis de dátos anómalos #####

#### Datos anómalos univariantes ####

#Primero se realizará una inspección ocular de los distintos boxplot de cada variable mediante la función multipleboxplot.
multipleboxplot <- function(data){
  require(ggplot2)
  data <- as.data.frame(data)
  var_name <- names(data)
  print(data)
  gathered <- data.frame(list(variables=rep(var_name, rep(nrow(data),ncol(data))) ,values=do.call(unlist, list(x=data,use.names=FALSE))), stringsAsFactors = TRUE)
  h <- ggplot(data=gathered) + aes(x = variables, y =values) + geom_boxplot() 
  print(h)
  
  
  
}
multipleboxplot(datos_num)
#como se puede observar, el hecho de la diferencia de escalas dificulta la interpretación. Así se usará el banco de datos tipificado. 
multipleboxplot(z_value(datos_num))
#como se puede observar todas las variables poseen valores extremos, usando como regla para determinación de esta, la misma que en un boxplot. Esto es valores superiores al cuartil superior o inferior mas 3/2 de la distancia entre el cuartil superior o inferior. 
# Estos valores se descartaran y serán sustituidos, imputación, por los valores medios de la variable antes de eliminar estos valores extremos. Para ello se usará la siguiente función
extremos_mean <- function(data, constant){
  extremos_media <- apply(data, 2, function(x){
    d = sum(quantile(x, c(0.25, 0.75))*c(-1, 1))
    c(
      superior = quantile(x, c(0.75),names=FALSE)+d*constant,
      inferior = quantile(x, c(0.25),names=FALSE)-d*constant,
      media = mean(x))
  })
  
  result <- apply(data, 1, function(x, y){
    idx<-which(x < y["inferior",] | x > y["superior",])
    x[idx] <- y["media",idx]
    x
  }, y = extremos_media)
  return(as.data.frame(t(result)))
}
datos_num_smooth <- extremos_mean(datos_num, constant = 3/2)
multipleboxplot(z_value(datos_num_smooth))
#como se puede observar se ha reducido el número de valores extremos, cabe destacar que la representación sigue mostrando valores extremos, esto es debido a que se han cambiado los valores extremos por la media, lo que centra la distribución de los valores, haciendo que otros valores antes no extremos puedan considerarse ahora extremos. 
#Tambien es util observar los histogramas de la variable sin valores extremos
multiplehist(z_value(datos_num_smooth), densiti = TRUE)
## Como se puede observar, los datos son mucho más compactos, y el grado de compactación es homogeneo. 
describe_custom(z_value(datos_num_smooth))["rango",]

#### Datos anómalos multivaiantes ####

## Para la descripción de datos anomalos multivariantes se hara uso de la distancia de Mahalanobis y se utilizara como valor de discriminación k + 3 sqrt(2k) siendo k el número de variables. 

x <- datos_num
rownames(x) <- codigo_nombre$NombreProv
ma <- mahalanobis(x, apply(x, 2, mean), cov(x))
k <- dim(x)[2] 
Lim <- k + 3 * sqrt(k * 2) 
plot(ma, pch = 20, ylim = c(0, max(ma, Lim, na.rm = TRUE)))
text(ma, rownames(x), pos = 2)
abline(h = Lim)
title("Distancia de Mahalanobis")

### Como se puede observar las ciudades autonomas de Ceuta y Melilla son consideradas como outliyers. Esto es lógico si se tienen en cuenta la hidiosincracia de estas regiones.

##### Relación entre las variables #####

#El analisis de las relaciones entre las variables se hara mediante la matriz de correlación para ello se hara uso de la siguiente función,
correlation_ggplot <- function(data){ # Se crea una función que genera gráficos
  #de correlación para la variables numéricas en el argumento data, usando
  #el motor gráfico ggplot2
  tipos <- sapply(data, function(x){
    is.numeric(x)
  })#Se obtiene la posición de las columnas de tipo numerico en el data.frame
  
  numeric_names <- sort(names(data)[tipos])#Se seleccionan los nombres de las 
  #variables de tipo factor. Notese que se han ordenado los nombres, esto es
  #necesario debido a que ggplot2 ordenará posteriormente las variables a 
  #representar. 
  data_new <- data[,numeric_names] #Se crea un data.frame adicional que
  #facilitará la representación con ggplot2
  correlation_mat <- round(cor(data_new),2)#Se crea la matriz de correlación
  #en este caso con la función cor
  
  correlation_mat[upper.tri(correlation_mat)] <- NA #Se elimina la información
  # de la diagonal superior, pues esta repetida
  correlation_mat <- t(correlation_mat) # Por conveniencia se transpone la 
  #matriz
  
  
  
  melt_corr <- data.frame(list(Var1=rep(numeric_names, length(numeric_names)),
                               Var2=rep(numeric_names, 
                                        rep(length(numeric_names),
                                            length(numeric_names))),
                               value=rep(NA, length(numeric_names)^2)))
  #Se crea un data.frame auxiliar con toda la informacióna representar. Este
  #se encuentra vacio y se rellenará con el bucle for que se encuentra más abajo.
  for (x in 1:nrow(melt_corr)){
    if(!is.na(correlation_mat[melt_corr[x, 2], melt_corr[x,1]])){
      melt_corr[x, 3] <- correlation_mat[melt_corr[x, 2], melt_corr[x,1]]
    }
  }#Se rellena el campo value del dataframe creado con la información presente
  #en correlation_mat gracias a la información aportada por las variables
  #Var1 y Var2
  
  melt_corr$Var1 <- as.factor(melt_corr$Var1)
  melt_corr$Var2 <- as.factor(melt_corr$Var2)
  melt_corr$value <- as.numeric(melt_corr$value)#Se convierten las variables
  #al tipo neceario
  melted_cormat <- melt_corr[!is.na(melt_corr$value),]#Se eliminan los NA
  
  h <- ggplot(data = melted_cormat, aes(Var1, Var2, fill = value))+ #Se definen
    #el data.set de donde saldrán los datos, así como que hacer con cada
    #variable
    geom_raster() + #Se genera un geom de tipo raster, una imagen. 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") + #Se crea una escala
    #de color para usar en la imagen creada con geom_raster. 
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+#Se define como se
    #representará la etiqueta del eje x. 
    coord_fixed()#Se fijan las coordenadas del plot creado. 
  j <-  h +
    geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  #Se añaden los valores del coeficiente de correlación en la posición adecuada.
  print(j)#Se imprime el gráfico creado. 
  
}
## Es necesario destacar que estos coeficientes de correlación solo son capaces de medir relaciones lineales entre las variables. 
correlation_ggplot(datos_num)
### Como se puede observar, existe una fuerte relación lineal positiva entre las variables EdadMedia2018 y TasaBrutaMortalidad, esto es lògico pues una población mas envejecidad tendrá mas decesos que una menos envejecida, asumiendo que la mayor fuente de decesos son las personas de mayor edad. 

### Además la mayor correlación negativa la podemos encontrar entre las variables EdadMedia2018 y TBNatalidad2018 algo logico pues a mayor edad media en la población menos individuos capaces de reproducirse y generar nuevos individuos. 

### Tambien es llamativa la correlación negativa entre el PorcMayores65_2018 y el PorcMenores16_2018 que ejemplifica de forma clara el envejecimiento demografico de la población española.

##### Relacijón entre los individuos #####

# Para el analisis de las relaciones entre individuos se realizará un analisis de las distancias entre individuos. 

x <- z_value(datos_num)
rownames(x) <- codigo_nombre$NombreProv
distancias <- dist(x)
heatmap(as.matrix(distancias))

### Como se puede observar, las ciudades autonomas de Ceuta y Melilla son las que poseen la mayor distancias con el resto de las provincias. Las diferencias entre las demás provincias no son tan palpables en esta representación. 

#Además se desea realizar un analisis de grupos mediante el metodo de K-means, a grande rasgos este metodo intenta situar en el mismo grupo a aquellos individuos mas similares. Para verificar que se ha utilizado un valor de k correcto, el número de clusters, se utilizará el metodo de la silueta. Este metodo mide la similaridad de los individuos dentro de un grupo, a mayor este valor, mayor similaridad dentro del kluster. 

valor_silueta <- function(k){
  km.result <- kmeans(datos_num, centers = k, nstart = 50)
  ss <- cluster::silhouette(km.result$cluster, dist(datos_num))
  mean(ss[, 3])
}
k.values <- 2:17
avg_sil_values <- sapply(k.values, valor_silueta)
avg_sil_values
plot(k.values, avg_sil_values, xlab="# clusters", ylab = "Valor medio silueta", type="b")
### Como se puede observar el valor que ofrece un mayor valor de silueta es con un k = 2. 


###
