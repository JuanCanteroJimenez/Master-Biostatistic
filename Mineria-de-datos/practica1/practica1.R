load("FicheroDatosP1.Rdata")
#### Tarea 1: Realiza un análisis univariante numérico del banco de datos #### +

datos_num <- datos[,sapply(datos, is.numeric)]
numeric_descrip <- apply(datos_num, 2, function(x){

  
  c(media=mean(x),
    mediana=median(x),
    varianza = var(x),
    des_tipic = sd(x),
    skew = e1071::skewness(x),
    kurto = e1071::kurtosis(x))
  
})
numeric_descrip
### como se puede observar, los datos se encuentran en escalas diferentes, en orden de minimizar este efecto se realizarán varios métodos de escalado de características. 

#### Normalización de tipo max-min
max_min <- rbind(maxi=apply(datos_num, 2, max), mini = apply(datos_num, 2, min))
data_num_maxmin <- t(apply(datos_num, 1, function(x,y){
  (x - y["mini", ])/(y["maxi", ]-(y["mini",]))
}, y = max_min))
data_num_maxmin
numeric_descrip_maxmin <- apply(data_num_maxmin, 2, function(x){
  
  
  c(media=mean(x),
    mediana=median(x),
    varianza = var(x),
    des_tipic = sd(x),
    skew = e1071::skewness(x),
    kurto = e1071::kurtosis(x))
  
})
numeric_descrip_maxmin


#### Z-score Normalization
mean_sd <- rbind(media=apply(datos_num, 2, mean), sdd = apply(datos_num, 2, sd))
data_num_z <- t(apply(datos_num, 1, function(x,y){
  (x - y["media", ])/(y["sdd",])
}, y = mean_sd))
data_num_z
numeric_descrip_z <- apply(data_num_z, 2, function(x){
  
  
  c(media=mean(x),
    mediana=median(x),
    varianza = var(x),
    des_tipic = sd(x),
    skew = e1071::skewness(x),
    kurto = e1071::kurtosis(x))
  
})
numeric_descrip_z

#### Representaciones gráficas:
#Histogramas
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
multiplehist(datos_num,FALSE)
multiplehist(data_num_z,TRUE)

#### Analisis de outliers univariante ####
multipleboxplot <- function(data){
  require(ggplot2)
  data <- as.data.frame(data)
  var_name <- names(data)
  print(data)
  gathered <- data.frame(list(variables=rep(var_name, rep(nrow(data),ncol(data))) ,values=do.call(unlist, list(x=data,use.names=FALSE))), stringsAsFactors = TRUE)
    h <- ggplot(data=gathered) + aes(x = variables, y =values) + geom_boxplot() 
    print(h)
  
  
  
}
multipleboxplot(data_num_z)
### como se puede observar, existen bastantes valores extremos
### se crea una función que elimina los valores extremos sustituyendolos por la media antes de la sustitución, como criterio de discrimación: valores superiores al cs + d*3; donde cs = C3, d = distancia c3-c1 o valores inferiores a ci - d*3

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


extremos_graphic <- function(data, constant){
  extremos_media <- apply(data, 2, function(x){
    d = sum(quantile(x, c(0.25, 0.75))*c(-1, 1))
    c(
      superior = quantile(x, c(0.75),names=FALSE)+d*constant,
      inferior = quantile(x, c(0.25),names=FALSE)-d*constant,
      media = mean(x))
  })
  
  result <- t(apply(data, 1, function(x, y){
    idx<-which(x < y["inferior",] | x > y["superior",])
    idx2<-which(x > y["inferior",] | x < y["superior",])
    x[idx2] <- 0
    x[idx] <- 1
    
    x
  }, y = extremos_media))
  x = result
  par( mar = par( "mar" ) + c( 3, 10, 1, 1 ) )
  image( x, xaxt= "n", yaxt= "n" )
  axis( 2, at=seq(0,1,length.out=ncol( x ) ), labels= colnames( data ), las= 2 )
  axis( 1, at=seq(0,1,length.out=nrow( x ) ), labels= rownames( data ), las= 2)
  par(mar=c(0,0,0,0))
  #e <- expand.grid(seq(0,1, length=ncol(result)), seq(1,0, length=4))
}

extremos_graphic(datos_num, constant=1.5)

datos_num_filter <- extremos_mean(datos_num, constant=1.5)
multiplehist(datos_num,TRUE)
multiplehist(datos_num_filter,TRUE)
multipleboxplot(j)

#### Analisis de valores extremos vivariante 

cov_mat <-cov(datos_num)
mean_data <- apply(datos_num, 2, mean)
(datos_num[1,]-mean_data)%*%solve(cov_mat)%*%t(datos_num[1,]-mean_data)
