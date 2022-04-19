##### Tarea 1 #####
set.seed(seed=21121)
x1=rnorm(100)
set.seed(seed=21122)
x2=rnorm(100,sd=2)
set.seed(seed=21123)
x3=rnorm(100,sd=0.5)
dat.t1<-data.frame(x1,x2,x3)

pairs(dat.t1)
pca <- princomp(dat.t1)
### ¿Qué proporción de varianza explica la primera componente principal? ¿y la segunda? ¿y la tercera? : respectivamente 0.33, 0.667, 1.00
pca$loadings
summary(pca)
### Interpreta cómo se forma (con qué variables originales) la primera componente principal, y la segunda y la tercera. : la primera componente esta formada por la variable x2, y aporta principalmente forma, la segunda componente se forma con la variable x1 y aporta principalmente tamaño, ?valor absoluto, la tercera componente se forma con la variable 3 y aporta forma principalmente. 
### El pca realizado es patológico devido a que las variables no poseen correlación alguna. 

Sigma <- matrix(c(10,2,3,
                  2),2,2)
j <- MASS::mvrnorm(n = 100, rep(0, 2), Sigma)
pca2 <- princomp(j)
pca2$loadings


##### Tarea 2 #####

load("EPF2.RData")
head(EPF)
rownames(EPF) <- EPF$NoName
EPF <- EPF[,-1]
head(EPF)
pairs(EPF)
image(1:9,1:9,cor(EPF))
barplot(apply(EPF, 2, mean))
boxplot(EPF)

###Explica si consideras más adecuado hacer un Análisis de Componentes Principales basado en la matriz de covarianza de los datos o basado en su matriz de correlaciones : Puesto que existe una ligera diferencia en la escala de las distintas variables, se decide realizar el analisis PCA con la matriz de correlación

pca <- princomp(EPF, cor = FALSE)
pca$loadings
plot(pca)
summary(pca)
### Explora cuántas componentes principales necesitas para explicar el 80% de la varianza original del banco de datos : Seran necesarias las tres primeras componentes principales para explicar un 80% de la variabilidad. 
pca$loadings
### Explora e intenta interpretar las dos primeras componente : La primera componente aporta principalmente tamaño pues todos sus pesos son positivos,puesto que mayor valor en las variables indica mas gasto, y esto en un principio es mejor las regiones mas ricas estarán mas a la derecha en el eje x, la segunda componente aportará forma, y como posee varios componentes con valor negativo, se espera que las observaciones con mayor absoluto en estas variables no esten totalmente desplazadas a la derecha . 

plot(pca$score[,1:2],col=as.factor(rownames(pca$scores)))
library(ggplot2)
provincias <- rownames(EPF)
provincias[which(provincias == "\xc1vila")] <- "Avila"
provincias[which(provincias == "\xc1lava")] <- "Alava"
gathered <- data.frame(comp1 = as.numeric(pca$scores[,1]),
                       comp2 = as.numeric(pca$scores[,2]),
                       prov = as.factor(provincias))
ggplot(data = gathered,aes(x=comp1, y=comp2, col = prov, label=prov))  + geom_point() + geom_text(hjust=0, vjust=0)

### Se han obtenido los resultados esperables, pues como se dijo en el análisis de componentes principales, el primer término es positivo para todas las variables, asi regiones con un alto valor absoluto en las variables apareceran mas a la derecha. 

##### Tarea 3 #####

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

### Calcula las desviaciones típicas de las variables cuantitativas del banco de datos. Responde, a la vista del significado de las variables y del resultado anterior, si consideras que se debería realizar el análisis de componentes principales con la matriz de varianzas-covarianzas o con la de correlaciones : 
load("datos_prac1_ok.RData")
cod_nombre <- datos.p1[, 1:2]
datos.p1 <- datos.p1[,-(1:2)]
numeric_des <- describe_custom(datos.p1)
numeric_des

### Será necesario el uso de la matriz de correlaciones debido a que existe una gran diferencia de escala en los datos, tanto en valor absoluto como de varianzas. 

pca <- princomp(datos.p1, cor=TRUE)
summary(pca)
### ¿Qué porcentaje de varianza del banco de datos original explica la primera componente principal? ¿Y la segunda? : respectivamente el 0.319432 y 0.20468448

### ¿Con cuántas componentes principales nos deberíamos quedar si queremos mantener al menos el 90% de la varianza del banco de datos orignal? : Seran necesarias las 7 primeras componentes principales para recoger un 90 % de la variabilidad. 

loads <- as.matrix(pca$loadings)
colors <- rep("grey", nrow(loads))
colors[loads[,1] > 0.1] <- "red"
colors[loads[,1] < -0.1] <- "blue"
par(mar= c(5.1, 14.1, 4.1, 2.1))
barplot(loads[,1],horiz = T,las=1,main = "Loads primera componente principal", col=colors)


colors <- rep("grey", nrow(loads))
colors[loads[,2] > 0.1] <- "red"
colors[loads[,2] < -0.1] <- "blue"
par(mar= c(5.1, 14.1, 4.1, 2.1))
barplot(loads[,2],horiz = T,las=1,main = "Loads segunda componente principal", col=colors) 
library(ggplot2)


### Intenta interpretar de forma breve el significado de la primera componente principal : la primera componente principal arroja coeficientes positivos para las variables, PorcMayores65_2018, EdadMedia2018, TasaBrutaMortalidad, EsperanzaVidaH2018, EsperanzaVidaM2018, PorcParoIndustria, PorcParoConstrucción. Además aporta coeficientes negativos a TBNatalidad2018, TasaMortalidadMenores5anyos, PorcParoServicios, PorcPAroOtros. En general no se puede interpretar de forma clara esta componente pues mezcla muchos parámetros de distinta indole, además de dar coeficientes positivos y negativos a variables que indican lo mismo en distinto sector, vease PorcParoServicios y PorcParoAgricultura. 
comunidades_provincias <- read.csv("ComunidadesProvincias.csv",sep = ";",encoding="latin1")

cod_nombre_color <- t(apply(cod_nombre, 1, function(x, y){
  c(CodProv=as.numeric(x[1]), x[2], CCAA=y[, "CCAA"][y[,"Cprov"]==as.numeric(x[1])])
}, y = comunidades_provincias))
cod_nombre_color
cod_nombre_color <- as.data.frame(cod_nombre_color)
gather <- data.frame(comp1 = pca$scores[,1], comp2 = pca$scores[,2], coloring = factor(cod_nombre_color$CCAA))
library(ggplot2)
ggplot(data=gather, aes(x=comp1, y=comp2, col=coloring))+geom_point()
