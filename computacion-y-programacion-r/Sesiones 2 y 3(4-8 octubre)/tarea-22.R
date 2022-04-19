### Ejercicio 1 #####

v1 <- exp(1):1 
print(v1)
v1c <- as.character(v1)
print(v1c)
v1i <- as.integer(v1c)
print(v1i)

v1 == v1i

v1-v1i

v2 <- 1:exp(1)
print(v2)
v2c <- as.character(v2)
print(v2c)
v2i <- as.integer(v2c)
print(v2i)

v2 == v2i

v2-v2i

abs(v2-v1)
v2 == v1


#### Ejercicio 2 #####
set.seed(1)

sim <- function(){
  v <- rt(100, 10)
  return(table(cut(v,5)))
}
sim()

##### Ejercicio 3 #####

#### a 
bin <-  rep(c("SANO","Enfermo"), c(500, 500))
length(bin)
table(bin)

#### b 
ji <- rep(seq(1,20,length.out=100) , 2)
ji

#### c 
ja <- rep(1:6, 1:6)
table(ja)

#### d
ju <- c(seq(1, 4), seq(4, 1) , rep(1, 5))
ju


###### Ejercicio 4 #######

data = read.csv("20173.csv",sep = ";")
## clase y tipo de los datos
for (x in colnames(data)){
  cat("La variable",x,"es de clase",class(data[[x]]),"\n")
  cat("La variable",x,"es de tipo",typeof(data[[x]]),"\n")
}
data$Provincias <- as.factor(data$Provincias)
data$Tipo.de.disolución.matrimonial <- as.factor(data$Tipo.de.disolución.matrimonial)
#Es necesario quitar el punto que en español indica unidades de millar pero que en ingles indica los decimales, esto se realiza con la función gsub y una expresión regular del tip \\.
data$Total <- as.numeric(gsub("\\.","",data$Total))
for (x in colnames(data)){
  cat("La variable",x,"es de clase",class(data[[x]]),"\n")
  cat("La variable",x,"es de tipo",typeof(data[[x]]),"\n")
}
write.csv(data,"separaciones.csv")
