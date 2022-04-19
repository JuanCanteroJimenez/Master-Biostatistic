### 1

##a

N <- 1000
n <- 100
data <- matrix(rnorm(n*N), ncol=n, nrow=N)
p_values <- apply(data, 1, function(x){
  t.test(x, mu=0)$p.value
})
mean(p_values < 0.05)
hist(p_values)
##b los datos siguen una distribución uniforme (0, 1)
##c 
for (x in 0.05*(1:10)){
  N <- 1000
  n <- 100
  data <- matrix(rnorm(n*N,x), ncol=n, nrow=N)
  p_values <- apply(data, 1, function(x){
    t.test(x, mu=0)$p.value
  })
  mean(p_values < 0.05)
  hist(p_values,main=paste("media", x))
  Sys.sleep(4)
}
results <- NULL
set.seed(1)
for (x in 0.05*(1:10)){
  N <- 1000
  n <- 100
  data <- matrix(rnorm(n*N,x), ncol=n, nrow=N)
  p_values <- apply(data, 1, function(x){
    t.test(x, mu=0)$p.value
  })
  results <- c(results, mean(p_values < 0.05))

}
plot((1:10)*0.5, results)
### C como se puede observar aunque existe una diferencia real en las medias, desde el inicio, debido al tamaño del test y al valor de esa diferencia, en muchas ocasiones se da el error de tipo ii, si se cambia el tamaño de la muestra, este se reduce. 


#### 2 
load("Glucosa.Rdata")
head(Glucosa)
nrow(Glucosa)
table(Glucosa$Dieta)
## a
carbo_azucar <- t.test(Glucosa[Glucosa$Dieta=="Azúcares","GDespues"],
                       Glucosa[Glucosa$Dieta=="H. Carbono","GDespues"])
carbo_azucar
## b
azucar_normal <- t.test(Glucosa[Glucosa$Dieta=="Azúcares","GDespues"],
                       Glucosa[Glucosa$Dieta=="Normal","GDespues"],
                       alternative = "less")
azucar_normal
## c
azucar <- t.test(Glucosa[Glucosa$Dieta=="Azúcares","GDespues"],
                        Glucosa[Glucosa$Dieta=="Azúcares","GAntes"],
                        alternative = "t",paired = T)
azucar
carbono <- t.test(Glucosa[Glucosa$Dieta=="H. Carbono","GDespues"],
                                   Glucosa[Glucosa$Dieta=="H. Carbono","GAntes"],
                                   alternative = "t")
carbono
normal <- t.test(Glucosa[Glucosa$Dieta=="Normal","GDespues"],
                  Glucosa[Glucosa$Dieta=="Normal","GAntes"],
                  alternative = "t")
normal


### 3
##a no puesto que también cambia la definición del p-valor. 
##c no dependerá del valor del p-valor, si este es menor que 0.01 si se encontran diferencias significativas para ambos alfas, por el contrario si p-valor < 0.05 y p-valor > 0.01, solo se obtendrian diferencias significativs en el primer caso. 
##b si, puesto que el valor 0.05 es mayor que 0.01.
##d falso, que se rechaze la hipótesis nula no implica probar la hipótesis alternativa. mal, pero creo que ha sido por léxico, cierto si se rechaza la hipótesis nula se da por cierta la hipótesis alternativa. 
## no simplemente no se han encontrado pruebas para rechazarla, pero no para aceptarla. 