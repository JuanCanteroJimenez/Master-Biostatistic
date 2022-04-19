#### 1 ####
set.seed(1)
n <- 100
N <- 10000
data <- matrix(rnorm(N*n), ncol=n, nrow=N)
data_p <- apply(data, 1, function(x){t.test(x, mu=0)$p.value})
hist(data_p)
### Los p.valores singuen una distribución uniforme 0.1
prop <- NULL
for (x in 0.05*(1:20)){
  set.seed(1)
  n <- 100
  N <- 1000
  data <- matrix(rnorm(N*n,x), ncol=n, nrow=N)
  data_p <- apply(data, 1, function(x){t.test(x, mu=0)$p.value})
  prop <- c(prop,mean(data_p < 0.05) )
}
prop
plot(0.05*(1:20), prop)
prop
### si se repite el mismo test con distintos valores de n se verá como la cantidad de errores de tipo 2 van disminuyendo, si aumenta n. Esto es lógico 

#### 2 ####
Glucosa
