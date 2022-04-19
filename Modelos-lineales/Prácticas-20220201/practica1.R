### 1

# A y B
set.seed(1)
n <- 100
N <- 100
data <- matrix(rnorm(n*N), nrow=100, ncol=100)
# C
medias<-apply(data, 1, mean)
medianas <- apply(data, 1, median)
MQE_medias <- mean((0-medias)^2)
MQE_medias
MQE_medianas <- mean((0-medianas)^2)
MQE_medianas
# D
n <- 100
N <- 100
data <- matrix(rt(n*N,1), nrow=100, ncol=100)

medias<-apply(data, 1, mean)
medianas <- apply(data, 1, median)
MQE_medias <- mean((0-medias)^2)
MQE_medias
MQE_medianas <- mean((0-medianas)^2)
MQE_medianas


### 2

L_mlog <- function(alpha,beta){
  set.seed(1)
  x <- exp(rnorm(50))
  return(-sum(dgamma(x, alpha, beta,log = TRUE)))
  
}
alpha_deri <- function(alpha, beta){
  L_mlog <- function(alpha,beta){
    set.seed(1)
    x <- exp(rnorm(50))
    return(-sum(dgamma(x, alpha, beta,log=TRUE)))
    
  }
  h <- 1e-9
  (L_mlog(alpha+h, beta)-L_mlog(alpha, beta))/h
}
beta_deri <- function(alpha, beta){
  L_mlog <- function(alpha,beta){
    set.seed(1)
    x <- exp(rnorm(50))
    return(-sum(dgamma(x, alpha, beta,log=TRUE)))
    
  }
  h <- 1e-9
  (L_mlog(alpha, beta+h)-L_mlog(alpha, beta))/h
}

gradient_descent <- function(step, epochs){
  alphai <- abs(rnorm(1))
  betai <- abs(rnorm(1))
  control <- 1
  
  while(control < epochs){
    alphai <-alphai - step*alpha_deri(alphai,betai)
    betai <- betai - step*beta_deri(alphai, betai)
    
    control = control +1
  }
  return(c(alphai, betai))
}
result <-gradient_descent(0.0001, 5000)
result
set.seed(1)
x <- exp(rnorm(500))

plot((1:600)/100,dgamma((1:600)/100, result[1],result[2]),"l")
lines(density(x))
result[1]+ c(-1,1)*1.96*(sd(x)/sqrt(length(x)))
arrowsPlot <- function(x, y, lwd = 1, col = 1, angle = 20, length = 0.2) {
  invisible(sapply(1:length(x),
                   function(i) arrows(x[i], y[i], x[i + 1], y[i + 1], lwd = lwd,
                                      col = col, angle = angle, length = length)))
}
### falta el intervalo de confianza, para eso hace falta la matriz de derivadas parciales

### 3
N <- 1000
n <- 100
#set.seed(1)
data <- matrix(rnorm(n*N), ncol=n, nrow=N)
confi <- apply(data, 1, function(x){
  lower <- mean(x)-1.96*(1/sqrt(length(x)))
  upper <- mean(x)+1.96*(1/sqrt(length(x)))
  if (0 < lower | 0 > upper){
    return(FALSE)
  }else{return(TRUE)}
})
mean(confi)
#### 4
set.seed(1)
n1 <- 10
n2 <- 10
x <- rnorm(n1)
y <- rnorm(n2, 1)
t.test(x, y)

set.seed(1)
n1 <- 20
n2 <- 30
x <- rnorm(n1)
y <- rnorm(n2, 1)
t.test(x, y,alternative = "less")
