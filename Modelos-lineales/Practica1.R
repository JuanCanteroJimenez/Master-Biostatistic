#### 1 ####
###1
n <- 50 
data <- rnorm(n)
media <- mean(data)
mediana <- median(data)
###2
N <- 100
result <- replicate(N, {
  n <- 50 
  data <- rnorm(n)
  c(media = mean(data), mediana = median(data))
})
ecm <- apply((0-result)^2, 1, sum)
ecm
### es más conveniente el uso de la media
###3
N <- 100
result <- replicate(N, {
  n <- 50 
  data <- rt(n,1)
  c(media = mean(data), mediana = median(data))
})
ecm <- apply((0-result)^2, 1, sum)
ecm

#### 2 ####
x <- (1:5000)/1000
y <- sapply(x, function(x){set.seed(1);data <- exp(rnorm(50));-log(prod(dgamma(data,x),x))})
plot(x,y,"l")
x[which.min(y)]


### gradient descent

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
set.seed(1)
x <- exp(rnorm(50))

plot((1:600)/100,dgamma((1:600)/100, result[1],result[2]),"l")
lines(density(x))
result[1]+ c(-1,1)*1.96*(sd(x)/sqrt(length(x)))
arrowsPlot <- function(x, y, lwd = 1, col = 1, angle = 20, length = 0.2) {
  invisible(sapply(1:length(x),
                   function(i) arrows(x[i], y[i], x[i + 1], y[i + 1], lwd = lwd,
                                      col = col, angle = angle, length = length)))
}

gradient_descent_graphics <- function(step, epochs,alpha_init, beta_init){
  alphai <-alpha_init
  betai <- beta_init
  control <- 1
  xs <- (1:100)/10
  ys <- (1:100)/10
  graph <- matrix(0, ncol=length(xs), nrow=length(ys))
  for (x in 1:length(xs)){
    for (y in 1:length(ys)){
      graph[x, y] <- L_mlog(xs[x],ys[y])
    }
  }
  contour(xs, ys, graph)
  alphas <- c()
  betas <- c()
  while(control < epochs){
    alphai <-alphai - step*alpha_deri(alphai,betai)
    betai <- betai - step*beta_deri(alphai, betai)
    alphas <- c(alphas, alphai)
    betas <- c(betas, betai)
    control = control +1
    contour(xs, ys, graph,nlevels=100)
    arrowsPlot(alphas, betas,col=2)
    Sys.sleep(0.1)
  }
  return(c(alphai, betai))
}
gradient_descent_graphics(0.04, 1000,10, 10)
gradient_descent(0.001, 10000)
