#### 1 ####

#a#
lambda<-1.8
pv <- 0.51
pm <- 1-pv
dpois(2,lambda)*dbinom(0,2,pv)
dpois(2,lambda)*dbinom(2,2,pm)

#b#
set.seed(123)
N <- 1000000
lambda <- 1.8
p <- 0.49
H <- rpois(N, lambda)
M <- sapply(H, function(h){
  rbinom(1, h, p)
})
pm2 <- mean(M==2)
pm2

h_ <- 0:2
result <- 1-sum(sapply(h_, function(x,lambda,pm2){
  ( dbinom(2, x, 0.49) * dpois(x,lambda) )/pm2
},lambda=lambda,pm2=pm2))
result
#### 2 ####

n <- 15
N <- 1000000
lambda <- 0.2
data_exp <- matrix(qexp(runif(n*N),lambda),ncol=n,nrow=N)
mean_data_exp <- apply(data_exp,1,mean)
plot(density(mean_data_exp),col="blue")
curve(dgamma(x,n,n*lambda),add=TRUE,col="red",lty=2)

