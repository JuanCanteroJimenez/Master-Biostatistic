#### 1 ####
#a media
x <- 1:7
p <- 1/7
media <-sum(x*p)
barplot(rep(1/7,7))
points(media,0,col="red")

varianza <- sum((x^2)*p)-media^2
varianza
#b
x <- 0:4
n <- 4
p <- 1/2
media <- sum(x*dbinom(x,n,p))
barplot(dbinom(x,n,p))
points(sum(x*dbinom(x,n,p)),0,col="red")
varianza <- sum((x^2)*dbinom(x,n,p))-media^2
varianza
#### 2 #### comprobando, la x de este ejercicio sera la x del apartado b.
#a E(2X-3)
x <- 0:4
x_t <- x*2 - 3
p <- 1/2
media_t <- sum(x_t*dbinom(x,n,p))
media_t
## resultado analitico 2mu-3
(media*2)-3
## coinciden
#a V(2X-3)
varianza_t <- sum((x_t^2)*dbinom(x,n,p))-media_t^2
varianza_t
#resultado analitico 2^2 v(x)
2^2 * varianza
###b
x <- 0:4
x_t <- 5-x
media_t <- sum(x_t*dbinom(x,n,p))
varianza_t <- sum((x_t^2)*dbinom(x,n,p))-media_t^2
varianza_t
### resultado analitico v(x)
##c E[(x-2)(x+1)]
x <- 0:4
x_t <- (x-2)*(x+1)
media_t <- sum(x_t*dbinom(x,n,p))
media_t
### resultado analitico v(x)
varianza^2 + media^2 - media - 2

#### 3 ####
## a 
x <- c(1,2,1000)
dx <- c(1/4, 1/2, 1/4)
sum(x*dx)
## b 
p.analitica <- 1 - exp(-1)
p.analitica
lambda <- 10
media <- 1/lambda
pexp(media,lambda)
## c 
lambda <- 6
m <- -log(1/2)/lambda
m
1-pexp(m, lambda)

### 4 Se hizo en clase

###  5
x <- 1:3
y <- 1:3
fx <- function(x){p <- c(0.31,0.39,0.30);return(p[x])}
fy <- function(x){p <- c(0.47,0.27,0.26);return(p[x])}
fxy <- function(x,y){
  p <- matrix(c(0.12,0.08,0.11,
                0.18,0.14,0.07,
                0.17,0.05,0.08),byrow=TRUE, ncol=3,nrow=3)
  return(p[x,y])
}
fxy(1,3)
fz <- function(z){p <- c(0.12, 0.26, 0.28, 0.14, NaN, 0.12, NaN, NaN, 0.08); return(p[z])}
z <- c(1,2,3,4,6,9)
sum(fz(z))
esperanza_z <- sum(z*fz(z))
esperanza_z
covari <- esperanza_z - sum(x*fx(x))*sum(y*fy(y))
covari
varianza_x <- sum((x^2)*fx(x)) - sum(x * fx(x))^2
varianza_x
varianza_y <- sum((y^2)*fy(y)) - sum(y * fy(y))^2
varianza_y
corre <- covari/sqrt(varianza_x*varianza_y)
corre

#### 6 ####
fxy <- function(x,y){
  p <- matrix(c(0.1,0.05,0.02,0.02,
                0.05, 0.20, 0.05, 0.02,
                0.02, 0.05, 0.20, 0.04,
                0.02,0.02,0.04,0.10),byrow=TRUE, ncol=4, nrow=4)
  return(p[x,y])
}
fx <- function(x){
  p <- matrix(c(0.1,0.05,0.02,0.02,
                0.05, 0.20, 0.05, 0.02,
                0.02, 0.05, 0.20, 0.04,
                0.02,0.02,0.04,0.10),byrow=TRUE, ncol=4, nrow=4)
  pmarginal <- apply(p, 2, sum)
  return(pmarginal[x])
}
fy <- function(x){
  p <- matrix(c(0.1,0.05,0.02,0.02,
                0.05, 0.20, 0.05, 0.02,
                0.02, 0.05, 0.20, 0.04,
                0.02,0.02,0.04,0.10),byrow=TRUE, ncol=4, nrow=4)
  pmarginal <- apply(p, 1, sum)
  return(pmarginal[x])
}

y <- 1:4
x <- 1:4
eyx = sapply(x, function(x,y){
   
   sum(y*(fxy(x,y)/fx(x)))
       },y=y)

fx(1:4)
 
y <- 1:4
x <- 1:4
ex2y<- sapply(y, function(x,y){
  
  sum((y^2)*(fxy(y,x)/fy(x)))
},y=x)
ex2y

exy<- sapply(y, function(x,y){
  
  sum((y)*(fxy(y,x)/fy(x)))
},y=x)
ex2y[4]-exy[4]^2

 mat <- matrix(c(0.10,0.05,0.02,0.02,0.05,0.20,0.05,0.02,0.02,0.05,
                0.20,0.04,0.02,0.02,0.04,0.10),ncol=4) # Tabla de probabilidades
fx <- apply(mat,2,sum)
# Probabilidades marginales de X
fy <- apply(mat,1,sum)
# Probabilidades marginales de Y
fy_x1 <- mat[,1]/fx[1]
# Probabilidades condicionales de Y dado X=1
fy_x2 <- mat[,2]/fx[2]
# Probabilidades condicionales de Y dado X=2
fy_x3 <- mat[,3]/fx[3]
# Probabilidades condicionales de Y dado X=3
fy_x4 <- mat[,4]/fx[4]
# Probabilidades condicionales de Y dado X=4
fx_y2 <- mat[2,]/fy[2]
# Probabilidades condicionales de X dado Y=2
fx_y4 <- mat[4,]/fy[4]

###c
ex2 <- sum(x*fx(x))
ex2 -3


####b
x <- 0:1000
p <- 0.5
n <- 1000
ex <- sum(x*dbinom(x,n,p))
ex_10 <- ex + ex*0.1 
ex_10
vx <- sum((x^2)*dbinom(x,n,p))-ex^2
vx/(ex_10-ex)^2
#### 8
a <- 1
x <- c(-a, 0, a)
p <- 0.5
N <- 10000
result <-replicate(N, sample(x,1,TRUE,prob=c(p,(1-2*p),p)))
abs(mean(result >= a)-2*p)
mean(result >= a)
### 9

mu <- 75.2
sigma <- sqrt(8.5*(1))
#a 
pnorm(77.4,mu,sigma)-pnorm(73.0,mu,sigma)
#b
1-pnorm(80,mu,sigma)


#### 10
##a
N <- 1000000
data <- rnorm(N)

mean(data <= 3) - mean(data <= -3)
pnorm(3,0,10)-pnorm(-3)
##b
mu <- 3
sigma <- 12
amplitudes <- (1:20000)/20000

for (x in amplitudes){
  print(round(pnorm(mu+sigma*x, mu, sigma)-pnorm(mu-sigma*x,mu,sigma),1))
  if (round(pnorm(mu+sigma*x, mu, sigma)-pnorm(mu-sigma*x,mu,sigma),1) == 0.5){ amplitud = x;break}
}
amplitud

### 11
mu = 86
sigma = 5
mu_10 = mu*0.1
mu-mu_10
mu+mu_10
1-(pnorm(mu+mu_10,mu,sigma)-pnorm(mu-mu_10,mu,sigma))
###d
a = qnorm(0.95,mu,sigma)-mu
pnorm(mu+a, mu,sigma)-pnorm(mu-a, mu,sigma)

### 12
mu = 5.13
sigma = 0.08

##a 
pnorm(5, mu,sigma)
### b
n = 12
p = 1-pnorm(5, mu,sigma)
1-pbinom(9,n,p)
###c
pnorm(5, mu,sigma)
new_mu = 5 + ((mu - qnorm(0.01,mu,sigma))/sigma)*sigma
a = new_mu - mu
pnorm(5, new_mu, sigma)
a

### 13
## c
n = 100
mu = 6
sigma = sqrt(1.5)
sigma_mu <- sigma/sqrt(25)
qt(0.975,24)
qnorm(0.975)
((qt(0.975,24)*sigma)/0.05)^2
N <- 100000
n <- 2556
data <- matrix(rnorm(N*n, mu, sigma),ncol=n)
mean_data <- apply(data, 1, mean)
mean(5.95 <= mean_data & mean_data <= 6.05)

### 14
mu = 3
sigma = sqrt(2.4)
n = (sigma/0.01)^2
N <- 100
data <- matrix(rnorm(N*n, mu,sigma),ncol=n)
data_mean <- apply(data, 1, mean)
sd(data_mean)
### 15
mu = 157.92
sigma = 30.20
n = 75
mu_media = mu
sigma_media = sigma/sqrt(n)
1-pnorm(170,mu_media, sigma_media)
N <- 10000
data <- matrix(rnorm(N*n, mu, sigma), ncol=n)
data_mean <- apply(data, 1, mean)
mean(data_mean >= 170)
### 16
mu = 4675
sigma = 345
mu_suma = 40*mu
sigma_suma =sigma* 40^2 
result = qnorm(0.99, mu_suma, sigma_suma)
N <- 10000000
n <- 40
data <- matrix(rnorm(N*n,mu,sigma),ncol=n)
data_sum <- apply(data, 1, sum)
mean(data_sum <= 192076)

##17
n = 150
p = 0.001
1-pbinom(0,n,p)
### 18
n = 1000
p = 0.4
pbinom(420,n,p)-pbinom(380,n,p)
sum(((0:1000)^2)*dbinom(0:1000, n, p)) - 400^2
### 19
n = 100
mu = 15
sigma = 10
mu_suma = 15*n
sigma_suma = sqrt(n)*sigma
1-pnorm(1700, mu_suma,sigma_suma)
#Distribución normal
N <- 10000
data <- matrix(rnorm(N*n, mu, sigma), ncol=n)
data_suma <- apply(data, 1, sum)
mean(data_suma > 1700)
#Distribución gamma
alpha = mu^2 / (sigma)^2
beta = alpha/mu
N <- 10000
data <- matrix(rgamma(N*n, alpha, beta), ncol=n)
data_suma <- apply(data, 1, sum)
mean(data_suma > 1700)

lambda = 1/mu
N <- 100000
data <- matrix(rexp(N*n, lambda), ncol=n)
data_suma <- apply(data, 1, sum)
mean(data_suma > 1700)
### 20
n = 25
p = 0.3
1-pbinom(9, n, p)
### 21
n = (sqrt(1.44)/0.5)^2
n
### 22
p <- 0.2
N <- c(1,10,20,30,1000,10000,100000,1000000)
cbind(sapply(N, function(x){
   mean(replicate(x, {
    rbinom(1,1,p)
  }))
}),N)
p <- 0.2
N <- c(1,10,20,30,1000,10000,100000,1000000)
cbind(sapply(N, function(x){
  mean(replicate(x, {
    rpois(1,5)
  }) < 4)
}),N)
ppois(3, 5)
### 23
n <- seq(1, 1000, 20)
N <- 10000
result <- sapply(n, function(x){
  
})
### 26
N <- 10000
x <- rnorm(N, 2, 0.1)
y <- exp(x)
ey = mean(y)
y2 <- y^2
ey2 <- mean(y2)
vy <- ey2-ey^2
sdy <- sqrt(vy)

ic_vy <- c( ((N-1)*vy)/qchisq(0.975, N-1), ((N-1)*vy)/qchisq(0.025, N-1) )
ncent <- (4*(sdy/0.01))^2
ncent

wrap <- function(x){
  exp(x)*dnorm(x,2,0.1)
}
curve(wrap(x),-10,10)

result = replicate(10000, {
N <- ncent
x <- rnorm(N, 2, 0.1)
y <- exp(x)
media <- mean(y)

ey-0.005 < media & media < ey+0.005
})
N = 10000
data <- exp(matrix(rnorm(N*ncent, 2, 0.1), ncol=ncent))

mean(result)
result = replicate(1000, {
  N <- 85876.74+10
  x <- rnorm(N, 2, 0.1)
  y <- exp(x)
  media <- mean(y)
  
  
  IC <- mean(y) + c(-1, 1) * 2 * sd(y) / sqrt(N) # IC
  IC[2] - IC[1]
})
mean(result <= 0.01)


((sd(y)/0.01)*qt(0.975,999)*2) ^2
