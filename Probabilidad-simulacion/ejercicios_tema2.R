### 1 ###

lambda = 3.2
ppois(1,lambda)
ppois(0.99,lambda)
ppois(3.2, lambda)
ppois(-4.5, lambda)


### 2 ###

n = 10
pii = 0.2

barplot(pbinom(-2:10,n,pii))
barplot(dbinom(-2:10,n,pii))
pbinom(c(2,3.7,14.2),n,pii)
qbinom(c(0.25,0.50,0.75),n,pii)
hist(qbinom(runif(100000),n,pii))

### 3 ### 

mu = 5
sigma = 2
curve(pnorm(x,mu,sigma),0,20)
curve(dnorm(x,mu,sigma),-10,15)
pnorm(2, mu,sigma)
integrate(dnorm,-Inf,2,mean=mu,sd=sigma)

### 4 ###

# p(2 < x < 5)
pnorm(5,3,1)-pnorm(2,3,1)
pgamma(5,6,2)-pgamma(2,6,2)
ppois(4,3)-ppois(2,3)
dpois(4,3)+dpois(3,3)
# p(x <= 4)
pnorm(4,3,1)
pgamma(4,6,2)
ppois(4,3)
# p(x < 4)
pnorm(4,3,1)
pgamma(4,6,2)
ppois(3,3)
#p(X > 5)
1-pnorm(5,3,1)
1-pgamma(5,6,2)
1-ppois(5,6,2)

### 5 ###

pcustom <- function(x){
  wrap <- function(x){
    if (as.integer(x)==x){1-(1/2)^x}else{0}
  }
  if (length(x)==1){
    wrap(x)
  }else{
    sapply(x,wrap)
  }
}

dcustom <- function(x){
  wrap <- function(x){
    if (as.integer(x)==x){(log(1/2)*-(1/2)^x)}else{0}
  }
  if (length(x)==1){
    
    return(wrap(x))
  }else{
    sapply(x,wrap)
  }
}

dcustom2 <- function(x){
  return( (((2^(x+1))-(2^(x-1)))/(4^x))*(x > 1)+(1-(1/2)^x)*(x == 1))
}
dcustom3 <- function(x){
  return( (pcustom(x)-pcustom(x-1)) )
}

barplot(dcustom3(1:10))
barplot(pcustom(1:10))
dcustom(3)
pcustom(3)
dcustom(1:10)
m <- matrix(1:1000000, ncol = 1)
m <- cbind(m,apply(m, 1, dpois,lambda=3))
sum(m[,2])
integrate(dcustom3,0,Inf)

### 6 ### 
p = 1-(4/20)
p
#p(x >= 8)
1-pbinom(7,10,p)

### 7 ### hecho

### 8 ###
#a
(pexp(200, 0.005)-pexp(150,0.005))/(1-pexp(150,0.005))
#b

### 9 ###

for (sigma in c(0.25,1,4,16)){
  print(pnorm(sigma, 0, sigma)-pnorm(-sigma, 0, sigma))
  print(pnorm(sigma*2, 0, sigma )-pnorm(-sigma*2, 0, sigma))
  print(pnorm(sigma*3, 0, sigma)-pnorm(-sigma*2, 0, sigma))
}

### 10 y 11 hechos en clase ###

### 12 ### 
pnorm(sqrt(3))-pnorm(-sqrt(3))
pchisq(3,1)

### 13 ###

qcustom <- function(x){
  1*(0 < x & x <= 0.25)+2*(0.25 < x & x<= 0.50 )+3*(0.50 < x & x <= 0.75)+4*(0.75 < x & x <= 1)
  
}
pcustom <- function(x){
  0.25*(x>=1)+0.25*(x>=2)+0.25*(x>=3)+0.25*(x>=4)
}
pcustom(3.2)
curve(pcustom(x),0,4)
N <- 10000
table(qcustom(runif(N)))/N

### 14 ###
qcustom2 <- function(x){
  1*(0 < x & x <= 0.1)+2*(0.1 < x & x<= 0.3 )+3*(0.3 < x & x <= 0.6)+4*(0.6 < x & x <= 1)
}
curve(qcustom2(x))
### 15 ###
#x = tan(π(u − 0.5))

qinvers <- function(x){
  return(tan(pi*(x-0.5)))
}
x <- qinvers(runif(10000))
x <- x[abs(x)<50]
plot(density(x))
curve(dcauchy(x),-10,10, add=TRUE,col="red")

### 16 ###

dlogistic <- function(t){
  return(  exp(-t)/(1+exp(-t))^2  )
}
curve(dlogistic(x),-10,10)
qlogistic <- function(u){
  log(u/(1-u))
}
curve(qlogistic(x),0,1)
curve(qlogis(x),0,1)
hist(qlogistic(runif(10000)))
hist(qlogis(runif(10000)))

### 17 ###

curve(dunif(x),0,1)
curve(punif(x),0,1)
### 18 ### 

### 19 ###
dpois(2,1.8)*dbinom(2,2,0.49)
### 20 ###
# M = 3

dcondy <- function(x,m){(dbinom(m,5,0.5)*dbinom(x,5,0.8))/dbinom(m,5,0.5)}
barplot(dcondy(0:3,3))
dcondy(0:3,3)
barplot(dbinom(0:3,3,0.8))
#
par(mfrow=c(1,1))
dcustom <- function(x,y){dbinom(x,5,0.5)*dbinom(y,x,0.8)}
matt <- matrix(1:36, nrow=6,ncol=6)
image(matt,useRaster =TRUE,axes=FALSE)
for (x in 0:5){
  for (y in 0:5){
    matt[x+1,y+1]<- dcustom(x,y)
  }
}
filas <- apply(matt,1,sum)
columnas <- apply(matt, 2, sum)
image(matt/max(matt),oldstyle = TRUE)
image(t(t(mat)))

### 21 ###

rcirculo <- function(n){
  x_result <- c()
  y_result <- c()
  while(length(x_result) <= n ){
  x <- runif(1,-1,1)
  y <- runif(1,-1,1)
  
  if((x^2)+(y^2) < 1){
    x_result <- c(x_result, x)
    y_result <- c(y_result, y)
    
  }
  }
  return(cbind(x=x_result, y=y_result))
}
plot(rcirculo(1000))

### 22 ###

rsemicirculo <- function(n){
  x_result <- c()
  y_result <- c()
  while(length(x_result) <= n ){
    x <- runif(1, -1,1)
    y <- runif(1,1,1)
    
    if((((x-1/2)^2)+(y^2) < 0.5^2) & (x != 1) & (x != 0) & (y != 1) & (y != -1)){
      x_result <- c(x_result, x)
      y_result <- c(y_result, y)
      
    }
  }
  return(cbind(x=x_result, y=y_result))
}

set.seed(1)
data <- rsemicirculo(10000)
plot(data,asp=1)
abline(h=0)
abline(v= 0)
cauch <- apply(data, 1, function(x){
  x["y"]/x["x"]
})

max(cauch)
min(cauch)
hist(cauch)
plot(density(cauch))
curve(dcauchy(x),-1,1,add=TRUE,col="red")

### 23 ### 





mu_1 = 10
mu_2 = 5
sigma_1 = 2
sigma_2 = 1
rho = 0.5
x <- rnorm(10000,mu_1, sigma_1)
y <- sapply(x,FUN = function(x){
  rnorm(1, mean= (  mu_2+rho*(sigma_2/sigma_1)*(x-mu_1) ), sigma_2*sqrt(1-rho^2)  ) 
})
plot(x,y,asp=1)
xx <- x
yy <- y
curve(dnorm(x,5,1),add=TRUE)
plot(density(x))
curve(dnorm(x,10,2),add=TRUE,col="red")
dxy <- function(x,y){
   dnorm(x,mu_1, sigma_1)*dnorm(y,mean= (  mu_2+rho*(sigma_2/sigma_1)*(x-mu_1) ), sigma_2*sqrt(1-rho^2) )
}

dxy(10,5)
dxy(2,2)
xs <- seq(min(xx),max(xx),length.out=1000)
ys <- seq(min(yy),max(yy),length.out=1000)
dxymat <- matrix(rep(0,length(ys)*length(xs)), ncol=length(ys),nrow=length(xs))
for (x in 1:length(xs)){
  for (y in 1:length(ys)){
    dxymat[x,y] <- dxy(xs[x],ys[y])
    
}
}
colnames(dxymat) <- xs
rownames(dxymat) <- ys
par(mfrow=c(1,2))
image(dxymat)
plot(xx,yy,pch=16, col=rgb(1,0,1,alpha=0.010))
#axis(1, at=xs,labels=xs)
#axis(2, at=ys,labels=ys)
library(plotly)
p <- plot_ly(z = dxymat, type = "surface")
p
### 24 ###
##e (X, Y ) es Normal-Gamma con parámetros µ = 110, α = 15 y β = 12; esto e
alfa = 15
beta = 12
mu = 0
set.seed(123)
y <- rgamma(10000,alfa, beta )
x <- sapply(y, function(x){
  rnorm(1,mu, 1/sqrt(x) )
})
par(mfrow=c(1,1))
plot(density(x))
curve(dt(x,30),-3,3,col="red",add=TRUE)
### 25 ###

curve(dbeta(x,4,2))
curve(dunif(x))

c <- abs(max(dbeta((1:1000)/1000, 4, 2)/dunif((1:1000)/1000)))

rbetaa <- function(i,c){
  x <- c()
  y <- c()
  z <- 0
  while (length(x) <= i){
    z <- z+1
    number <- runif(1)
    hc <- runif(1,0,dunif(number)*c)
    if (hc <= dbeta(number,4,2) ){
      x <- c(x, number)
      y <- c(y, hc )
    }
  }
return(list(values=cbind(x=x,y=y),intentos=z))
  }
plot(rbetaa(1000,c)$values)
curve(dbeta(x,4,2),col="red")
curve(dunif(x)*c,add=TRUE)
curve(dbeta(x,4,2)/(dunif(x)*c))
fun <- function(x){dbeta(x,4,2)/(dunif(x)*c)}
integrate(fun,0,1)


rbetaa_efi <- function(i,c){
  x <- c()
  y <- c()
  z <- 0
  while (length(x) <= i){
    z <- z+1
    number <- runif(1)
    hc <- runif(1,0,dunif(number)*c)
    if (hc <= dbeta(number,4,2) ){
      x <- c(x, number)
      y <- c(y, hc )
    }
  }
  return(i/z)
}
efis <- replicate(100,rbetaa_efi(1000,c))
p <- integrate(fun,0,1)$value
rgeom(1000,p)
### version del profesor ###
M <- dbeta(0.75,4,2)
N <- 10000
x <- runif(2.1*N)
y <- runif(2.1*N,0,M)
xx <- x[y<dbeta(x,4,2)]
xx <- x[1:N]
length(xx)
plot(density(xx))
### 26 ###
f <- function(x){
wrap <- function(x){
  if(x > 0 & x < 20){return( (x^4)*(5+x)^-10  )}else{return(0)}
}
if (length(x) == 1){return(wrap(x))}else{return(sapply(x,wrap))}
}

curve(dgamma(x,2,0.3),0,20,col="red")
curve(f(x),0,20,add=TRUE)
cc <- max(f((1:190)/10)/dgamma((1:190)/10,2,0.3))

curve(dgamma(x,2,0.3)*cc,0,20,col="red")
curve(f(x),0,20,add=TRUE)



wrap <- function(x){dgamma(x,2,0.3)*cc}
efi<-integrate(f,0,20)$value/integrate(wrap,0,20)$value
print(efi)
rcustom <- function(i,cc,efi=FALSE){
  x <- c()
  y <- c()
  z <- 0
  while(length(x)< i){
    z <- z+1
   
    number <- rgamma(1,2,0.3)
    
    y_ <- runif(1,0,(dgamma(number,2,0.3)*cc))
    if(y_ < f(number)){
      x <- c(x, number)
      y <- c(y,y_ )
    }
  }
  
  if(efi){i/z}else{return(cbind(x=x,y=y))}
}

rcustom_grafic <- function(i,cc,efi=FALSE,efi_=efi){
  x <- c()
  x_recha <- c()
  y <- c()
  y_recha <- c()
  z <- 0
  while(length(x)< i){
    z <- z+1
    curve(dgamma(x,2,0.3)*cc,0,20,col="red")
    curve(f(x),0,20,add=TRUE)
    points(x,y,col="blue")
    points(x_recha,y_recha,col="red")
    number <- rgamma(1,2,0.3)
    
    points(number,0,col="green")
    text(number,0,"rgamma(1,2,0.3)",cex=0.5)
    text(15,6e-08,paste("Aceptados ",
                        length(x),
                        "\n",
                        "Rechazados ",
                        length(x_recha),
                        "\n",
                        "Eficiencia ",
                        length(x)/(length(x_recha)+length(x)),
                        "\n",
                        "Eficiencia Teorica ",
                        efi_),cex=0.6)
    Sys.sleep(1)
    y_ <- runif(1,0,(dgamma(number,2,0.3)*cc))
    
    if(y_ < f(number)){
      x <- c(x, number)
      y <- c(y,y_ )
      points(number,y_,col="blue")
      text(number,y_,"runif(1,0,(dgamma(number,2,0.3)*cc))",col="blue",cex=0.5)
      arrows(number,0,number,y_,col="blue")
      
      Sys.sleep(1)
    }else{
      x_recha <- c(x_recha,number)
      y_recha <- c(y_recha,y_)
      points(number,y_,col="red")
      text(number,y_,"runif(1,0,(dgamma(number,2,0.3)*cc))",col="red",cex=0.5)
      arrows(number,0,number,y_,col="red")
      
      Sys.sleep(1)
    }
  }
  
  if(efi){i/z}else{return(cbind(x=x,y=y))}
}
rcustom_grafic(1000,cc,efi_=efi)

rcustom(10,cc)
plot(density(rcustom(1000,cc)[,"x"]))

curve(f(x)/(dgamma(x,2,0.3)*cc),0,20)
curve(f(x),0,20,col="red",add=FALSE)
points(rcustom(1000,cc))

rcustom(100000,cc,TRUE)


### segundo intento con la uniforme ###
cc <- max(f((1:200)/10)/dunif((1:200)/10,0,20))
curve(f(x),0,20)
curve(dunif(x,0,20)*cc,0,20,add=TRUE)
rcustom2 <- function(i,cc){
  x <- c()
  y <- c()
  z <- 0
  while (length(x) <= i){
    z <- z+1
    number <- runif(1, 0,20)
    hc <- runif(1,0,dunif(number,0,20)*cc)
    if (hc <= f(number) ){
      x <- c(x, number)
      y <- c(y, hc )
    }
  }
  return(list(values=cbind(x=x,y=y),intentos=z))
}
rcustom2(1000,cc)
plot(density(rcustom2(1000,cc)$values[,"x"]))

curve(f(x)/(dgamma(x,2,0.3)*cc),0,20)
curve(f(x),0,20,col="red",add=FALSE)
points(rcustom2(1000,cc)$value)
plot(density(rcustom2(10000,cc)$values[,"x"]))
hist(rcustom2(1000,cc)$values[,"x"])/1000
hist(rnorm(1000))
### 27 ###

rcirculo <- function(n){
  x_result <- c()
  y_result <- c()
  while(length(x_result) <= n ){
    x <- runif(1,-1,1)
    y <- runif(1,-1,1)
    
    if((x^2)+(y^2) < 1){
      x_result <- c(x_result, x)
      y_result <- c(y_result, y)
      
    }
  }
  return(cbind(x=x_result, y=y_result))
}
rcirculo(10)
dat <- t(apply(rcirculo(10000),1,function(x){
  s = (x["x"]^2) + (x["y"]^2) 
  
  x1 <- x["x"]*sqrt((-2*log(s))/s)
  x2 <- x["y"]*sqrt((-2*log(s))/s)
  c(x1,x2)
}))
plot(density(dat[,"x"]))
curve(dnorm(x),add=TRUE,col="red")


### 29 ###
set.seed(123)
alfa = 15
beta = 12
mu = 110
set.seed(123)
y <- rgamma(1000000,alfa, beta )
x <- sapply(y, function(x){
  rnorm(1,mu, 1/sqrt(x) )
})
par(mfrow=c(1,1))
plot(density(x))

tau <- sqrt(1/y)/x
plot(density(tau))
curve(dt(x,30),-3,3,col="red",add=TRUE)
mean(tau)
sd(tau)
mean(tau<0.007)

### ejercicio 4 tutoria ###

m <- 10
sig <- 2
n <- 20
N <- 1000000
mat <- matrix(rnorm(n*N,m,sig),ncol=n)
medias <- apply(mat, 1,mean)
des_tip <- apply(mat, 1, sd)
fun_ic <- function(x,n){
  x[1]+c(-1,1)*qt(0.975,n-1)*x[2]/sqrt(n)
}
ic <- apply(cbind(medias,des_tip),1,fun_ic,n=n)
mean(ic[1,]< m & ic[2,] > m)

### 31 ###

lambda1 = 3
lambda2 = 4

ppois(0,lambda1)*ppois(2,lambda2)+ppois(1,lambda1)*ppois(1,lambda2)+ppois(2,lambda1)*ppois(0,lambda2)

### 32 ### 
#a
dunu <- function(x,lambda){
  y <- x
  dexp(x,lambda)*pexp(y,lambda)+pexp(x,lambda)*dexp(y,lambda)
}

curve(dunu(x,0.5),0,100)
integrate(dunu,lambda=0.5,0,Inf)
n <- 1000000
lambda <- 0.3
u <- apply(cbind(rexp(n,lambda),rexp(n,lambda)),1,max)
plot(density(u))
curve(dunu(x,lambda),add=TRUE,col="red")

#b
n <- 10
j <- 1:n
prob <- matrix(rep(0,n*n),ncol=n)
for (x in j){
  for (y in j){
    prob[x,y]<-max(x,y)
  }
}
image(prob)
dv <- function(x,lambda){
  y <-x
  -(  -dexp(x,lambda)*(1-pexp(y,lambda))-dexp(y,lambda)*(1-pexp(x,lambda)) )
}
n <- 10000000
lambda <- 0.7

integrate(dv, 0,Inf, lambda)
v <- apply(cbind(rexp(n,lambda),rexp(n,lambda)),1,min)

curve(dv(x,lambda),add=FALSE,col="red",0,20)
points(density(v))
integrate(dv,0,0.1,lambda)
mean(v<=0.1)

### 33 ###

n = 15
N = 1000000
mu = 100
mu_0 = 100
sigma = 5
matt <- matrix(rnorm(n*N, mu, sigma),ncol=n)
tvalues <- apply(matt, 1, function(x,n,mu_0){
  (sqrt(n)/sd(x))*(mean(x)-mu_0)
},n=n,mu_0=mu_0)
plot(density(tvalues))
curve(dt(x,14),add=TRUE,col="red")
abline(v=3.27)
(1-mean(tvalues<=3.27))*2
(1-pt(3.27,14))*2
### 34 ###
m <- 100
sig <- 5
n <- 15
N <- 1000000
mat <- matrix(rnorm(n*N,m,sig),ncol=n)
medias <- apply(mat, 1,mean)
des_tip <- apply(mat, 1, sd)
fun_ic <- function(x,n){
  x[1]+c(-1,1)*qt(0.975,n-1)*x[2]/sqrt(n)
}
ic <- apply(cbind(medias,des_tip),1,fun_ic,n=n)
mean(ic[1,]< m & ic[2,] > m)

### 35 ###
p = 0.2
p_0 = 0.2
n = 400
N = 1000000
zvalues <- t(replicate(N,{
  r<-rbinom(1,n,p)
  SE <- sqrt(((r/n)*(1-(r/n)))/n)
  z <-((r/n)-p_0)/sqrt((p_0*(1-p_0))/n)
  inf <- (r/n)+SE*qnorm((1-0.9)/2)
  sup <- (r/n)-SE*qnorm((1-0.9)/2)
  c(z=z,SE=SE,inf=inf,sup=sup)
}))
plot(density(zvalues[,"z"]))
mean(zvalues[,"inf"] < p_0 & zvalues[,"sup"] > p_0  )

### 36 ###
N = 100000
p = 0.5
 w_value<- replicate(N, {

x <- 1:15
 sum(sapply(x, function(x){
  if(rbinom(1, 1, p)){x}else{x*-1}
}))})
plot(density(w_value))
cumsum(table(w_value)/length(w_value))

#### 28 ####
cov <- matrix(c(4,1,
                1,1),byrow=TRUE,ncol=2,nrow=2)
cov_chol <- chol(cov)
data <- matrix(c(rnorm(100000,10,2),rnorm(100000,5,1)),nrow=2,ncol=100000)
result <- cov_chol%*%data
plot(result[1,],result[2,])

     
mu = 4
sigma = 10

pnorm(mu+abs(qnorm(0.25))*sigma, mu,sigma)-pnorm(mu-abs(qnorm(0.25))*sigma, mu,sigma)

mu = 0.15
sigma = sqrt(150*0.001*(1-0.001))
pnorm(0.5,mu,sigma)-pnorm(-0.5,mu,sigma)
1-dbinom(0,150,0.001)
