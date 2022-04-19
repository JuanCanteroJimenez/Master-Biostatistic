#### ejercicio 1 ####

#dominio de la función
par(mfrow=c(1,1))
kk = 0.1
lambdaa = 1

f <- function(x,k=kk,lambda=lambdaa){
  (x^(k-1))*(lambda^k)*exp(-lambda*x)*(x > 0)+0*(x <= 0)
}
curve(f(x),-10,0,ylim=c(-10,10),col="blue",xlim=c(0,1))
curve(f(x),1/1e+10, 10,ylim=c(-10,10),col="blue",add=TRUE)
f(-90)
fd <- function(x,k=kk, lambda=lambdaa){
  exp(-lambda*x)*(x^(k-1))*(k*x^-1-x^-1-lambda)*lambda^k*(x >0)+0*(x <=0)
  }
curve(fd(x),-10,0,add=TRUE,col="red")
curve(fd(x),1/1e+10,10,add=TRUE,col="red")
abline(h=0,v=0,col="grey")

### concavidad
lambda = 2
k = 2
a = lambda^2
b = 2*lambda-2*lambda*k
c = k^2 -3*k + 2
-b + c(-1,1)*( sqrt(b^2 - 4*a*c)/(2*a))

### representación gráfica
par(mfrow=c(2,2))
kk = 0.5
lambdaa = 1

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
x = (-10000:10000)/1000
yminn <-c( min(f(x),na.rm=TRUE),max(f(x),na.rm=TRUE))-c(0.1,0)
curve(f(x),-10,0,col="blue",xlim=c(-10,10),ylim=yminn)
curve(f(x),1/1e+10, 10,col="blue",add=TRUE,ylim=yminn)
abline(h=0, v=0, col = "grey")

kk = 0.5
lambdaa = 1

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
x = (-10000:10000)/1000
yminn <-c( min(f(x),na.rm=TRUE),max(f(x),na.rm=TRUE))-c(0.1,0)
curve(f(x),-10,0,col="blue",xlim=c(-10,10),ylim=yminn)
curve(f(x),1/1e+10, 10,col="blue",add=TRUE,ylim=yminn)
abline(h=0, v=0, col = "grey")

kk = 5
lambdaa = 2

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
x = (-10000:10000)/1000
yminn <-c( min(f(x),na.rm=TRUE),max(f(x),na.rm=TRUE))-c(0.1,0)
curve(f(x),-10,0,col="blue",xlim=c(-10,10),ylim=yminn)
curve(f(x),1/1e+10, 10,col="blue",add=TRUE,ylim=yminn)
abline(h=0, v=0, col = "grey")
kk = 9
lambdaa = 0.5

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
x = (-10000:10000)/1000
yminn <-c( min(f(x),na.rm=TRUE),max(f(x),na.rm=TRUE))-c(0.1,0)
curve(f(x),-10,0,col="blue",xlim=c(-10,10),ylim=yminn)
curve(f(x),1/1e+10, 10,col="blue",add=TRUE,ylim=yminn)
abline(h=0, v=0, col = "grey")

#### ejercicio 1 integración

kk = 0.5
lambdaa = 1

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
integrate(f,0,Inf)

kk = 1
lambdaa = 1

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
integrate(f,0,Inf)


kk = 5
lambdaa = 2

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
integrate(f,0,Inf)


kk = 9
lambdaa = 0.5

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
integrate(f,0,Inf)


#### ejercicio 2a ####
##1
n = 1:7
factorial(n-1) == gamma(n)
##2
fi <- function(x,k=1,lambda=1){
  (x^(k-1))*(lambda^k)*exp(-lambda*x)
}
dummy=sapply(n, function(n){
    cat("\n")
    cat("n=",n,"\n",sep="")
    j= integrate(fi, 0, Inf, k=n, lambda=1)$value
    cat("##",
       "integrate(fi, 0, Inf, k=n, lambda=1)$value \n",
       j,"\n",
       "##",
       "gamma(n) \n",
       gamma(n), "\n",sep="")
  #integrate(fi, 0, Inf, k=n, lambda=1)$value
  #gamma(n)
    cat("\n")
})
#### ejercicio 2b ####

kk = 0.5
lambdaa = 1

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
## 1a
integrate(f,0,1)$value*(1/gamma(kk))
pgamma(1, kk, lambdaa)
## 1b
integrate(f,0,0)$value*(1/gamma(kk))
pgamma(0, kk, lambdaa)
## 1c
integrate(f,0,kk)$value*(1/gamma(kk))
pgamma(kk, kk, lambdaa)
## 2
integrate(f,0,2*kk)$value*(1/gamma(kk))-integrate(f,0,0)$value*(1/gamma(kk))
pgamma(2*kk, kk, lambdaa)-pgamma(0, kk, lambdaa)
## 3
integrate(f,0,Inf)$value*(1/gamma(kk))
integrate(dgamma, 0, Inf, shape=kk, rate=lambdaa)


kk = 1
lambdaa = 1

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
## 1a
integrate(f,0,1)$value*(1/gamma(kk))
pgamma(1, kk, lambdaa)
## 1b
integrate(f,0,0)$value*(1/gamma(kk))
pgamma(0, kk, lambdaa)
## 1c
integrate(f,0,kk)$value*(1/gamma(kk))
pgamma(kk, kk, lambdaa)
## 2
integrate(f,0,2*kk)$value*(1/gamma(kk))-integrate(f,0,0)$value*(1/gamma(kk))
pgamma(2*kk, kk, lambdaa)-pgamma(0, kk, lambdaa)
## 3
integrate(f,0,Inf)$value*(1/gamma(kk))
integrate(dgamma, 0, Inf, shape=kk, rate=lambdaa)


kk = 5
lambdaa = 2

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
## 1a
integrate(f,0,1)$value*(1/gamma(kk))
pgamma(1, kk, lambdaa)
## 1b
integrate(f,0,0)$value*(1/gamma(kk))
pgamma(0, kk, lambdaa)
## 1c
integrate(f,0,kk)$value*(1/gamma(kk))
pgamma(kk, kk, lambdaa)
## 2
integrate(f,0,2*kk)$value*(1/gamma(kk))-integrate(f,0,0)$value*(1/gamma(kk))
pgamma(2*kk, kk, lambdaa)-pgamma(0, kk, lambdaa)
## 3
integrate(f,0,Inf)$value*(1/gamma(kk))
integrate(dgamma, 0, Inf, shape=kk, rate=lambdaa)


kk = 9
lambdaa = 0.5

f <- function(x,k=kk,lambda=lambdaa){
  wrap <- function(x){
    if (x >0){(x^(k-1))*(lambda^k)*exp(-lambda*x)}else{0}
  }
  if (length(x) > 1){
    sapply(x, wrap)
  }else(wrap(x))
}
## 1a
integrate(f,0,1)$value*(1/gamma(kk))
pgamma(1, kk, lambdaa)
## 1b
integrate(f,0,0)$value*(1/gamma(kk))
pgamma(0, kk, lambdaa)
## 1c
integrate(f,0,kk)$value*(1/gamma(kk))
pgamma(kk, kk, lambdaa)
## 2
integrate(f,0,2*kk)$value*(1/gamma(kk))-integrate(f,0,0)$value*(1/gamma(kk))
pgamma(2*kk, kk, lambdaa)-pgamma(0, kk, lambdaa)
## 3
integrate(f,0,Inf)$value*(1/gamma(kk))
integrate(dgamma, 0, Inf, shape=kk, rate=lambdaa)

####3a
par(mfrow=c(1,1))
L = 1000
P <- function(t, L, c, a){
  L/(1+c*exp(a*t))
}
t = c(0,1,2,3,4)
pt = c(200,400,650,850,950)
## 1
y = log((L/pt)-1)
plot(t,y)
plot(t, pt)
## 2
tabla <- NULL
tabla <- rbind(t,y)
rownames(tabla) <- c("t", "y")
tabla
## 3
co = lsfit(tabla[1,], tabla[2,])$coefficients
t_ = seq(0, 4, length.out=50)
y_ = t_*co[2] + co[1]
plot(t,y)
lines(t_, y_, col="red")
## 4
t_ = seq(0, 4, length.out=50)
p_ = P(t_, L, exp(co[1]), co[2])
plot(t,pt)
lines(t_, p_, col="red")

