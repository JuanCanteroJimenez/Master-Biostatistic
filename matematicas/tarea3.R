MSR <- function(y,y_hat){
  if (length(y)==length(y_hat)){
    return(mean((y-y_hat)^2))
  }
}#Se define una función para calcular el error cuadrático medio
EA <- function(y,y_hat){
  sum(abs(y-y_hat))
}#se define una función para calcular el error absoluto en el intervalo
####a####
D_def_aprox <- function(i){
  
  f <- function(x){ (x^2)/x }
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ rep(1,length(x))}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")


#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))

####b####
D_def_aprox <- function(i){
  
  f <- function(x){ ( ((x^3)+2*x+1)/(x+1)  )}
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ ((2*x^3)+(3*x^2)+1)/((x+1)^2)}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")

#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))
####c####
D_def_aprox <- function(i){
  
  f <- function(x){ log(x)*exp(x)}
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ exp(x)*((1/x)+log(x))}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")


#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))
####d####
D_def_aprox <- function(i){
  
  f <- function(x){ exp(sin(x))}
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ exp(sin(x))*cos(x)}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")
#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))
####e####
D_def_aprox <- function(i){
  
  f <- function(x){ log((x^2)+cos(x^2))}
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ ((2*x)*(1-sin(x^2)))/((x^2)+cos(x^2))}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")
#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))

####f####
D_def_aprox <- function(i){
  
  f <- function(x){ sin(x)*((x+1)^2)}
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ (x+1)*(cos(x)*(x+1)+sin(x)*2)}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")


#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))
####g####
D_def_aprox <- function(i){
  
  f <- function(x){sin(x^2)+cos(x^2) }
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ 2*x*(cos(x^2)+sin(x^2))}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")


#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))
####h####
D_def_aprox <- function(i){
  
  f <- function(x){(sin(x)^2)+(cos(x)^2)  }
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){ rep(0,length(x))}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")


#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))
####i####
D_def_aprox <- function(i){
  
  f <- function(x){log((x^2)+1)/((x^2)+1)  }
  
  
  stepp <- (1/100000)
  return((f(i+stepp)-f(i))/stepp)
}#Se define la función D_def que calcula la derivada dela función f definida
#internamente mediante la definición de derivada. 
x <- (1:200)/20
y <- round(D_def_aprox(x),5)

par(mfrow=c(1,3))


D_simb <- function(x){  (2*x*(1-log((x^2)+1)))/(x^2+1)^2}
#Se define la derivada obtenida de forma analítica.
plot(x,y,type="l",col="red")
title(main="D_def_aprox(x)")
curve(D_simb(x),0,10,col="blue",lty=2)
title(main="D_simb(x)")

plot(x,y,type="l",col="red")
curve(D_simb(x),0,10,col="blue",lty=2,add=TRUE)
title(main="D_def_aprox(x) y D_simb(x)")


#Se calcúla el error cuadrático medio. 
MSR(D_simb(x),D_def_aprox(x))
#Se calcúla el error absoluto en el intervalo 1,10
EA(D_simb(x),D_def_aprox(x))

