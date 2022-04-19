
a <- 1
b <- 2

MM <- function(x,a=1,b=2){
  j <- (a*x)/(1+b*x)
  
  return(j)
  
}

## 1
par(mfrow=c(1,1))

ylim <- c(min(MM(-50:50)),max(MM(-50:50)))
curve(MM(x),-50,-0.5,xlim = c(-50,50), ylim=ylim)
curve(MM(x),-0.5,50,add=TRUE)
abline(h=0,v=0,col="gray")

### domino de la función
x <- seq(-5,5,length.out=100000000)
y <- MM(x)
no_dominio <- x[(y==Inf | y == -Inf | is.null(y) | is.na(y))]
no_domino
### b

D_def_aprox <- function(i){
  
  MM <- function(x,a=1,b=2){
    return((a*x)/(1+b*x))
  }
  
  
  stepp <- (1/100000)
  return((MM(i+stepp)-MM(i))/stepp)
}

x <- (-2000:2000)/200
x <- x[x != (-1/b)]
y <- round(D_def_aprox(x),5)
plot(x,y,type = "l")

puntos <- cbind(x,y)
puntos <- cbind(puntos,apply(puntos,1,function(x){
  
  if (x[2] > 0){"blue"}else{"red"}
}))

ylim <- c(min(MM(-15:15)),max(MM(-15:15)))
curve(MM(x),-15,-0.5,xlim = c(-15,15), ylim=ylim)
curve(MM(x),-0.5,15,add=TRUE)
lines(as.numeric(puntos[,1]),MM(as.numeric(puntos[,1])),col=puntos[,3],lty=2)

### c

DD_def_aprox <- function(z){
  
  D_def_aprox <- function(i){
    
    MM_log <- function(x,a=1,b=2){
      l <- (x*a)/(1+b*x)
      return(l)
    }
    
    
    stepp <- (1e-4)
    l <- (MM_log(i+stepp)-MM_log(i))/(stepp)
    return(l)
  }
  
  
  stepp <- (1e-4)
  l <- ((D_def_aprox(z+stepp)-D_def_aprox(z)))/(stepp)
  return(l)
}

x <- (-2000:2000)/20
x <- x[x != (-1/b)]
y <- DD_def_aprox(x)
plot(x,y,type = "l")

puntos <- cbind(x,y)
puntos <- cbind(puntos,apply(puntos,1,function(x){
  
  if (x[2] > 0){"blue"}else{"red"}
}))
par(mfrow=c(1,1))
ylim <- c(min(MM(-15:15)),max(MM(-15:15)))
curve(MM(x),-15,-0.5,xlim = c(-15,15), ylim=ylim)
curve(MM(x),-0.5,15,add=TRUE)
points(as.numeric(puntos[,1]),MM(as.numeric(puntos[,1])),col=puntos[,3],lty=2)

### d

interesting_points <- function(intervalos_inicales = 1000, intervalo =c(-5,5) ){
  MM <- function(x,a=1,b=2){
    j <- (a*x)/(1+b*x)
    
    return(j)
    
  }
}



#### 3 ####
xs <- matrix(c(9,3,1,
               2,1,0,
               1,1,1),byrow = TRUE, ncol=3,nrow=3)
ys <- c(0,0,-2)
result <- solve(xs,ys)

result
curve(result[1]*x^2+result[2]*x+result[3], -2, 4)
# f(3) = 0
points(3,0, col="blue")
# f'(1) = 0 y f(1) = -2
points(1, -2, col = "red")
#### 4 ####
x = c(0,
      0.3000,
      0.6000,
      0.9000,
      1.2000,
      1.5000,
      1.8000,
      2.1000,
      2.4000,
      2.7000,
      3.0000)
y = c(3.1009,
      2.2877,
      1.5185,
      1.1597,
      0.7645,
      0.6326,
      0.9010,
      1.1100,
      1.5550,
      2.2912,
      3.0275)
plot(x,y)
x_eq <- matrix(c(0,0,1,
                 2*1.5,1,0,
                 (15/10)^2,15/10,1),byrow = TRUE,ncol=3,nrow=3)
y_eq <- c(31009/10000,0,6326/10000)
abc<-solve(x_eq,y_eq)
plot(x,y)
curve(abc[1]*x^2+ abc[2]*x+abc[3],0,3,add=TRUE)

