### ejercicio 1 ###





dcustom <- function(z){
  fdensi <- function(x){
    if (x < 0){return(0)}
    if (0 <= x & x < 1/2){return(x*2)}
    if (1/2 <= x & x <= 1){return(6*(1-x))}
    if (x > 1){return(0)}
  }
  if (length(z) == 1){
    fdensi(z)
  }else{
    sapply(z,fdensi)
    
  }
}

integrate(dcustom,0,1)


fundistri <- function(x){
  if (length(x)==0){
  return(integrate(dcustom,0,x)$value)
  }else{
    return(sapply(x,function(x){
      integrate(dcustom,0,x)$value
    }))
  }
}
pcustom <-function(x){
  
fundistri_numeric_ <- function(x){
  if (x < 1/2){
    return((dcustom(x)*x)/2)
  }else{
    # [0,1/2)
    fi <- 1-((dcustom(1/2)*1/2)/2)
    # [1/2,x]
    fi2 <- ( ((dcustom(1/2)-dcustom(x))*(x-1/2))/2 + (dcustom(x)*(x-1/2) ))
    return(fi+fi2)     
  }
}
  if(length(x) == 1){
    fundistri_numeric_(x)
  }else{
    sapply(x,fundistri_numeric_)
  }
}

curve(dcustom(x),0,1/2)
curve(dcustom(x),1/2,1,add=T)
curve(pcustom(x),0,1)
curve(pcustom(x),0,1)
### la función de distribución debe de tener un área bajo la curva = 1, si se observa la gráfica de pcustom entre {0,1}, se verá que corresponde con dos triangulos rectangulos de base 1/2 y altura 
x = 0.49
pcustom(1)
## c
par(mfrow=c(2,2))
x <- (1:20000)/20000
y <- dcustom(x)
mayor <-1/3
menor <- 2/3
plot(x,y,type = "n")
lines(x[x < 1/2], y[x<1/2])
lines(x[x >= 1/2], y[x >= 1/2])
if (mayor < 1/2){
xx = x[which(x < 1/2 & mayor < x & x < menor)]
yy = y[which(x < 1/2 & mayor < x & x < menor)]

polygon(c(xx,xx[length(xx)],xx[1]),
        c(yy,0,0)-0.01,col = "coral",border=NA)
}
xx = x[which(x >= 1/2 & mayor < x & x < menor)]
yy = y[which(x >= 1/2 & mayor < x & x < menor)]
polygon(c(xx,xx[length(xx)],xx[1]),
        c(yy,0,0)-0.01,col = "coral",border=NA)

curve(pcustom(x),0,1)
arrows(mayor,0,mayor,pcustom(mayor),length = 0.1)
arrows(menor,0,menor,pcustom(menor),length = 0.1)
points(c(mayor,menor),c(pcustom(mayor),pcustom(menor)),col="coral",bg="coral",pch=16)


x <- (1:20000)/20000
y <- dcustom(x)
mayor <-3/4
menor <- 1
plot(x,y,type = "n")
lines(x[x < 1/2], y[x<1/2])
lines(x[x >= 1/2], y[x >= 1/2])
if (mayor < 1/2){
  xx = x[which(x < 1/2 & mayor < x & x < menor)]
  yy = y[which(x < 1/2 & mayor < x & x < menor)]
  
  polygon(c(xx,xx[length(xx)],xx[1]),
          c(yy,0,0)-0.01,col = "coral",border=NA)
}
xx = x[which(x >= 1/2 & mayor < x & x < menor)]
yy = y[which(x >= 1/2 & mayor < x & x < menor)]
polygon(c(xx,xx[length(xx)],xx[1]),
        c(yy,0,0)-0.01,col = "coral",border=NA)

curve(pcustom(x),0,1)
arrows(mayor,0,mayor,pcustom(mayor),length = 0.1)
arrows(menor,0,menor,pcustom(menor),length = 0.1)
points(c(mayor,menor),c(pcustom(mayor),pcustom(menor)),col="coral",bg="coral",pch=16)

### ejercicio 2

n <- 474
pM <- 0.498
mu_IMC <- 18.7
sigma_IMC <- 4.1
sobrepeso <- 21.8
desnutricion <- 14.1
mu_peso <- 35.1
sigma_peso <- 12.3

## a
1-pnorm(sobrepeso_IMC, mu_IMC, sigma_IMC)
pnorm(desnutricion_IMC, mu_IMC,sigma_IMC)



## b
pnorm(sobrepeso_IMC,mu_IMC,sigma_IMC) - pnorm(desnutricion_IMC,mu_IMC,sigma_IMC)
## c
qnorm(c(0.35,0.65),mu_IMC,sigma_IMC)
## d
qnorm(c(0.25,0.75),mu_IMC,sigma_IMC)
qnorm(0.75,mu_IMC,sigma_IMC)-qnorm(0.25,mu_IMC,sigma_IMC)
## e
p <- 1-pnorm(38, mu_peso, sigma_peso)
pbinom(3,21,p)

