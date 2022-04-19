A <- 10

fx <- function(x){1/(2+2*x)}
curve(fx(x), -10,10)
curve(Fx(x), -10,10,add = TRUE)
#6
integrate(fx,0,4)
integrate(fx,0,Inf)
Fx <- function(x){log(2+2*x)/2}
integrate(fx,0,4)
Fx(4)-Fx(0)
curve(Fx, 0,1000000000000000000000000000)
