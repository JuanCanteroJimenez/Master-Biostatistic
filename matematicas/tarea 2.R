### ejercicio 1 ###
par(mfrow=c(1,2))
for (x in c(1,3)){
M <-x
l <- c(0, max(M^4, 3*M^2))

curve(3*x^2, -M, M, xlab="x", ylab="", ylim=l, col =1)
curve(x^4, -M, M, xlab="x", ylab="", ylim=l, col=2, add=T)

legend("bottomleft", c("3*x^2","x^4"), col=c(1, 2) , lty=c(1,1), lwd=5,cex=0.40, horiz = TRUE,inset=0 )
}


par(mfrow=c(1,2))
for (x in c(1,3)){
  M <-x
  l <- c(0, max(M^4, 30*M^2))
  
  curve(30*x^2, -M, M, xlab="x", ylab="", ylim=l, col =1)
  curve(x^4, -M, M, xlab="x", ylab="", ylim=l, col=2, add=T)
  
  legend("bottomleft", c("30*x^2","x^4"), col=c(1, 2) , lty=c(1,1), lwd=5,cex=0.40, horiz = TRUE,inset=0 )
}

### ejercicio 2 ###

par(mfrow=c(2,2))
for (x in c(1,3,5,8)){
  M <-x
  l <- c(0, max(exp(M), (M^3)+2*M+5))
  print(l)
  curve(exp(x), -M, M, xlab="x", ylab="", ylim=l, col =1)
  curve(x^3+2*x+5, -M, M, xlab="x", ylab="", ylim=l, col=2, add=T)
  
  legend("bottomleft", c("e^x","x^3+2*x+5"), col=c(1, 2) , lty=c(1,1), lwd=5,cex=0.40, horiz = TRUE,inset=0 )
}

par(mfrow=c(2,2))
for (x in c(1,3,5,8)){
  M <-x
  l <- c(0, max((M^2)+2*M+5, (M^3)+2*M+5))
  
  curve((x^2)+2*x+5, -M, M, xlab="x", ylab="", ylim=l, col =1)
  curve(x^3+2*x+5, -M, M, xlab="x", ylab="", ylim=l, col=2, add=T)
  
  legend("bottomleft", c("x^2+2*x+5","x^3+2*x+5"), col=c(1, 2) , lty=c(1,1), lwd=5,cex=0.40, horiz = TRUE,inset=0 )
}

### ejercicio 3 ###

par(mfrow=c(2,2))
for (x in c(1,3,5,8)){
  M <-x
  l <- c(0, max(log(M), log10(M),log(M,base=2)))
  
  curve(log(x), 0, M, xlab="x", ylab="", ylim=l, col =1)
  curve(log10(x), 0, M, xlab="x", ylab="", ylim=l, col=2, add=T)
  curve(log(x,base=2), 0, M, xlab="x", ylab="", ylim=l, col=3, add=T)
  legend("top", c("log(x)","log10(x)","log(x,base=2)"), col=c(1, 2,3) , lty=c(1,1,1), lwd=5,cex=0.40, horiz = TRUE,inset=0 )
  abline(h=0,col=gray(0.85))
  abline(v=1,col=gray(0.85))
}

### ejercicio 4 ###
par(mfrow=c(1,1))
M <-4*pi
l <- c(min(sin(1:(4*pi))), max(sin(1:(4*pi))))
curve(sin(x), 0, M, xlab="x", ylab="", ylim=l, col =1)

legend("bottomleft", c("sen(x)"), col=c(1) , lty=c(1) )

M <-4*pi
l <- c(min(3*sin( 2*(1:(4*pi)) )), max(3*sin( 2*(1:(4*pi)) )))
curve(3*sin(2*x), 0, M, xlab="x", ylab="", ylim=l, col =1)

legend("bottomleft", c("3*sen(2*x)"), col=c(1) , lty=c(1) )

M <-4*pi
l <- c(min(cos( 4*(1:(4*pi)) )), max(cos( 4*(1:(4*pi)) )))
curve(2*cos(4*x), 0, M, xlab="x", ylab="", ylim=l, col =1)

legend("bottomleft", c("sen(4*x)"), col=c(1) , lty=c(1) )

M <-4*pi
y <- sin(  (3.5*pi)+(3/4*(1:M)) )*-3.5
l <- c(min(y),max(y))
curve(sin((3/4)*x + 3.5*pi)*-3.5, 0, M, xlab="x", ylab="", ylim=l, col =1)

legend("bottomleft", c("sen(4*x)"), col=c(1) , lty=c(1) )

