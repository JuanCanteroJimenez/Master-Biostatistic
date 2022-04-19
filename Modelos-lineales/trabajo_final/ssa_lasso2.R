
###Analisis de las muestras

# original 

spectro(sheep, osc=T)

# ruido 

spectro(cabritus, osc=T, cont = F)

ssa_lasso_reg <- function(x,l){
  require(lars)
  require(Rssa)
  origin <- x
  
  
  s1 <- ssa(origin, L = l)
  
  
  recon1 <- reconstruct(s1, groups = lapply(1:l, c))
  
  print(length(recon1))
  #plot(recon1)
  vectores <-  lapply(recon1, function(x) as.vector(x))
  
  data_vectores_ori <- data.frame(vectores)
  #print(data_vectores_ori)
  idx<-order(abs(apply(data_vectores_ori, 2, function(x) cor(x, origin))), decreasing = T)
  #print(idx)
  vectores<- as.matrix(data.frame(data_vectores_ori[,idx[1:500]]))
  
  
  
  modelo.lasso <- lars(x = vectores, y = origin, type="lasso", normalize = F)
  print("ok")
  lasso.lambdas <- 10^seq(0, -1, length = 400)
  modelo.lasso.cvlars <- cv.lars(x = vectores, y = origin, index = lasso.lambdas, plot.it = FALSE)
  lasso.predict <- predict(modelo.lasso, newx=vectores)
  
  
  #También vamos a excluir aquellos coeficientes que sean iguales a 0
  
  #Y como vemos, nos quedamos con 761 variables del total. Si queremos representar gráficamente nuestra nueva onda sonora:
  
  
  return(lasso.predict)
  
}
start = 9000
stop = 9100
r1<-ssa_lasso_reg(cabritus@left[start:stop], l=500)
residuos1 <- apply(r1$fit, 2, function(x) x - cabritus@left[start:stop])
par(mfrow=c(1,3))
for (x in 1:dim(residuos1)[2]){
plot(residuos1[,x], ylim=c(-20000, 20000))
qqnorm(y=residuos1[,x])
qqline(residuos1[,x])
print(shapiro.test(residuos1[,x])$p.value)
plot(r1$fit[,x], type="l",ylim=c(-20000, 20000))
lines(cabritus@left[start:stop],type="l", col="red", lty=2)
lines(sheep@left[start:stop], type="l", col ="blue")
Sys.sleep(0.081)
}

for (x in 1:dim(residuos1)[2]){
  spectro(Wave(r1$fit[,x], samp.rate=8000, bit=16),wl=256)
  Sys.sleep(0.1)
}





freq_lasso_reg <- function(x,l = freq.amp){
  require(lars)
  require(Rssa)
  origin <- x
  
  
  
  vectores<- as.matrix(l)
  
  
  
  modelo.lasso <- lars(x = vectores, y = origin, type="lasso", normalize = F, use.Gram = F)
  print("ok")
  
  
  lasso.predict <- predict(modelo.lasso, newx=vectores)
  
  
  #También vamos a excluir aquellos coeficientes que sean iguales a 0
  
  #Y como vemos, nos quedamos con 761 variables del total. Si queremos representar gráficamente nuestra nueva onda sonora:
  
  
  return(lasso.predict)
  
}

start = 10000
stop = 10020
r1<-freq_lasso_reg(cabritus@left[start:stop], l = freq.amp[start:stop,])
residuos1 <- apply(r1$fit, 2, function(x) x - cabritus@left[start:stop])
par(mfrow=c(1,3))
for (x in 1:dim(residuos1)[2]){
  plot(residuos1[,x], ylim=c(-20000, 20000))
  qqnorm(y=residuos1[,x])
  qqline(residuos1[,x])
  print(sd(residuos1[,x]))
  print(mean(r1$fit[,x]))
  plot(r1$fit[,x], type="l",ylim=c(-20000, 20000))
  lines(cabritus@left[start:stop],type="l", col="red", lty=2)
  lines(sheep@left[start:stop], type="l", col ="blue")
  Sys.sleep(0.09)
}
plot(apply(residuos1, 2, function(x) sd(x)), type = "l")
plot(apply(residuos1, 2, function(x) shapiro.test(x)$p.value), type="l")
par(mfrow=c(1,1))
plot(r1$fit[,13], type="l", ylim=c(-11000,10500))
lines(sheep@left[start:stop], type="l", col ="blue")
lines(cabritus@left[start:stop],type="l", col="red", lty=2)
sqrt(mean((sheep@left[start:stop]-r1$fit[,13])^2))

library(e1071)
result<-stft(cabritus@left)
plot(result)
