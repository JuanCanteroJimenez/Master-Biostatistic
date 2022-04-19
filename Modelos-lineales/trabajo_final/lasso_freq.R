ssa_lasso_reg <- function(x){
  
  require(Rssa)
  origin <- x
 
  
  s1 <- ssa(origin, L = 10)
  
  
  recon1 <- reconstruct(s1)
  
  print(length(recon1))
  #plot(recon1)
  vectores <-  lapply(recon1, function(x) as.vector(x))
  
  data_vectores_ori <- data.frame(vectores)
  #print(data_vectores_ori)
  idx<-order(abs(apply(data_vectores_ori, 2, function(x) cor(x, origin))), decreasing = T)
  #print(idx)
  vectores<- as.matrix(data.frame(data_vectores_ori[,idx[1:2]]))
  
  
  
  modelo.lasso <- lars(x = vectores, y = origin, type="lasso")
  print("ok")
  lasso.lambdas <- 10^seq(0, -1, length = 100)
  modelo.lasso.cvlars <- cv.lars(x = vectores, y = origin, index = lasso.lambdas, plot.it = FALSE)
  lasso.predict <- predict(modelo.lasso, newx=vectores)
  lasso.coefs <- coef(modelo.lasso)
  fractions <- apply(coef(modelo.lasso), 1, function(x){sum(abs(x))}) / sum(abs(coef
                                                                                (modelo.lasso)[-1, ][length(modelo.lasso$lambda), ]))
  
  
  fit <- lasso.coefs[which.min( abs(lasso.lambdas[which.min(modelo.lasso.cvlars$cv)] - fractions)), ]
  
  
  #También vamos a excluir aquellos coeficientes que sean iguales a 0
  
  #Y como vemos, nos quedamos con 761 variables del total. Si queremos representar gráficamente nuestra nueva onda sonora:
  
  
  return(lasso.predict$fit[,which.min(abs(fractions-lasso.lambdas[which.min(modelo.lasso.cvlars$cv)]))][6])
  
}

load("cabritus.Rdata")
conv_matrix <- list()

model1 <-lm(cabritus@left ~ freq.amp[, 1:473])
origin <- cabritus@left - model1$residuals
origin_padded <- c(rep(0, 151), cabritus@left, rep(0,150))
#origin <- cabritus@left
for (i in 1:(length(origin_padded)-11)){
  print(i)
  
  conv_matrix[[i]] <- origin_padded[i:(i+10)]
}

library(parallel)

result3 <- mclapply(conv_matrix, ssa_lasso_reg, mc.cores=11 )

result3 <- unlist(result3)
par(mfrow=c(1,1))
plot(result3, type="l")
spectro(Wave(result3, samp.rate=8000))
sqrt(mean((sheep@left-result3)^2))


result<-ssa_lasso_reg(cabritus)

par(mfrow=c(1,2))  
plot(ssa_lasso_reg(cabritus), type="l")
plot(cabritus, type = "l")
spectro(sheep)
spectro(Wave(ssa_lasso_reg(cabritus), samp.rate=8000))
sqrt(mean((sheep@left-resultz$x)^2))




lasso_freq.amp <- function(origin, freqq.amp){
  
  
  start <- origin[length(origin)-1]
  stop <- origin[length(origin)]
  origin <- origin[1:(length(origin)-2)]
  
  modelo.lasso <- lars(x = freqq.amp[start:stop,], y = origin, type="lasso", use.Gram = FALSE)
  
  lasso.lambdas <- 10^seq(0, -1, length = 100)
  modelo.lasso.cvlars <- cv.lars(x = freqq.amp[start:stop,], y = origin, index = lasso.lambdas, plot.it = FALSE, use.Gram = F)
  lasso.predict <- predict(modelo.lasso, freqq.amp[start:stop,])
  lasso.coefs <- coef(modelo.lasso)
  fractions <- apply(coef(modelo.lasso), 1, function(x){sum(abs(x))}) / sum(abs(coef
                                                                                (modelo.lasso)[-1, ][length(modelo.lasso$lambda), ]))
  
  
  fit <- lasso.coefs[which.min( abs(lasso.lambdas[which.min(modelo.lasso.cvlars$cv)] - fractions)), ]
  
  
  #También vamos a excluir aquellos coeficientes que sean iguales a 0
  
  #Y como vemos, nos quedamos con 761 variables del total. Si queremos representar gráficamente nuestra nueva onda sonora:
  
  
  return(lasso.predict$fit[,which.min(abs(fractions-lasso.lambdas[which.min(modelo.lasso.cvlars$cv)]))][51])
  
}
conv_matrix <- list()
origin_padded <- c(rep(0, 51), cabritus@left, rep(0,50))
#origin <- cabritus@left
for (i in 1:(length(origin_padded)-101)){
  print(i)
  
  conv_matrix[[i]] <- c(origin_padded[i:(i+100)], i, i+100)
}


library(lars)
library(parallel)
start <- Sys.time()
result<- mclapply(conv_matrix[1:19664], function(x,y) {lasso_freq.amp(x, y)}, mc.cores=11,y = freq.amp[,1:473])
result <- unlist(result)
end <- Sys.time()
time_elapsed <- end-start
par(mfrow=c(1,1))
plot(result, type="l")
lines(cabritus@left[start:stop], type="l", col = "red")
lines(sheep@left[start:stop], type="l", col = "blue")
spectro(Wave(result, samp.rate=8000))
result_slow <- c()
for (i in 1:length(conv_matrix)){
  result<- lasso_freq.amp(conv_matrix[[i]], freq.amp[, 1:473])
  result_slow <- c(result_slow, result)
  print(i)
  
}

result_slow = unlist(result_slow)
plot(result_slow, type = "l")
spectro(Wave(result_slow, samp.rate=8000))
sqrt(mean((sheep@left - result)^2))

start <- Sys.time()
lasso_freq.amp(conv_matrix[[1]], freq.amp[, 1:473])
end <- Sys.time()
end-start


library(Rssa)

s1 <- ssa(cabritus@left, 300)
recon <- reconstruct(s1, groups = list(1:50))
plot(recon$F1, type = "l")
spectro(Wave(recon$F1, samp.rate=8000))
sqrt(mean((sheep@left - recon$F1)^2))
