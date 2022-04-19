stf<-spectro(cabritus, wl=256)$amp
stf.discrete <-t(matrix(stf > quantile(as.vector(stf), (0.99)), nrow= nrow(stf), ncol=ncol(stf)))
par(mfrow=c(1,1))
image(stf.discrete)
franctions <- apply(stf.discrete, 1, mean)
library(spatstat)
plot(franctions, type="l")



strech <- function(x, flength = 19764){
  result <- c()
  for (i in 1:flength){
    result <- c(result, x[round(qunif(i/flength, 0, length(x)))])
  }
  return(result)
}
res<-strech(franctions)
plot(res*10, type="l")


freq_lasso_reg <- function(x,l = freq.amp){
  require(lars)
  
  patron <- x[(14+1):length(x)]
  position <- x[(14-2):14]
  origin <- x[1:11]
  
  print(length(patron))
  
  
  vectores<- as.matrix(l[position[1]:position[2],])
  
  
  
  modelo.lasso <- lars(x = vectores, y = origin, type="lasso", normalize = F, use.Gram = F)
  
  
  
  lasso.predict <- predict(modelo.lasso, newx=vectores)
  
  rmses <- apply(lasso.predict$fit, 2, function(x) sqrt(mean((patron-x)^2, na.rm = T)))
  
  
  
  
  
  
  #También vamos a excluir aquellos coeficientes que sean iguales a 0
  
  #Y como vemos, nos quedamos con 761 variables del total. Si queremos representar gráficamente nuestra nueva onda sonora:
  
  #
  return(lasso.predict$fit[,which.min(rmses)][6])
  
}

library(Rssa)

s1 <- ssa(cabritus@left, 300)
recon <- reconstruct(s1, groups = list(1:50))
plot(recon$F1, type = "l")
spectro(Wave(recon$F1, samp.rate=8000))
sqrt(mean((sheep@left - recon$F1)^2))

patron <- recon$F1


conv_matrix <- list()
origin_padded <- c(rep(0, 6), cabritus@left, rep(0,5))
patron_padded <- c(rep(0, 6), patron, rep(0,5))
freq.amp_pad <- rbind(matrix(rep(0,6*dim(freq.amp)[2]), nrow=6, ncol=dim(freq.amp)[2]),
                      freq.amp,
                      matrix(rep(0,5*dim(freq.amp)[2]), nrow=5, ncol=dim(freq.amp)[2]))
#origin <- cabritus@left
elements <- list()
for (i in 1:(length(origin_padded)-11)){
  
  print(i)
  elements[[i]] <-c(  origin_padded[i:(i+10)], i,(i+10), res[(i+6)]*20, patron_padded[i:(i+10)]) 
}

library(parallel)
result <- mclapply(elements, function(x,y) freq_lasso_reg(x, y), y = freq.amp_pad, mc.cores = 11)
result <- unlist(result)
par(mfrow=c(1,1))
plot(result, type = "l")
spectro(Wave(result, samp.rate=8000))
sqrt(mean((sheep@left-result)^2, na.rm = T))
result[is.na(result)] <- 0
model2 <- lm(result ~ freq.amp)
result3 <- result - model2$residuals
plot(result3, type = "l")
spectro(Wave(result3, samp.rate=8000))
sqrt(mean((sheep@left-result3)^2, na.rm = T))
start <- Sys.time()
freq_lasso_reg(elements[[1000]], freq.amp_pad[, 1:473])
end <- Sys.time()
end-start