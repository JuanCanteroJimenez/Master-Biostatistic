#### Single value decomposition

library(Rssa)
library(seewave)
library(leaps)
load("cabritus.Rdata")


plot(cabritus@left)
spectro(Wave(cabritus@left[2300:length(cabritus@left)], samp.rate = 8000))
cabritus_sound <- cabritus@left
model_ori <- lm(cabritus_sound ~ freq.amp[, 1:473])
summary(model_ori)
origin <- cabritus_sound - model_ori$residuals
spectro(Wave(origin, samp.rate=8000))


s1 <- ssa(origin, L = 20)
plot(s1)
plot(s1, type="vectors")
recon1 <- reconstruct(s1)
par(mfrow=c(1,1))

#plot(recon1)
vectores <- lapply(recon1, as.vector)
data_vectores_ori <- data.frame(vectores)
data_vectores <- data.frame(vectores)
data_vectores$Y <- cabritus_sound
models<-regsubsets(Y ~ ., data_vectores, nvmax=50, really.big=TRUE)  
model.sum<-summary(models)
par(mfrow=c(1,3))
plot(model.sum$bic)
plot(model.sum$cp)
plot(model.sum$rss)
data_vectores<-data.frame(data_vectores_ori[,model.sum$which[2,][-1]])
data_vectores$Y <- as.vector( cabritus_sound)

model1 <- lm(Y ~ ., data_vectores)
summary(model1)
result1 <- cabritus_sound-model1$residuals
par(mfrow=c(1,1))
plot(result1, type="l")
spectro(Wave(result1, samp.rate=8000))
spectro(sheep, samp.rate=8000)
writeWave(Wave(result1, samp.rate=8000), "test.wav")
mean((sheep@left-result1)^2)/mean((sheep@left-cabritus_sound)^2)
#### Descomposición aditiva de media

library(forecast)
cabritus_ts <- ts(cabritus@left, frequency = 8000, start=1)
par(mfrow=c(1,1))
plot(cabritus_ts)
cabritus_ts_decompose <- stats::decompose(cabritus_ts, type = "multiplicative")
autoplot(cabritus_ts_decompose)


#### lasso 
library(lars)
train <- 9000:9100
test <- 9050:9100
cabritus_m0 <- cabritus@left-mean(cabritus@left)
sound.lasso <- lars( x = freq.amp[train, ], y = cabritus_m0[train], trace=TRUE)



fraction <- 10^seq(0, -6, length = 100)
sound.lasso.cv <- cv.lars(x = freq.amp[train,], y = cabritus_m0[train], type = "lasso",  trace=T)
fraction[which.min(sound.lasso.cv$cv)]
fracs <- apply(coef(sound.lasso), 1, function(x){sum(abs(x))}) / sum(abs(coef(sound.lasso)[-1,][length(sound.lasso$lambda), ]))
sound.lasso.coef <- coef(sound.lasso)
fit <- sound.lasso.coef[which.min(abs(fraction[which.min(sound.lasso.cv$cv)]-fracs)),]
result <- apply(freq.amp[train, ], 1, function(x) sum(x*fit))
par(mfrow=c(1,1))
plot(result, type="l")
mean((sheep@left[train]-cabritus@left[train])^2)
plot(sheep@left[train]-mean(sheep@left), type="l")
lines(result, col="red")
plot(cabritus@left[train]-result)
plot(cabritus_m0[train], type="l")


#### Single value decomposition window
library(Rssa)
library(leaps)
window = 9300:9351
plot(cabritus@left[window], type = "l")
plot(sheep@left[window], type = "l")

origin <- cabritus@left[window]


s1 <- ssa(origin, L = 5)
plot(s1)
plot(s1, type="vectors")
recon1 <- reconstruct(s1)
par(mfrow=c(1,1))
plot(recon1$F1, type="l")
#plot(recon1)
vectores <- lapply(recon1, function(x) as.vector(x)/mean(as.vector(x)))
data_vectores_ori <- data.frame(vectores)
idx<-order(abs(apply(data_vectores_ori, 2, function(x) cor(x, origin))), decreasing = T)
data_vectores<-data.frame(data_vectores_ori[,idx[1]])
data_vectores$Y <- as.vector( origin)

model1 <- lm(Y ~ ., data_vectores)
summary(model1)
result1 <- origin-model1$residuals
par(mfrow=c(1,3))
plot(result1, type="l")
plot(sheep@left[window], type="l")
plot(cabritus@left[window], type="l")
mean((sheep@left[window]- result1)^2)
mean((sheep@left[window]- cabritus@left[window])^2)


ssa_linar_reg <- function(x){
  require(Rssa)
  origin <- x
  
  
  s1 <- ssa(origin, L = 200)
  
  recon1 <- reconstruct(s1)
  
  #plot(recon1)
  vectores <- lapply(recon1, function(x) as.vector(x))
  data_vectores_ori <- data.frame(vectores)
  #print(data_vectores_ori)
  idx<-order(abs(apply(data_vectores_ori, 2, function(x) cor(x, origin))), decreasing = T)
  #print(idx)
  data_vectores<-data.frame(data_vectores_ori[,idx[1:5]])
  data_vectores$Y <- as.vector( origin)
  
  model1 <- lm(Y ~ ., data_vectores)
  
  result1 <- origin-model1$residuals
  print("ok")
  return(result1[151])
  
}

conv_matrix <- list()

model1 <-lm(cabritus@left ~ freq.amp[, 1:473])
origin <- cabritus@left - model1$residuals
origin_padded <- c(rep(0, 151), resultsz$x, rep(0,150))
#origin <- cabritus@left
for (i in 1:(length(origin_padded)-301)){
  print(i)
  
  conv_matrix[[i]] <- origin_padded[i:(i+300)]
}

library(parallel)
#library(lme4)

result2<-mclapply(conv_matrix, ssa_linar_reg,mc.cores=11)
result2 <- unlist(result2)
par(mfrow=c(1,1))
plot(result2, type="l")
writeWave(Wave(result2, samp.rate=8000), "testconv.wav")
spectro(Wave(result2, samp.rate=8000))
spectro(Wave(resultsz, samp.rate=8000))
sqrt(mean((sheep@left-result2)^2))
write.csv(result2, "resultad.csv", row.names = F)
origin_padded <- c(rep(0, 6), result2, rep(0,5))
#origin <- cabritus@left
for (i in 1:(length(origin_padded)-11)){
  print(i)
  
  conv_matrix[[i]] <- origin_padded[i:(i+10)]
}

ssa_linar_reg <- function(x){
  require(Rssa)
  origin <- x
  
  
  s1 <- ssa(origin, L = 10)
  
  recon1 <- reconstruct(s1)
  
  #plot(recon1)
  vectores <- lapply(recon1, function(x) as.vector(x))
  data_vectores_ori <- data.frame(vectores)
  #print(data_vectores_ori)
  idx<-order(abs(apply(data_vectores_ori, 2, function(x) cor(x, origin))), decreasing = T)
  #print(idx)
  data_vectores<-data.frame(data_vectores_ori[,idx[1]])
  data_vectores$Y <- as.vector( origin)
  
  model1 <- lm(Y ~ ., data_vectores)
  
  result1 <- origin-model1$residuals
  return(result1[6])
  
}



result3 <- mclapply(conv_matrix, ssa_linar_reg)
result3 <- unlist(result3)
par(mfrow=c(1,1))
plot(result3, type="l")
spectro(Wave(result3, samp.rate=8000))
mean((sheep@left-result3)^2)
