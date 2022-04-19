library(signal)
library(seewave)
library(tuneR)

data(sheep)
noisy <- readWave("cabritus.wav")
#### uniform noise
MSEsignals <- function(original, noisy, constant, constant2){
 
 
    noisy_add <- original@left*constant2 + as.vector(noisew(8000, 2.4705, type = "unif"))*constant
    spec_noisy_add <- abs(specgram(noisy_add)$S)
    spec_noisy <- abs(specgram(noisy@left)$S)
    
    
  
  return( mean((spec_noisy-spec_noisy_add)^2))
}

result<-sapply(seq(0.1,1,0.1), function(i){

sapply(1:10000, function(x, i){
  print(x)
  MSEsignals(sheep, noisy, x, i)
}, i = i)
})
colnames(result)<- seq(0.1,1,0.1)
rownames(result)<- 1:10000
image(1:10000,seq(0.1,1,0.1),result)
ind<-which(result == min(result), arr.ind = TRUE)


noisy_reconstruct <- as.vector(sheep@left*(seq(0.1,1,0.1)[ind[2]])+noisew(8000, 2.4705, type = "unif")*ind[1])
specgram(noisy@left)
specgram(noisy_reconstruct)




### gaussian noise

MSEsignals <- function(original, noisy, constant, constant2){
  
  
  noisy_add <- original@left*constant2 + as.vector(noisew(8000, 2.4705, type = "gaussian"))*constant
  spec_noisy_add <- abs(specgram(noisy_add)$S)
  spec_noisy <- abs(specgram(noisy@left)$S)
  
  
  
  return( mean((spec_noisy-spec_noisy_add)^2))
}

result_gaussian<-sapply(seq(0.1,1,0.1), function(i){
  
  sapply(1:10000, function(x, i){
    print(x)
    MSEsignals(sheep, noisy, x, i)
  }, i = i)
})
colnames(result_gaussian)<- seq(0.1,1,0.1)
rownames(result_gaussian)<- 1:10000
image(1:10000,seq(0.1,1,0.1),result)
ind<-which(result_gaussian == min(result_gaussian), arr.ind = TRUE)


noisy_reconstruct <- as.vector(sheep@left*(seq(0.1,1,0.1)[ind[2]])+noisew(8000, 2.4705, type = "gaussian")*ind[1])
par(mfrow=c(1,2))
specgram(noisy@left)
specgram(noisy_reconstruct)
par(mfrow=c(1,1))

