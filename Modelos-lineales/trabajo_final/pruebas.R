load("cabritus.Rdata")
X<-matrix(nrow=19764,ncol=10000)
x<-2*pi*(1:19764)/19764
for(i in 1:5000){
  X[,(i-1)*2+1]<-sin(i*x)
  X[,i*2]<-cos(i*x)
}
cors<-apply(X,2,function(x){cor(x,cabritus@left)})
altas<-which(abs(cors)>0.015)
length(altas)
set.seed(1)
otras<-sample((1:10000)[-altas],length(altas))
# Matriz de frecuencias (covariables) ampliada con la muestra de correlaciones bajas
freq.amp<-X[,c(altas,otras)]
# Eliminamos la base de funciones completa ya que ocupa bastante memoria
rm(list="X")
plot(cors)


desing_matrix <- data.frame(freq.amp)
desing_matrix$Y <- cabritus @left
model1 <- lm(Y ~ ., data=desing_matrix)
deterministic1 <- apply(freq.amp, 1,function(x) sum(model1$coefficients[2:877]*x))+model1$coefficients[1]

residuos <- as.vector(model1$residuals)

desing_matrix2 <- data.frame(freq.amp)
desing_matrix2$Y <- residuos

model2 <- lm(Y ~ ., data=desing_matrix2)
summary(model2)



### sliding window

### 

desing_matrix <- data.frame(freq.amp)[1:473]
desing_matrix$Y <- cabritus@left

model3 <- lm(Y ~ ., desing_matrix)
spectro(Wave(cabritus@left - model3$residuals, samp.rate=8000))
result2 <- cabritus@left - model3$residuals
s5 <- ssa(result2, L=10)
recon5 <- reconstruct(s5, groups=list(c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)))
spectro(Wave(recon5$F1, samp.rate=8000)) 


model5 <- lm(cabritus@left ~ recon5$F1 )
summary(model5)
result3 <- cabritus@left - model5$residuals
spectro(Wave(result3, samp.rate=8000))
writeWave(Wave(result3, samp.rate=8000), "test.wav")

ssa7 <- ssa(result3 , L = 10)
recon7 <- reconstruct(ssa7, groups=list(c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)))
spectro(Wave(recon7$F2, samp.rate=8000))
writeWave(Wave(recon7$F2, samp.rate=8000), "test.wav")
model6 <- lm(cabritus@left ~ recon7$F2 )
summary(model6)
result3 <- cabritus@left - model6$residuals
par(mfrow=c(1,1))
plot(result3, type = "l")
writeWave(Wave(result3, samp.rate=8000), "test.wav")
par(mfrow=c(1,2))
spectro(Wave(result3, samp.rate=8000))
spectro(cabritus)
spectro(sheep)
### etiquetado
  
desing_matrix <- data.frame(freq.amp)[,1:473]
#desing_matrix$silence <- factor(c(rep(0,2000), rep(1,length(cabritus@left)-2000)))
desing_matrix$Y <- cabritus@left
model3 <- lm(Y ~ ., desing_matrix)
summary(model3)
result = cabritus@left - model3$residuals
plot(result, type="l")
plot(cabritus@left, type="l")

correlations <- apply(freq.amp[, 1:473], 2, function(x) cor(x, result))
new_freq.amp <- desing_matrix[,order(correlations)[1:400]]
desing_matrix2 <- new_freq.amp
desing_matrix2$Y <- cabritus@left
model4 <- lm(Y ~ ., desing_matrix2)
summary(model4)
result2 <- cabritus@left - model4$residuals
plot(result2[1000:18000])
plot(result)



##### SSA analysis

library(Rssa)
library(seewave)
spectro(cabritus)
s <- ssa(cabritus@left, L=10,svd="spectrum")
plot(s)
plot(s, type="vectors")
#plot(wcor(s, groups= list(c(1,2), c(1,3), c(1,9), c(1,7))))

recon = reconstruct(s, groups=list(c(1,2)))
#plot(recon$F1, type="l")
plot(recon$F1, type="l")
spectro(Wave(recon$F1,samp.rate=8000))

writeWave(Wave(recon$F1, samp.rate=8000), "test.wav")

desing_matrix <- data.frame(freq.amp)
desing_matrix$y <- recon$F1
modelssa <- lm(y ~ ., data=desing_matrix)
summary(modelssa)
result <- recon$F1 - modelssa$residuals
plot(result)
writeWave(Wave(result, samp.rate=8000), "test.wav")

s2 <- ssa(result, L=5)
plot(s2)
plot(s2, type="vectors")
recon2 = reconstruct(s2, groups=list(c(1,2)))
plot(recon2$F1, type="l")

desing_matrix2 <- data.frame(freq.amp)[,1:373]
desing_matrix2$y <- recon2$F1
modelssa2 <- lm(y ~ ., data=desing_matrix2)
summary(modelssa2)
result2 <- recon2$F1 - modelssa2$residuals
plot(result2)
writeWave(Wave(result2, samp.rate=8000), "test.wav")
spectro(Wave(result2, samp.rate=8000))
data(sheep)
spectro(sheep)
spectro(cabritus)
mean((sheep@left - result2)^2)/mean((sheep@left - cabritus@left)^2)


### ssa + linear model 
library(Rssa)
library(seewave)
library(leaps)
data(sheep)
s <- ssa(cabritus@left, 10)
plot(s)
plot(s, type="vectors")
recon <- reconstruct(s, groups=list(c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)))
desing_matrix4 <- data.frame(F1=as.vector(recon$F1),
                                F2=as.vector(recon$F2),
                                F3=as.vector(recon$F3),
                                F4=as.vector(recon$F4),
                                F5=as.vector(recon$F5),
                                F6=as.vector(recon$F6),
                                F7=as.vector(recon$F7),
                                F8=as.vector(recon$F8),
                                F9=as.vector(recon$F9),
                                F10=as.vector(recon$F10))
desing_matrix4$Y <- cabritus@left
modelos <- regsubsets(Y ~ ., data=desing_matrix4, method = "seqrep", nvmax=11)
sum.modelos <- summary(modelos)
sum.modelos
model5 <- lm(Y ~ F1 + F2 + F3    , desing_matrix4)

summary(model5)
result <- cabritus@left - model5$residuals
par(mfrow=c(1,1))
plot(result, type="l")
spectro(Wave(result,samp.rate= 8000))
spectro(Wave(desing_matrix4$F2, samp.rate=8000))
mean((sheep@left - result)^2)/mean((sheep@left - cabritus@left)^2)
mean((sheep@left - result)^2)
mean((sheep@left - cabritus@left)^2)
writeWave(Wave(result, samp.rate=8000), "test.wav")


s2 <- ssa(result, L = 10)
plot(s2)
plot(s2, type="vector")
recon2 <- reconstruct(s2, groups=list(c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)))
spectro(Wave(recon2$F1, samp.rate=8000))
spectro(Wave(desing_matrix4$F2, samp.rate=8000))

s3 <- ssa(recon2$F1, L = 10)
plot(s3 , type = "vector")
recon3 <- reconstruct(s3, groups=list(c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9), c(10)))
spectro(Wave(recon3$F1, samp.rate=8000))
spectro(Wave(recon2$F1, samp.rate=8000))
par(mfrow=c(1,1))
plot(recon3$F1, type="l")
model6 <- lm(cabritus@left ~ as.vector(recon3$F1))
summary(model6)
plot(cabritus@left - model6$residuals, type="l")
writeWave(Wave(cabritus@left - model6$residuals, samp.rate=8000), "test.wav")
result2 <- cabritus@left - model6$residuals
mean((sheep@left - result2)^2)/mean((sheep@left - cabritus@left)^2)


model
