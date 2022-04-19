library(ISLR)
data(Auto)

## 1
head(Auto)
nrow(Auto)
d_mat <- data.frame(cbind(mpg=Auto$mpg, horsepower=Auto$horsepower))
head(d_mat)
plot(d_mat[,2], d_mat[,1])
cor(d_mat)
cor.test(d_mat[,1], d_mat[,2])
model1<-lm(mpg ~ horsepower, data=d_mat)
summary(model1)
plot(d_mat$horsepower, d_mat$mpg)
lines(50:200, predict(model1, data.frame(horsepower=50:200)))
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))
plot(model1$residuals)
### D la relación es negativa, es decir si aumenta la variable horsepower, disminuye mpg
### E
confint(model1)
## my confint 
predict(model1, newdata = data.frame(horsepower=c(98)), interval = "predict")
predict(model1, newdata = data.frame(horsepower=c(98)), interval = "confidence")
pred<-predict(model1, newdata = data.frame(horsepower=c(50:200)), interval = "predict")
confi <- predict(model1, newdata = data.frame(horsepower=c(50:200)), interval = "confidence")
plot(d_mat$horsepower, d_mat$mpg)
lines(50:200, predict(model1, data.frame(horsepower=50:200)))
lines(50:200, pred[,3], col="red")
lines(50:200, pred[,2], col="red")
lines(50:200, confi[,2],col="blue")
lines(50:200, confi[,3],col="blue")

### 2
head(Auto)
nrow(Auto)
d_mat <- data.frame(cbind(mpg=Auto$mpg, year=Auto$year))
head(d_mat)
plot(d_mat[,2], d_mat[,1])
cor(d_mat)
cor.test(d_mat[,1], d_mat[,2])
model2<-lm(mpg ~ year, data=d_mat)
summary(model2)
plot(d_mat$year, d_mat$mpg)
lines(50:200, predict(model2, data.frame(year=50:200)))
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))
plot(model2$residuals)
summary(model1)

predict(model2, newdata = data.frame(year=c(98)), interval = "predict")
predict(model2, newdata = data.frame(year=c(98)), interval = "confidence")
pred<-predict(model2, newdata = data.frame(year=c(50:200)), interval = "predict")
confi <- predict(model2, newdata = data.frame(year=c(50:200)), interval = "confidence")
plot(d_mat$year, d_mat$mpg)
lines(50:200, predict(model2, data.frame(year=50:200)))
lines(50:200, pred[,3], col="red")
lines(50:200, pred[,2], col="red")
lines(50:200, confi[,2],col="blue")
lines(50:200, confi[,3],col="blue")

### 3

d_mat <- data.frame(cbind(mpg=Auto$mpg, invhorsepower=1/Auto$horsepower))
plot(d_mat$invhorsepower, d_mat$mpg)
model3 <- lm(mpg ~ invhorsepower,data = d_mat)
summary(model3)
summary(model1)
plot(d_mat$invhorsepower, d_mat$mpg)
abline(model3$coefficients)
plot(model3$residuals)

### 4

### si, pues esto indica una relación negativa entre las variables es decir que cuando una aumenta la otra disminuye. 

### no, el intervalo de predicción no depende del tamaño de la muestra. 

### no, seria necesario eliminar la variabilidad de los puntos, restar algo a los coeficientes actua como un escalado de la variable. 

### no, la media de los residuos valdra cero. 
cor.test(d_mat$mpg, d_mat$invhorsepower-mean(d_mat$invhorsepower))

d_mat
