string <-  ", Mrs." 
m<-regexpr("\\w{1,20}", string)
regmatches(string, m)

res<-sapply(titanic_train$Name, function(x){
  
  m<-regexpr(",.{1,20}\\.", x)
  first.result <- regmatches(x, m)
  m2 <- regexpr("\\w{1,20}", first.result)
  regmatches(first.result, m2)
  
})
res


datos <- read.table("chdage.dat",header=T)
datos

datos <- read.table("chdage.dat",header=T)
attach(datos)
par(mfrow=c(2,1))
plot(AGE,CHD,xlab="Edad")
# table(AGRP)
counts <- table(titanic.work$Fare, titanic.work$Survived)
props <- as.vector(counts[,2]/table(titanic.work$Fare))
plot(1:length(props),props,xlab="Edad codificada")
