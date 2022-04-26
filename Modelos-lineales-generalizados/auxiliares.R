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




plot(AGE,CHD,xlab="Edad")
# table(AGRP)
counts <- table(titanic.work$Fare, titanic.work$Survived)
props <- as.vector(counts[,2]/table(titanic.work$Fare))
plot(1:length(props),props,xlab="Edad codificada")











c("aj1.ber.logit",
  "aj1.bi.logit",
  "aj1.ber.probit"
  ,"aj1.bi.probit",
  "aj1.ber.cloglog",
  "aj1.bi.cloglog")