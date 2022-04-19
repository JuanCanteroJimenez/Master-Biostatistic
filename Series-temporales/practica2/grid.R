library(forecast)
library(ggplot2)


create_matrix <- function(model = TRUE, damped = TRUE, lambda = TRUE, biasadj = TRUE, opt.crit = TRUE, ic = TRUE){
  options <- list()
  
  if (model){
    options[["level"]] <- c("A", "M", "N")
    options[["tendency"]] <- c("A", "M", "N")
    options[["error"]] <- c("A", "M", "N")
  }
  
  if (ic){
    options[["ic"]] <- c("aicc", "aic", "bic")
  }
  
  if (damped){
    options[["damped"]] <- c(TRUE, FALSE)
  }
  
  if (biasadj){
    options[["biasadj"]] <- c(TRUE, FALSE)
  }
  
  if (opt.crit){
    options[["opt.crit"]] <- c("lik", "amse", "mse", "sigma", "mae")
  }
  
  if (lambda){
    options[["lambda"]] <- c(TRUE, FALSE)
  }
  
  
  res <- expand.grid(options)
  if (model){
  models <- apply(res, 1, function(x) paste(x[ c("level", "tendency", "error")], collapse = ""))
  result <- res[, -which(colnames(res) == "level" | colnames(res) == "tendency" | colnames(res) == "error")]
  result$model <- models
  return(split(result, seq(nrow(result))))
  }else{
    return(split(result, seq(nrow(res))))
  }
}
expanded.grid<- create_matrix()
metrics <- function(x, data, k = 120, h = 36){
  
  arguments <- as.list(x)
  arguments$y <- data
  arguments$opt.crit <- as.character(arguments$opt.crit)
  arguments$ic <- as.character(arguments$ic)
  arguments$lambda <- as.numeric(arguments$lambda)
 
  if(arguments$lambda == 1){ arguments$lambda <- 0}else{arguments$lambda <- NULL}
  
  
  res<-try(do.call(ets, arguments), silent=TRUE)
  
  if (class(res)  == "ets"){
                      
    TT <- length(data)
    s <- TT - k - h           
    
    mapeEts <- matrix(NA, s + 1, h)
    for (i in 0:s) {
      train.set <- subset(data, start = i + 1, end = i + k)
      test.set <-  subset(data, start = i + k + 1, end = i + k + h)
      arguments$y <- train.set
      fit <- do.call(ets, arguments)
      fcast <- forecast(fit, h = h)
      mapeEts[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
    }
    
    mapeEts <- colMeans(mapeEts)
    
    result <- cbind(accuracy(res), matrix(mapeEts, nrow = 1, ncol= h, byrow = T))
    colnames(result) <- c(colnames(accuracy(res)), 1:h)
  return(result)
    
    }else{
    return(rep(NA, 7+h))}
}

serie <- read.csv2("AustriaT.csv")
serie <- ts(serie, start = 1999, frequency = 12)
library(parallel)
start <- Sys.time()
resultados<-mclapply(expanded.grid, function(x, y) metrics(x, y), y = serie, mc.cores = 11)
end <- Sys.time()
time <- end-start
matresult <- do.call(rbind.data.frame, resultados)

expanded.grid[[which.min(abs(matresult$"36"))]]
matresult[which.min(abs(matresult$"36")),]
accuracy(ets(serie))
ets(serie)

start <- Sys.time()
metrics(expanded.grid[[1621]], serie)
end <- Sys.time()
time <- end-start