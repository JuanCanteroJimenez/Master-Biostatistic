create_matrix <- function(model = TRUE,
                           damped = TRUE,
                           lambda = TRUE,
                           biasadj = TRUE,
                           opt.crit = TRUE,
                           ic = TRUE){
  
  matrix_to_functions <- function(x){
  
      expressions <- c("ets(serie,")
    
      arguments <- c("model", "lambda", "damped", "biasadj", "opt.crit", "ic")
    
      for (arg in arguments){
      
        sen <- try({x[[arg]]}, silent = T)
      
      if(!( class(sen) == "try-error")){
        
        if( arg == "opt.crit" | arg == "ic" | arg == "model"){
          expressions <- c(expressions, arg, "=", "\"",x[[arg]],"\"" ,"," )
        }else{
          expressions <- c(expressions, arg, "=", x[[arg]], "," )
        }
      }
    }
   
       result<- paste(expressions, collapse = "")
    
       result<- substring(result,1, nchar(result)-1)
    
       result<- paste(result, ")", sep = "")
    
       return(result)
    
  }
  
  
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
    
    options[["damped"]] <- c("TRUE", "FALSE")
  }
  if (biasadj){
    
    options[["biasadj"]] <- c("TRUE", "FALSE")
  }
  if (opt.crit){
    
    options[["opt.crit"]] <- c("lik", "amse", "mse", "sigma", "mae")
  }
  if (lambda){
    
    options[["lambda"]] <- c("0", "NULL")
  }
  res <- expand.grid(options)
  if (model){
    
    
    models <- apply(res, 1, function(x) paste(x[ c("level", "tendency", "error")], collapse = ""))
    
    result <- res[, -which(colnames(res) == "level" | colnames(res) == "tendency" | colnames(res) == "error")]
    
    result$model <- models
    
    result <- as.matrix(result)
    
  }else{
    result <- as.matrix(res)
  }
  return(as.list(apply(result, 1, matrix_to_functions)))
}



grid_search <- function(list_functions, datos){
  require(parallel)
  require(forecast)
  analisys <- function(x, data, k = 120, h = 36){
    serie <- data
    res<-try(eval(parse(text = x)), silent=TRUE)
    
    if (class(res)  == "ets"){
      
      TT <- length(data)
      s <- TT - k - h           
      
      mapeEts <- matrix(NA, s + 1, h)
      for (i in 0:s) {
        
        train.set <- subset(data, start = i + 1, end = i + k)
        
        test.set <-  subset(data, start = i + k + 1, end = i + k + h)
        
        serie <- train.set
        
        fit<-try(eval(parse(text = x)), silent=TRUE)
        
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
  
  previo <- parallel::mclapply(list_functions, function(x, y) analisys(x, data = y), y = datos, mc.cores = 11)
  
  previo <- do.call(rbind.data.frame, previo)
  
  result <- cbind(unlist(list_functions), previo)
  
  colnames(result) <- c("model", colnames(previo)) 
  
  return(result)
  
  
}


functions <- create_matrix2()
serie <- read.csv2("AustriaT.csv")
serie <- ts(serie, start = 1999, frequency = 12)

start <- Sys.time()
modelos <- grid_search(functions, serie)
end <- Sys.time()
time <- end - start
time
modelos_fil <- modelos[!is.na(modelos$ME), ]

modelos_fil[which.min(abs(modelos_fil$"36")),]
