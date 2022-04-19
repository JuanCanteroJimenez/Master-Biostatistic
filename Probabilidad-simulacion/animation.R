f <- function(x){
  wrap <- function(x){
    if(x > 0 & x < 20){return( (x^4)*(5+x)^-10  )}else{return(0)}
  }
  if (length(x) == 1){return(wrap(x))}else{return(sapply(x,wrap))}
}

curve(dgamma(x,2,0.3),0,20,col="red")
curve(f(x),0,20,add=TRUE)
cc <- max(f((1:190)/10)/dgamma((1:190)/10,2,0.3))

curve(dgamma(x,2,0.3)*cc,0,20,col="red")
curve(f(x),0,20,add=TRUE)



wrap <- function(x){dgamma(x,2,0.3)*cc}
efi<-integrate(f,0,20)$value/integrate(wrap,0,20)$value
print(efi)
rcustom <- function(i,cc,efi=FALSE){
  x <- c()
  y <- c()
  z <- 0
  while(length(x)< i){
    z <- z+1
    
    number <- rgamma(1,2,0.3)
    
    y_ <- runif(1,0,(dgamma(number,2,0.3)*cc))
    if(y_ < f(number)){
      x <- c(x, number)
      y <- c(y,y_ )
    }
  }
  
  if(efi){i/z}else{return(cbind(x=x,y=y))}
}





library(animation)
rcustom_grafic_animation <- function(i,cc,efi=FALSE,efi_=efi){
  x <- c()
  x_recha <- c()
  y <- c()
  y_recha <- c()
  z <- 0
  ani.record(reset = TRUE)
  while(length(x)< i){
    z <- z+1
    curve(dgamma(x,2,0.3)*cc,0,20,col="red")
    curve(f(x),0,20,add=TRUE)
    points(x,y,col="blue")
    points(x_recha,y_recha,col="red")
    number <- rgamma(1,2,0.3)
    
    points(number,0,col="green")
    text(number,0,"rgamma(1,2,0.3)",cex=0.5)
    text(15,6e-08,paste("Aceptados ",
                        length(x),
                        "\n",
                        "Rechazados ",
                        length(x_recha),
                        "\n",
                        "Eficiencia ",
                        length(x)/(length(x_recha)+length(x)),
                        "\n",
                        "Eficiencia Teorica ",
                        efi_),cex=0.6)
    ani.record()
    y_ <- runif(1,0,(dgamma(number,2,0.3)*cc))
    
    if(y_ < f(number)){
      x <- c(x, number)
      y <- c(y,y_ )
      points(number,y_,col="blue")
      text(number,y_,"runif(1,0,(dgamma(number,2,0.3)*cc))",col="blue",cex=0.5)
      arrows(number,0,number,y_,col="blue")
      
      
    }else{
      x_recha <- c(x_recha,number)
      y_recha <- c(y_recha,y_)
      points(number,y_,col="red")
      text(number,y_,"runif(1,0,(dgamma(number,2,0.3)*cc))",col="red",cex=0.5)
      arrows(number,0,number,y_,col="red")
      
      
    }
    
    ani.record()
  }
  
  saveVideo(ani.replay(),img.name="record_plot",video.name = "ar1.mp4"
            )
}

rcustom_grafic_animation(100,cc,efi_ = efi)

rcustom_grafic(1000,cc,efi_=efi)
rcustom_grafic <- function(i,cc,efi=FALSE,efi_=efi){
  x <- c()
  x_recha <- c()
  y <- c()
  y_recha <- c()
  z <- 0
  while(length(x)< i){
    z <- z+1
    curve(dgamma(x,2,0.3)*cc,0,20,col="red")
    curve(f(x),0,20,add=TRUE)
    points(x,y,col="blue")
    points(x_recha,y_recha,col="red")
    number <- rgamma(1,2,0.3)
    
    points(number,0,col="green")
    text(number,0,"rgamma(1,2,0.3)",cex=0.5)
    text(15,6e-08,paste("Aceptados ",
                        length(x),
                        "\n",
                        "Rechazados ",
                        length(x_recha),
                        "\n",
                        "Eficiencia ",
                        length(x)/(length(x_recha)+length(x)),
                        "\n",
                        "Eficiencia Teorica ",
                        efi_),cex=0.6)
    Sys.sleep(1)
    y_ <- runif(1,0,(dgamma(number,2,0.3)*cc))
    
    if(y_ < f(number)){
      x <- c(x, number)
      y <- c(y,y_ )
      points(number,y_,col="blue")
      text(number,y_,"runif(1,0,(dgamma(number,2,0.3)*cc))",col="blue",cex=0.5)
      arrows(number,0,number,y_,col="blue")
      
      Sys.sleep(1)
    }else{
      x_recha <- c(x_recha,number)
      y_recha <- c(y_recha,y_)
      points(number,y_,col="red")
      text(number,y_,"runif(1,0,(dgamma(number,2,0.3)*cc))",col="red",cex=0.5)
      arrows(number,0,number,y_,col="red")
      
      Sys.sleep(1)
    }
  }
  
  if(efi){i/z}else{return(cbind(x=x,y=y))}
}