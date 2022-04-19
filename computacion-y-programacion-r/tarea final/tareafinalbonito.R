graphics_wrap <- function(data,
                          tipo=c("correlacion","unico","VS"),
                          variable=NULL,
                          gplot=FALSE){#Se crea una función que genera plots
  #de correlación, plot de una sola variable, así como agrupaciones de una
  #variable numérica con todas la categóricas. Permitiendo además seleccionar
  #un motor gráfico. 
  correlation <- function(data){ #Se define la función que creará un plot de
   #correlación con todas la variables numéricas presentes en data. Esta función
    #hace uso del paquete gráfico básico de R. 
    tipos <- sapply(data, function(x){
      is.numeric(x)
  })#Se itera sobre las columnas del data.frame, comprobando si esta es númerica
    # el resultado es un vector de tipo lógico.
    
    numeric_names <- names(data)[tipos] # se selecciónan los nombres de las 
    #  columnas que poseen datos de tipo numérico
    if (length(numeric_names )<= 5){ # se comprueba que el número de variables
    #sea menor que 5. En este caso se realizará un plot de correlación mediante
     #un panel en el que se representan, mediante scatter plot, todas las
      #convinaciones de variables numéricas de tamaño 2. 
par(mfrow=c(length(numeric_names),length(numeric_names)))#se ajusta la distri_
      #bución de los plots.
      for (x in numeric_names){
        for (y in numeric_names){
          
      plot(data[[x]],data[[y]],xlab="",ylab="")#Se crean los plots que enfrentan
          # a las variables.
          title(main=paste(x,"VS",y), 
                sub = paste("correlacion =",
                            cor(data[[x]],
                                data[[y]])),
                xlab=x, ylab=y)#se añade el título, el titulo de los ejes, así 
          # como un subtitulo que indique el valor del indice de correlación. 
        }
      }# Se iterá dos veces sobre el vector numeric_names. 
    }else{
      #stop("Too much to plot")
    matt <- matrix(0, nrow=length(numeric_names),ncol=length(numeric_names))#se
      #crea la matriz que alvergará los coeficientes de correlación
      colnames(matt) <- numeric_names#Se da nombre a las columnas
      rownames(matt) <- numeric_names# Se da nombre a las filas
      for (x in 1:length(numeric_names)){
        for (y in 1:length(numeric_names)){
          matt[x, y] <- cor(data[[x]],data[[y]])
        }
      }#Se iterá sobre el vector numeric_names para calcular los coeficientes de 
      #correlación así como para rellenar matt
      par(mfrow=c(1,1))# Se ajustan el número de plots por ventana
      #matt[nrow(matt):1,]
      colfunc <- colorRampPalette(c("blue","white","red"))#Se crea la funcíon
    #colfunc que devuelve un degradado de colores desde el azula al rojo pasando
    #por el blanco, en un número de pasos igual al valor de su único argumento.
      #Vease uso más adelante. 
    matt[upper.tri(matt)] <- 0 #Se elimina la información repetida de la matriz
    layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))#Se crea un layout
      #que permita representar la matriz de puntos así como la leyenda. 
      image( matt, xaxt= "n", yaxt= "n",col = colfunc(20) )#Se representa la
      #matriz matt con los valores de correlación, coloreados en función de 
      #colfunc
      axis( 1,
            at=seq(0,1,length.out=ncol( matt ) ),
            labels= colnames( matt ), las= 2 )
      axis( 2,
            at=seq(0,1,length.out=nrow( matt ) ),
            labels= rownames( matt ), las= 2)#Se añaden ambos ejes a la imagen
      #crada por la función image
      
      legend_image <- as.raster(matrix(colfunc(20), ncol=1))#Se crea la imagen
      #que indica el valor numérico aproximado de la coloración escogida. 
      plot(c(0,2),c(0,1),type = 'n',
           axes = F,xlab = '', ylab = '', main = 'Correlación')#se crea un plot
      #vacio en el que se situará la leyenda
   text(x=1.5, y = seq(0,1,l=5), labels = seq(-1,1,l=5)[5:1])#Se añade el texto
      #de esta. 
      rasterImage(legend_image, 0, 0, 1,1)#Se generá la imagen en el plot vacio
      #crado anteriormente. 
    }# En caso de que existan demasiadas columnas a representar, el plot de
    #correlación se realiza como una matriz, coloreada en función del indice
    # de correlación. 
    #return(matt)
  }
  
  numeric_categoric <- function(data,variable){ #Se crea una función que
    #represente la variable numerica indicada en sus argumentos, en un boxplot
#con los datos agrupados en función de todas la variables categóricas presentes
    #en el dataset
    tipos <- sapply(data, function(x){
      is.factor(x)
    })#Se obtiene la posición de las columnas de tipo factor en el data.frame
    
    numeric_names <- names(data)[tipos]#Se seleccionan los nombres de las 
    #variables de tipo factor. 
    n_plots <- length(numeric_names)#Se obtiene la longitud del vector
    #numeric names
    if (n_plots <= 2){
      par(mfrow=c(1, n_plots))
    }else{
      par(mfrow=c(round(sqrt(n_plots)),round(sqrt(n_plots))))
    }#Se crea un layout en función del número de plots en la imagen, si este
  #es inferior a 2, se representan en una única fila, mientras que si es mas que
    #2 se representa en una cuadrícula. 
    for (grp in numeric_names){
      
      
      
      boxplot(data[[variable]] ~ data[[grp]],xlab="", ylab="")
      title(main = paste(variable, "vs", grp), ylab=variable, xlab=grp)#Se crean
      # los boxplot y se añade título a estos. 
    }#Se iterá sobre numeric_names para generar un plot de la variable, agrupada
    #en función de las variables presentes en numeric_names. 
    if (length(numeric_names) < prod(c(round(sqrt(n_plots)),round(sqrt(n_plots))))){
    n <-   prod(c(round(sqrt(n_plots)),round(sqrt(n_plots)))) -length(numeric_names)  
    for (i in n){
      plot(c(1,1),c(1,1),type = "n")
    }
    }
  }
  
  var_description <- function(data, variable, ggplot){#Se crea una función que 
    #genera un plot de la variable indicada del dataset aportado, permitiendo
    #escoger el tipo de motor gráfico que se desea usar. 
    if (ggplot){
      require(ggplot2)
    }#Se carga el paquete ggplot2 en el caso que sea necesario
    par(mfrow=c(1,1))#Se ajusta la disposición de los plots en la pantalla
    if(is.numeric(data[[variable]]) & ggplot == FALSE){
      hist(data[[variable]], xlab="",main="") 
      title(main = paste("Histograma de",variable), xlab=variable)#Se crea un 
      #histograma de la variables indicada con R base, se añade el título 
      #principal y el de el eje x
      print(summary(data[[variable]]))#Se añade un sumario estadístico de la
      #variable
      
    }#Se define una subrutina para cuando la variables es númerica y se quiere
    #graficar con R base
    if(is.factor(data[[variable]]) & ggplot ==FALSE){
      barplot(table(data[[variable]]), xlab="")
      title(main = paste("Barplot de",variable), xlab=variable)#Se crea un
      #gráfico de barras de la variable discreta deseada, se añade el título
      #principal y el de la variable x. 
      print(table(data[[variable]])/nrow(data))#Se muestrán las frecuencias 
      #relativas de la variables a graficar. 
    }#Se define una subrutina para cuando la variables es un factor y se quiere
    #graficar con R base.
    if(is.numeric(data[[variable]]) & ggplot == TRUE){
      j <-ggplot(data, aes_string(x=variable)) + geom_histogram() #Se crea un 
      #histograma haciendo uso de ggplot2
      print(j)#Se imprime la figura generada
      print(summary(data[[variable]]))#Se añade un sumario estadístico de 
      #la variable
    }#Se define una subrutina para cuando la variables es numerica y se quiere
    #graficar con ggplot.
    if(is.factor(data[[variable]]) & ggplot == TRUE){
      gather <- data.frame(categori = names(table(data[[variable]])), 
                           dat = as.vector(table(data[[variable]])))#se crea un 
      #dataframe adicional que facilitará la representación gráfica con ggplot2
      
      j <- ggplot(data=gather, aes(x=categori, y=dat)) +
      geom_bar(stat = "identity") # Se crea un grafico de barras de la variable
      # a graficar con el paquete ggplot2
      print(j)#Se imprime la figura creada
      print(table(data[[variable]])/nrow(data))#Se muestrán las frecuencias 
      #relativas de la variables a graficar. 
    }#Se define una subrutina para cuando la variables es de tipo factor y 
    #se quiere graficar con ggplot.
  }
  
correlation_ggplot <- function(data){ # Se crea una función que genera gráficos
    #de correlación para la variables numéricas en el argumento data, usando
    #el motor gráfico ggplot2
    tipos <- sapply(data, function(x){
      is.numeric(x)
    })#Se obtiene la posición de las columnas de tipo numerico en el data.frame
    
    numeric_names <- sort(names(data)[tipos])#Se seleccionan los nombres de las 
    #variables de tipo factor. Notese que se han ordenado los nombres, esto es
    #necesario debido a que ggplot2 ordenará posteriormente las variables a 
    #representar. 
    data_new <- data[,numeric_names] #Se crea un data.frame adicional que
    #facilitará la representación con ggplot2
    correlation_mat <- round(cor(data_new),2)#Se crea la matriz de correlación
    #en este caso con la función cor
    
    correlation_mat[upper.tri(correlation_mat)] <- NA #Se elimina la información
    # de la diagonal superior, pues esta repetida
    correlation_mat <- t(correlation_mat) # Por conveniencia se transpone la 
    #matriz
    
    
    
    melt_corr <- data.frame(list(Var1=rep(numeric_names, length(numeric_names)),
                                 Var2=rep(numeric_names, 
                                          rep(length(numeric_names),
                                              length(numeric_names))),
                                 value=rep(NA, length(numeric_names)^2)))
    #Se crea un data.frame auxiliar con toda la informacióna representar. Este
  #se encuentra vacio y se rellenará con el bucle for que se encuentra más abajo.
    for (x in 1:nrow(melt_corr)){
      if(!is.na(correlation_mat[melt_corr[x, 2], melt_corr[x,1]])){
        melt_corr[x, 3] <- correlation_mat[melt_corr[x, 2], melt_corr[x,1]]
      }
    }#Se rellena el campo value del dataframe creado con la información presente
    #en correlation_mat gracias a la información aportada por las variables
    #Var1 y Var2
    
    melt_corr$Var1 <- as.factor(melt_corr$Var1)
    melt_corr$Var2 <- as.factor(melt_corr$Var2)
    melt_corr$value <- as.numeric(melt_corr$value)#Se convierten las variables
    #al tipo neceario
    melted_cormat <- melt_corr[!is.na(melt_corr$value),]#Se eliminan los NA
    
  h <- ggplot(data = melted_cormat, aes(Var1, Var2, fill = value))+ #Se definen
      #el data.set de donde saldrán los datos, así como que hacer con cada
      #variable
      geom_raster() + #Se genera un geom de tipo raster, una imagen. 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") + #Se crea una escala
      #de color para usar en la imagen creada con geom_raster. 
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+#Se define como se
      #representará la etiqueta del eje x. 
      coord_fixed()#Se fijan las coordenadas del plot creado. 
    j <-  h +
      geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))
  #Se añaden los valores del coeficiente de correlación en la posición adecuada.
    print(j)#Se imprime el gráfico creado. 
    
}

numeric_categoric_ggplot <- function(data, variable){ #Se crea una función que
  #represente la variable numerica indicada en sus argumentos, en un boxplot
  #con los datos agrupados en función de todas la variables categóricas presentes
  #en el dataset
  require(ggplot2)#Se carga ggplot en caso de que no lo este
  tipos <- sapply(data, function(x){
    is.factor(x)
  })#Se obtiene la posición de las columnas de tipo factor en el data.frame
  
  factor_names <- names(data)[tipos]#Se seleccionan los nombres de las variables
  #de tipo factor a representar. 
  data_variables <- rep(factor_names, rep(nrow(data), length(factor_names)))
  #Se genera una columna con el nombre de cada variable categórica repetida
  #tantas veces como observaciones tiene el dataset. 
  data_numeric <- rep(data[[variable]], length(factor_names)) #Se crea una
  #columna con la variable numérica repetida tantas veces como variables
  #catefóricas tenga el dataset. 
  data_factors <- NULL
  for ( x in factor_names){
    
    data_factors <- c(data_factors, as.character(unlist(data[x])))
  }#Se rellena data_factors con el contenido de las columnas categóricas en el,
  #mismo orden que data_variables. 
  
  gather <- data.frame(list(variables=as.factor(data_variables),
                            response=as.numeric(data_numeric),
                            grupos=as.factor(data_factors)))#Se agrupan las 
  #columnas creadas antes, imponiendoles además el tipo de dato que deben ser. 
  p <- ggplot(gather)+
    aes(x=grupos, y=response)+
    geom_boxplot() + 
    facet_grid(. ~ variables,space="free",scale="free") #Se crea un plot de tipo
  #boxplot en función de los grupos de las variables. Estas variables además
  #serán usados para definir el layout del plot. 
  print(p)
  
}

if (length(tipo) > 1){tipo ="correlacion"}#Si no se indica un valor, por defecto
#realizará plot de tipo correlación.

if (tipo == "correlacion"){
  if (gplot == TRUE){
    correlation_ggplot(data)
    tipos <- sapply(data, function(x){
      is.numeric(x)
    })#Se itera sobre las columnas del data.frame, comprobando si esta es númerica
    # el resultado es un vector de tipo lógico.
    
    numeric_names <- names(data)[tipos] #Se obtienen los nombres de las variables
    #numéricas
    for (x in numeric_names){
      print(x)
      print(c(summary(data[[x]]), varianza=var(data[[x]])))
    }
    
  }else{
    correlation(data)
    tipos <- sapply(data, function(x){
      is.numeric(x)
    })#Se itera sobre las columnas del data.frame, comprobando si esta es númerica
    # el resultado es un vector de tipo lógico.
    
    numeric_names <- names(data)[tipos] #Se obtienen los nombres de las variables
    #numéricas
    for (x in numeric_names){
      print(x)
      print(c(summary(data[[x]]), varianza=var(data[[x]])))
    }
    
  }
}#Se crea una condición que ejecuta el programa en caso de que el tipo sea 
#correlación
if(tipo == "unico"){
  var_description(data, variable,ggplot = gplot)
}#Se crea una condición que ejecuta el programa en caso de que el tipo sea 
#unico
if(tipo == "VS"){
  if (gplot == TRUE){
    numeric_categoric_ggplot(data, variable)
    tipos <- sapply(data, function(x){
      is.factor(x)
    })#Se obtiene la posición de las columnas de tipo factor en el data.frame
    
    factor_names <- names(data)[tipos]#Se seleccionan los nombres de las variables
    #de tipo factor a representar. 
    print(variable)
    print(c(summary(data[[variable]]),varianza=var(data[[variable]])))
    for (x in factor_names){
      print(x)
      print(table(data[[x]])/length(data[[x]]))
    }
  }else{
    numeric_categoric(data, variable)
    tipos <- sapply(data, function(x){
      is.factor(x)
    })#Se obtiene la posición de las columnas de tipo factor en el data.frame
    
    factor_names <- names(data)[tipos]#Se seleccionan los nombres de las variables
    #de tipo factor a representar. 
    print(variable)
    print(c(summary(data[[variable]]),varianza=var(data[[variable]])))
    for (x in factor_names){
      print(x)
      print(table(data[[x]])/length(data[[x]]))
    }
  }
}#Se crea una condición que ejecuta el programa en caso de que el tipo sea 
# VS

  
}
graphics_wrap(data,gplot = FALSE)
graphics_wrap(data,gplot = TRUE)
graphics_wrap(mtcars, gplot=FALSE )
graphics_wrap(mtcars, gplot=TRUE)
graphics_wrap(data,tipo="unico",variable="A",gplot=TRUE)
graphics_wrap(data,tipo="unico",variable="A",gplot=FALSE)
graphics_wrap(data,tipo="unico",variable="B",gplot=TRUE)
graphics_wrap(data,tipo="unico",variable="B",gplot=FALSE)
graphics_wrap(data,tipo = "VS",variable="A",gplot=FALSE)
graphics_wrap(data,tipo = "VS",variable="A",gplot=TRUE)
