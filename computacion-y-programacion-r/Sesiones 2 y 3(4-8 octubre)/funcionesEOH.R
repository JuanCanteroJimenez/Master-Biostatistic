setwd("S:/CargaDatos/0609002 EOH") #Fijamos directorio de trabajo.

list.of.packages <- c("httr", "zip", "openxlsx") #Paquetes que queremos tener
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) #Paquetes que instalamos si no estan

library(httr) #Libreria para leer informacion de paginas web
if(any(grepl("package:xlsx", search()))) detach("package:xlsx")
library(openxlsx) #Libreria para trabajar con documentos excel.
#La segunda linea de este conjunto de tres lineas es para desfijar la libreria xlsx.
#xlsx y open trabajan de manera muy similar, pero preferimos trabajar con openxlsx.

hoteles <- function(last){
  
  
  
Tipo=c(1,2) #Existen hotelos y apartamentos. Estamos trabajando con hoteles, que son tipo 1.
CCAA=10 #Codigo para cuando trabajemos con Comunidad Autonoma (C.Valenciana).
Provincia=46 #Codigo para cuando trabajemos con Provincia (Valencia).
municipios<-c(08019, 28079, 29067, 41091, 46250, 50297) #Codigos relativos a Barcelona, Madrid, Malaga, Sevilla, Valencia y Zaragoza.
codigos_municipios_INE<-c(563, 2813, 485, 7112, 493, 494) #Codigos con los que trabaja INE, y por tanto, nosotros.
  
  
options(scipen=999)

#DEMANDA ESPANYA
url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2074?tv=349:16473&nult=",last) #Por ultimo a partir de categorias, ya estaremos cerca de conseguir la informacion final que deseamos.
path <- httr::GET(url.data)
Sys.sleep(1)
data.json <- httr::content(path, as = "text")
datos <- jsonlite::fromJSON(data.json) #Aqui estan nuestros datos finales objetos de estudio.
Sys.sleep(1)

datos$Nombre<-tolower(datos$Nombre) 
#En esta linea trabajamos con la columna Nombre de la "matriz(data.frame)" datos.
#Convertimos la informacion de la columna a minusculas para evitar confusion en la busqueda de informacion.

pos_Viaj_ResEsp<-which(grepl(pattern = "viajeros.*residentes en espa\u00F1a", x = datos$Nombre))
#Queremos saber en que posicion de la columna nombre esta la informacion entre comillas.
Viaj_ResEsp<-datos$Data[[pos_Viaj_ResEsp]]$Valor
#De esa posicion extraemos de la columna Data la informacion correspondiente a valor.

#ESTE ES EL MISMO RAZONAMIENTO QUE IMPLEMENTAMOS EN GRAN PARTE DEL CODIGO

pos_Viaj_ResExt<-which(grepl(pattern = "viajeros.*residentes en el extranjero", x = datos$Nombre))
Viaj_ResExt<-datos$Data[[pos_Viaj_ResExt]]$Valor

Viaj_Total<-Viaj_ResEsp+Viaj_ResExt
#Viajeros totales son los residentes en Espanya mas los residentes en el extranjero, calculados previamente.

pos_per_ResEsp<-which(grepl(pattern = "pernoctaciones.*espa\u00F1a", x = datos$Nombre))
per_ResEsp<-datos$Data[[pos_per_ResEsp]]$Valor

pos_per_ResExt<-which(grepl(pattern = "pernoctaciones.*extranjero", x = datos$Nombre))
per_ResExt<-datos$Data[[pos_per_ResExt]]$Valor

Per_Total<-per_ResEsp+per_ResExt
#Las pernoctaciones totales son las relativas a los residentes en Espanya mas las relativas a los residentes en el extranjero, calculadas previamente.


est_med_esp <- per_ResEsp/Viaj_ResEsp #La media de pernoctaciones por viajero en Espanya.
est_med_ext <- per_ResExt/Viaj_ResExt #La media de pernoctaciones por viajero en el extranjero.
est_med_total <- Per_Total/Viaj_Total #La media de pernoctaciones por viajero total.

anyo <- datos$Data[[1]]$Anyo #Guardamos el anyo en el que trabajamos.
#Como la informacion anyo es la misma para el conjunto Data$Anyo, podemos seleccionar la posicion 1.
mes <- datos$Data[[1]]$FK_Periodo
#Para seleccionar el mes seguimos un razonamiento similar.

DatosDemanda1Espana<-data.frame(anyo,mes,Tipo[1],Viaj_Total,Viaj_ResEsp,Viaj_ResExt,Per_Total,per_ResEsp,per_ResExt,est_med_total,est_med_esp,est_med_ext)
#Por ultimo debemos construir un data.frame que almacene toda la informacion final.
#Usamos data.frame y no matrix porque la informacion almacenada presenta distintos tipos de caracter.

colnames(DatosDemanda1Espana)<-c("A\u00F1o","Mes","TipoEstablecimiento","Viaj_Total","Viaj_ResEsp","Viaj_ResExt","Pern_Total","Pern_ResEsp","Pern_ResExt","EstanciaMedia_Total","EstanciaMedia_ResEsp","EstanciaMedia_ResExt")
#Por ultimo establecemos los titulos de las columnas para otorgar dinamismo a la tabla(data.frame).

#ESTE PROCEDIMIENTO ES SIMILAR PARA DEMANDA EN CCAA Y PROVINCIA (SALVANDO LOS CODIGOS DONDE BUSCAMOS INFORMACION)

#DEMANDA CCAA
url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2074?tv=70:9006&nult=",last)
Sys.sleep(1)
path <- httr::GET(url.data)
data.json <- httr::content(path, as = "text")
datos<- jsonlite::fromJSON(data.json)
Sys.sleep(1)

datos$Nombre<-tolower(datos$Nombre)

pos_Viaj_ResEsp<-which(grepl(pattern = "viajeros.*residentes en espa\u00F1a", x = datos$Nombre))
Viaj_ResEsp<-datos$Data[[pos_Viaj_ResEsp]]$Valor

pos_Viaj_ResExt<-which(grepl(pattern = "viajeros.*residentes en el extranjero", x = datos$Nombre))
Viaj_ResExt<-datos$Data[[pos_Viaj_ResExt]]$Valor

Viaj_Total<-Viaj_ResEsp+Viaj_ResExt

pos_per_ResEsp<-which(grepl(pattern = "pernoctaciones.*espa\u00F1a", x = datos$Nombre))
per_ResEsp<-datos$Data[[pos_per_ResEsp]]$Valor

pos_per_ResExt<-which(grepl(pattern = "pernoctaciones.*extranjero", x = datos$Nombre))
per_ResExt<-datos$Data[[pos_per_ResExt]]$Valor

Per_Total<-per_ResEsp+per_ResExt

est_med_esp <- per_ResEsp/Viaj_ResEsp
est_med_ext <- per_ResExt/Viaj_ResExt
est_med_total <- Per_Total/Viaj_Total

anyo <- datos$Data[[1]]$Anyo
mes <- datos$Data[[1]]$FK_Periodo

DatosDemanda1CCAA<-data.frame(anyo,mes,CCAA, Tipo[1],Viaj_Total,Viaj_ResEsp,Viaj_ResExt,Per_Total,per_ResEsp,per_ResExt,est_med_total,est_med_esp,est_med_ext)
colnames(DatosDemanda1CCAA)<-c("A\u00F1o","Mes","CCAA","TipoEstablecimiento","Viaj_Total","Viaj_ResEsp","Viaj_ResExt","Pern_Total","Pern_ResEsp","Pern_ResExt","EstanciaMedia","EstanciaMedia_ResEsp","EstanciaMedia_ResExt")


#DEMANDA PROVINCIA
url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2074?tv=115:46&nult=",last)
Sys.sleep(1)
path <- httr::GET(url.data)
data.json <- httr::content(path, as = "text")
datos<- jsonlite::fromJSON(data.json)
Sys.sleep(1)

datos$Nombre<-tolower(datos$Nombre)

pos_Viaj_ResEsp<-which(grepl(pattern = "viajeros.*residentes en espa\u00F1a", x = datos$Nombre))
Viaj_ResEsp<-datos$Data[[pos_Viaj_ResEsp]]$Valor

pos_Viaj_ResExt<-which(grepl(pattern = "viajeros.*residentes en el extranjero", x = datos$Nombre))
Viaj_ResExt<-datos$Data[[pos_Viaj_ResExt]]$Valor

Viaj_Total<-Viaj_ResEsp+Viaj_ResExt

pos_per_ResEsp<-which(grepl(pattern = "pernoctaciones.*espa\u00F1a", x = datos$Nombre))
per_ResEsp<-datos$Data[[pos_per_ResEsp]]$Valor

pos_per_ResExt<-which(grepl(pattern = "pernoctaciones.*extranjero", x = datos$Nombre))
per_ResExt<-datos$Data[[pos_per_ResExt]]$Valor

Per_Total<-per_ResEsp+per_ResExt

est_med_esp <- per_ResEsp/Viaj_ResEsp
est_med_ext <- per_ResExt/Viaj_ResExt
est_med_total <- Per_Total/Viaj_Total

anyo <- datos$Data[[1]]$Anyo
mes <- datos$Data[[1]]$FK_Periodo

DatosDemanda1Provincia<-data.frame(anyo,mes,Provincia,Tipo[1],Viaj_Total,Viaj_ResEsp,Viaj_ResExt,Per_Total,per_ResEsp,per_ResExt,est_med_total,est_med_esp,est_med_ext)
colnames(DatosDemanda1Provincia)<-c("A\u00F1o","Mes","Provincia","TipoEstablecimiento","Viaj_Total","Viaj_ResEsp","Viaj_ResExt","Pern_Total","Pern_ResEsp","Pern_ResExt","EstanciaMedia_Total","EstanciaMedia_ResEsp","EstanciaMedia_ResExt")



#DEMANDA MUNICIPIOS
#En municipios se nos presenta el problema que tenemos que buscar informacion relativa a seis municipios.
#Por eso, realizaremos un procedimiento muy similar al anterior, aunque con un bucle for.
DatosDemanda1Municipio<-c() 
#Este es el data.frame que construiremos al final. Lo iniciamos vacio para posteriormente poder anyadir la informacion relativa a cada municipio.

for(i in 1:length(codigos_municipios_INE)){ #Este bucle nos permite extraer la info de cada municipio.
  
  
  url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2078?tv=103:",codigos_municipios_INE[i],"&nult=",last)
  #Utilizamos la funcion paste0. De esta manera no hace falta que copiemos el mismo url.data para los 6 municipios.
  #paste0 mantiene la informacion entre comillas y escribe el codigo municipio correspondiente segun la "i".
  #Los codigos municipio los hemos presentado al inicio del codigo.
  path <- httr::GET(url.data)
  Sys.sleep(1)
  data.json <- httr::content(path, as = "text")
  datos<- jsonlite::fromJSON(data.json) #Estas tres lineas son similares a las de siempre.
  Sys.sleep(1)
  #A PARTIR DE AQUI EL RAZONAMIENTO ES SIMILAR A ESPANYA, CA Y PROVINCIA SALVO ALGUN MATIZ QUE EXPLICAMOS
  datos$Nombre<-tolower(datos$Nombre)
  
  pos_Viaj_ResEsp<-which(grepl(pattern = "viajeros.*residentes en espa\u00F1a", x = datos$Nombre))
  Viaj_ResEsp<-datos$Data[[pos_Viaj_ResEsp]]$Valor
  
  pos_Viaj_ResExt<-which(grepl(pattern = "viajeros.*residentes en el extranjero", x = datos$Nombre))
  Viaj_ResExt<-datos$Data[[pos_Viaj_ResExt]]$Valor
  
  pos_per_ResEsp<-which(grepl(pattern = "pernoctaciones.*espa\u00F1a", x = datos$Nombre))
  per_ResEsp<-datos$Data[[pos_per_ResEsp]]$Valor
  
  pos_per_ResExt<-which(grepl(pattern = "pernoctaciones.*extranjero", x = datos$Nombre))
  per_ResExt<-datos$Data[[pos_per_ResExt]]$Valor
  
  arreglo<-paste0(mes,anyo)
  
  # Arreglo para Malaga: faltan los datos para mayo de 2020
  if("52020" %in% arreglo & i==3){
    pos.arreglo<-which(arreglo=="52020")
    if(pos.arreglo<length(Viaj_ResExt)){
      Viaj_ResExt<-c(Viaj_ResExt[1:(pos.arreglo-1)],NA,Viaj_ResExt[pos.arreglo:length(Viaj_ResExt)])
      per_ResExt<-c(per_ResExt[1:(pos.arreglo-1)],NA,per_ResExt[pos.arreglo:length(per_ResExt)])
    }else{
      Viaj_ResExt<-c(Viaj_ResExt[1:(pos.arreglo-1)],NA)
      per_ResExt<-c(per_ResExt[1:(pos.arreglo-1)],NA)
    }
  }
  
  Viaj_Total<-Viaj_ResEsp+Viaj_ResExt
  
  Per_Total<-per_ResEsp+per_ResExt
  
  est_med_esp <- per_ResEsp/Viaj_ResEsp
  est_med_ext <- per_ResExt/Viaj_ResExt
  est_med_total <- Per_Total/Viaj_Total
  
  anyo <- datos$Data[[1]]$Anyo
  mes <- datos$Data[[1]]$FK_Periodo
  

  
  DatosMunicipios<-data.frame(anyo,mes, municipios[i],Tipo[1], Viaj_Total,Viaj_ResEsp,Viaj_ResExt,Per_Total,per_ResEsp,per_ResExt,est_med_total,est_med_esp,est_med_ext)
  #Construimos DatosMuncicipios con la informacion relativa al municipio "i" con el que trabajamos.
  DatosDemanda1Municipio<-rbind(DatosDemanda1Municipio, DatosMunicipios)
  #Por ultimo vamos incorporando la informacion relativa a cada municipio al data.frame "DatosDemanda1Municipio".
  #Esto es facil de hacer con la funcion rbind, donde introducimos la matriz donde queremos cargar la informacion y la informacion que queremos cargar.
}

colnames(DatosDemanda1Municipio)<-c("A\u00F1o","Mes","Municipio","TipoEstablecimiento","Viaj_Total","Viaj_ResEsp","Viaj_ResExt","Pern_Total","Pern_ResEsp","Pern_ResExt","EstanciaMedia_Total","EstanciaMedia_ResEsp","EstanciaMedia_ResExt")



#TODA LA INFORMACION ANTERIOR ERA RELATIVA A DEMANDA. EN OFERTA TRABAJAMOS DE UNA MANERA MUY SIMILAR CAMBIANDO EL NOMBRE DE LAS VARIABLES.

#OFERTA ESPANYA
url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2066?tv=349:16473&nult=",last)
Sys.sleep(1)
path <- httr::GET(url.data)
data.json <- httr::content(path, as = "text")
datos<- jsonlite::fromJSON(data.json)
Sys.sleep(1)

datos$Nombre<-tolower(datos$Nombre)

Est_Abi<-which(grepl(pattern = "establecimientos.*abiertos", x = datos$Nombre))
NEst_Abi<-datos$Data[[Est_Abi]]$Valor #ESTABLECIMIENTOS ABIERTOS

Hab_Est<-which(grepl(pattern = "habitaciones.*estimadas", x = datos$Nombre))
NHab_Est<-datos$Data[[Hab_Est]]$Valor #HABITACIONES ESTIMADAS

Pla_Est<-which(grepl(pattern = "plazas.*estimadas", x = datos$Nombre))
NPla_Est<-datos$Data[[Pla_Est]]$Valor #PLAZAS ESTIMADAS

Pos_Ocu_Hab<-grep(pattern = "ocupaci\u00F3n por plazas\\.", x = datos$Nombre)
Pos_Ocu_Hab_FS<-grep(pattern = "ocupaci\u00F3n por plazas en", x = datos$Nombre)
Ocu_Plz<-datos$Data[[Pos_Ocu_Hab]]$Valor #OCUPACION POR PLAZAS
Ocu_Plz_FS<-datos$Data[[Pos_Ocu_Hab_FS]]$Valor #OCUPACION POR PLAZAS FIN DE SEMANA

Ocu_Hab<-which(grepl(pattern = "ocupaci\u00F3n.*habitaciones", x = datos$Nombre))
NOcu_Hab<-datos$Data[[Ocu_Hab]]$Valor #OCUPACION HABITACIONES

Per_Emp<-which(grepl(pattern = "personal.*empleado", x = datos$Nombre))
NPer_Empt<-datos$Data[[Per_Emp]]$Valor #CANTIDAD PERSONAL EMPLEADO

anyo <- datos$Data[[1]]$Anyo
mes <- datos$Data[[1]]$FK_Periodo
a <- c("")
b <- c("")
c <- c("")
#ESTOS TRES VECTORES SON RELATIVOS A INFORMACION PROCEDENTE DE APARTAMENTOS
#En concreto, apartamentos, ocupacion apartamentos y ocupacion apartamentos fin de semana respectivamente.

DatosOfertaEspana<-data.frame(anyo,mes,Tipo[1],NEst_Abi,NHab_Est,NPla_Est,Ocu_Plz,Ocu_Plz_FS,NOcu_Hab,NPer_Empt,a,b,c)
colnames(DatosOfertaEspana)<-c("A\u00F1o",	"Mes",	"TipoEstablecimiento",	"Establecimientos",	"Habitaciones_est",	"Plazas_est",	"GOcup_plazas",	"GOcup_plazas_finde",	"GOcup_habit",	"Personal",	"Apartamentos",	"GOcup_apart",	"GOcup_apart_finde")


#OFERTA CCAA
url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2066?tv=70:9006&nult=",last)
path <- httr::GET(url.data)
Sys.sleep(1)
data.json <- httr::content(path, as = "text")
datos<- jsonlite::fromJSON(data.json)
Sys.sleep(1)

datos$Nombre<-tolower(datos$Nombre)

Est_Abi<-which(grepl(pattern = "establecimientos.*abiertos", x = datos$Nombre))
NEst_Abi<-datos$Data[[Est_Abi]]$Valor

Hab_Est<-which(grepl(pattern = "habitaciones.*estimadas", x = datos$Nombre))
NHab_Est<-datos$Data[[Hab_Est]]$Valor

Pla_Est<-which(grepl(pattern = "plazas.*estimadas", x = datos$Nombre))
NPla_Est<-datos$Data[[Pla_Est]]$Valor

Pos_Ocu_Hab<-grep(pattern = "ocupaci\u00F3n por plazas\\.", x = datos$Nombre)
Pos_Ocu_Hab_FS<-grep(pattern = "ocupaci\u00F3n por plazas en", x = datos$Nombre)
Ocu_Plz<-datos$Data[[Pos_Ocu_Hab]]$Valor
Ocu_Plz_FS<-datos$Data[[Pos_Ocu_Hab_FS]]$Valor

Ocu_Hab<-which(grepl(pattern = "ocupaci\u00F3n.*habitaciones", x = datos$Nombre))
NOcu_Hab<-datos$Data[[Ocu_Hab]]$Valor

Per_Emp<-which(grepl(pattern = "personal.*empleado", x = datos$Nombre))
NPer_Empt<-datos$Data[[Per_Emp]]$Valor

anyo <- datos$Data[[1]]$Anyo
mes <- datos$Data[[1]]$FK_Periodo
a <- c("")
b <- c("")
c <- c("")

DatosOfertaCCAA<-data.frame(anyo,mes,CCAA,Tipo[1],NEst_Abi,NHab_Est,NPla_Est,Ocu_Plz,Ocu_Plz_FS,NOcu_Hab,NPer_Empt,a,b,c)
colnames(DatosOfertaCCAA)<-c("A\u00F1o","Mes","CCAA","TipoEstablecimiento","Establecimientos","Habitaciones_est","Plazas_est","GOcup_plazas","GOcup_plazas_finde","GOcup_habit","Personal","Apartamentos","GOcup_apart","GOcup_apart_finde")

#OFERTA PROVINCIA
url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2066?tv=115:46&nult=",last)
path <- httr::GET(url.data)
Sys.sleep(1)
data.json <- httr::content(path, as = "text")
datos<- jsonlite::fromJSON(data.json)
Sys.sleep(1)

datos$Nombre<-tolower(datos$Nombre)

Est_Abi<-which(grepl(pattern = "establecimientos.*abiertos", x = datos$Nombre))
NEst_Abi<-datos$Data[[Est_Abi]]$Valor

Hab_Est<-which(grepl(pattern = "habitaciones.*estimadas", x = datos$Nombre))
NHab_Est<-datos$Data[[Hab_Est]]$Valor

Pla_Est<-which(grepl(pattern = "plazas.*estimadas", x = datos$Nombre))
NPla_Est<-datos$Data[[Pla_Est]]$Valor

Pos_Ocu_Hab<-grep(pattern = "ocupaci\u00F3n por plazas\\.", x = datos$Nombre)
Pos_Ocu_Hab_FS<-grep(pattern = "ocupaci\u00F3n por plazas en", x = datos$Nombre)
Ocu_Plz<-datos$Data[[Pos_Ocu_Hab]]$Valor
Ocu_Plz_FS<-datos$Data[[Pos_Ocu_Hab_FS]]$Valor 

Ocu_Hab<-which(grepl(pattern = "ocupaci\u00F3n.*habitaciones", x = datos$Nombre))
NOcu_Hab<-datos$Data[[Ocu_Hab]]$Valor

Per_Emp<-which(grepl(pattern = "personal.*empleado", x = datos$Nombre))
NPer_Empt<-datos$Data[[Per_Emp]]$Valor

anyo <- datos$Data[[1]]$Anyo
mes <- datos$Data[[1]]$FK_Periodo
a <- c("")
b <- c("")
c <- c("")

DatosOfertaProvincia<-data.frame(anyo,mes,Provincia,Tipo[1],NEst_Abi,NHab_Est,NPla_Est,Ocu_Plz,Ocu_Plz_FS,NOcu_Hab,NPer_Empt,a,b,c)
colnames(DatosOfertaProvincia)<-c("A\u00F1o","Mes","Provincia","TipoEstablecimiento","Establecimientos","Habitaciones_est","Plazas_est","GOcup_plazas","GOcup_plazas_finde","GOcup_habit","Personal","Apartamentos","GOcup_apart","GOcup_apart_finde")


#OFERTA MUNICIPIOS
DatosOfertaMunicipio<-c()

for(i in 1:length(codigos_municipios_INE)){
  
url.data<-paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/2076?tv=103:",codigos_municipios_INE[i],"&nult=",last)  
path <- httr::GET(url.data)
Sys.sleep(1)
data.json <- httr::content(path, as = "text")
datos<- jsonlite::fromJSON(data.json)
Sys.sleep(1)
  
datos$Nombre<-tolower(datos$Nombre)
  
Est_Abi<-which(grepl(pattern = "establecimientos.*abiertos", x = datos$Nombre))
NEst_Abi<-datos$Data[[Est_Abi]]$Valor
  
Hab_Est<-which(grepl(pattern = "habitaciones.*estimadas", x = datos$Nombre))
NHab_Est<-datos$Data[[Hab_Est]]$Valor
  
Pla_Est<-which(grepl(pattern = "plazas.*estimadas", x = datos$Nombre))
NPla_Est<-datos$Data[[Pla_Est]]$Valor
  
Pos_Ocu_Hab<-grep(pattern = "ocupaci\u00F3n por plazas\\.", x = datos$Nombre)
Pos_Ocu_Hab_FS<-grep(pattern = "ocupaci\u00F3n por plazas en", x = datos$Nombre)
Ocu_Plz<-datos$Data[[Pos_Ocu_Hab]]$Valor
Ocu_Plz_FS<-datos$Data[[Pos_Ocu_Hab_FS]]$Valor 
  
Ocu_Hab<-which(grepl(pattern = "ocupaci\u00F3n.*habitaciones", x = datos$Nombre))
NOcu_Hab<-datos$Data[[Ocu_Hab]]$Valor
  
Per_Emp<-which(grepl(pattern = "personal.*empleado", x = datos$Nombre))
NPer_Empt<-datos$Data[[Per_Emp]]$Valor

anyo <- datos$Data[[1]]$Anyo
mes <- datos$Data[[1]]$FK_Periodo
a <- c("")
b <- c("")
c <- c("")
  
OfertaMunicipio<-data.frame(anyo,mes,municipios[i],Tipo[1],NEst_Abi,NHab_Est,NPla_Est,Ocu_Plz,Ocu_Plz_FS,NOcu_Hab,NPer_Empt,a,b,c)
DatosOfertaMunicipio<-rbind(DatosOfertaMunicipio, OfertaMunicipio)
}
colnames(DatosOfertaMunicipio)<-c("A\u00F1o","Mes","Municipio","TipoEstablecimiento","Establecimientos","Habitaciones_est","Plazas_est","GOcup_plazas","GOcup_plazas_finde","GOcup_habit","Personal","Apartamentos","GOcup_apart","GOcup_apart_finde")


# wb <- createWorkbook() #Por ultimo creamos el documento Excel donde guardar la informacion.
#Recordad la libreria definida al principio e imprescindible para el trabajo.
#
# addWorksheet(wb, "DatosDemanda1Espa\u00F1a")
# addWorksheet(wb, "DatosDemanda1CCAA")
# addWorksheet(wb, "DatosDemanda1Provincia")
# addWorksheet(wb, "DatosDemanda1Municipio")
# addWorksheet(wb, "DatosOfertaEspana")
# addWorksheet(wb, "DatosOfertaCCAA")
# addWorksheet(wb, "DatosOfertaProvincia")
# addWorksheet(wb, "DatosOfertaMunicipio")
#Creamos ocho hojas dentro del documento excel que hemos creado.
#En cada una de ellas escribiremos la informacion relativa a demanda y oferta de nuestros 4 objetos de estudio.

# writeData(wb, "DatosDemanda1Espa\u00F1a", DatosDemanda1Espana, startRow = 1, startCol = 1, rowNames = F)
# writeData(wb, "DatosDemanda1CCAA", DatosDemanda1CCAA, startRow = 1, startCol = 1, rowNames = F)
# writeData(wb, "DatosDemanda1Provincia", DatosDemanda1Provincia, startRow = 1, startCol = 1, rowNames = F)
# writeData(wb, "DatosDemanda1Municipio", DatosDemanda1Municipio, startRow = 1, startCol = 1, rowNames = F)
# writeData(wb, "DatosOfertaEspana", DatosOfertaEspana, startRow = 1, startCol = 1, rowNames = F)
# writeData(wb, "DatosOfertaCCAA", DatosOfertaCCAA, startRow = 1, startCol = 1, rowNames = F)
# writeData(wb, "DatosOfertaProvincia", DatosOfertaProvincia, startRow = 1, startCol = 1, rowNames = F)
# writeData(wb, "DatosOfertaMunicipio", DatosOfertaMunicipio, startRow = 1, startCol = 1, rowNames = F)
#Traspasamos la informacion de R a Excel especificando en que fila y columna queremos empezar.
#rowNames = False especifica que no queremos que aparezcan en excel los titulos de fila (1,2,3,...,)

# saveWorkbook(wb, file = "BD Encuesta de Ocupaci\u00F3n Hotelera del INE.xlsx", overwrite = TRUE)
#Por ultimos, guardamos el documento con el nombre indicado.
#Este se guardara en el escritorio establecido al comienzo del codigo.


if(dir.exists("Z:/")){

  FACT_VI_EOH_DEMTURPAIS<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosDemanda1Espana)), DatosDemanda1Espana)
  colnames(FACT_VI_EOH_DEMTURPAIS)<-c("FechaGrabaci\u00F3n", colnames(DatosDemanda1Espana))
  FACT_VI_EOH_DEMTURCCAA<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosDemanda1CCAA)), DatosDemanda1CCAA)
  colnames(FACT_VI_EOH_DEMTURCCAA)<-c("FechaGrabaci\u00F3n", colnames(DatosDemanda1CCAA))
  FACT_VI_EOH_DEMTURPROV<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosDemanda1Provincia)), DatosDemanda1Provincia)
  colnames(FACT_VI_EOH_DEMTURPROV)<-c("FechaGrabaci\u00F3n", colnames(DatosDemanda1Provincia))
  FACT_VI_EOH_DEMTURMUNI<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosDemanda1Municipio)), DatosDemanda1Municipio)
  colnames(FACT_VI_EOH_DEMTURMUNI)<-c("FechaGrabaci\u00F3n", colnames(DatosDemanda1Municipio))
  FACT_VI_EOH_OFTURIPAIS<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosOfertaEspana)), DatosOfertaEspana)
  colnames(FACT_VI_EOH_OFTURIPAIS)<-c("FechaGrabaci\u00F3n", colnames(DatosOfertaEspana))
  FACT_VI_EOH_OFTURICCAA<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosOfertaCCAA)), DatosOfertaCCAA)
  colnames(FACT_VI_EOH_OFTURICCAA)<-c("FechaGrabaci\u00F3n", colnames(DatosOfertaCCAA))
  FACT_VI_EOH_OFTURIPROV<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosOfertaProvincia)), DatosOfertaProvincia)
  colnames(FACT_VI_EOH_OFTURIPROV)<-c("FechaGrabaci\u00F3n", colnames(DatosOfertaProvincia))
  FACT_VI_EOH_OFTURIMUNI<-data.frame(rep(format(Sys.time(), "%d/%m/%Y %X"), nrow(DatosOfertaMunicipio)), DatosOfertaMunicipio)
  colnames(FACT_VI_EOH_OFTURIMUNI)<-c("FechaGrabaci\u00F3n", colnames(DatosOfertaMunicipio))


  write.table(x = FACT_VI_EOH_DEMTURPAIS, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_DEMTURPAIS.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")
  write.table(x = FACT_VI_EOH_DEMTURCCAA, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_DEMTURCCAA.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")
  write.table(x = FACT_VI_EOH_DEMTURPROV, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_DEMTURPROV.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")
  write.table(x = FACT_VI_EOH_DEMTURMUNI, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_DEMTURMUNI.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")
  write.table(x = FACT_VI_EOH_OFTURIPAIS, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_OFTURIPAIS.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")
  write.table(x = FACT_VI_EOH_OFTURICCAA, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_OFTURICCAA.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")
  write.table(x = FACT_VI_EOH_OFTURIPROV, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_OFTURIPROV.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")
  write.table(x = FACT_VI_EOH_OFTURIMUNI, file = "Z:/VI. Actividad Econ\u00F3mica/BD Encuesta ocupacion hotelera/FACT_VI_EOH_OFTURIMUNI.txt", sep = "#", row.names = F, quote = F, dec = ",", na = "")


  cat("\n\tOh my goodness! This is fantastic!")

}else{
  cat("\n\tQu\u00E9 pena! Este c\u00F3digo no est\u00E1 hecho para ti ya que no tienes acceso al disco Z:/")
}


}

