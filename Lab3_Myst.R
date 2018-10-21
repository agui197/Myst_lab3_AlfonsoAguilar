# -- Borrar todos los elementos del environment
rm(list=ls())
mdir <- getwd()

# -- Establecer el sistema de medicion de la computadora
Sys.setlocale(category = "LC_ALL", locale = "")

# -- Huso horario
Sys.setenv(tz="America/Monterrey", TZ="America/Monterrey")
options(tz="America/Monterrey", TZ="America/Monterrey")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl","RMySQL", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 17
# Uso horario
OA_Ta <- "America/Mexico_City"
# Instrumento
OA_In <- "EUR_USD"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
OA_Pr <- "D"
# Multiplicador de precios para convertir a PIPS
MultPip_MT1 <- 10000

Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                           DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                           Instrument = OA_In, 
                           Start = NULL, End = NULL, Count = 900)
# calcular ganancias va a ser el precio de cierre menos el precio de apertura por 
# los 10,000 unidadeas que compraremos (para que el valor del pip sea 1$) por el 
# valor del pip 

######################
# Funcion de aceptacion de promedio movil
BorS <- function(Precio,npm,pond,t){
  pm   <- c()
  if(sum(pond*Precio[(t-npm):(t-1)])<Precio[t]){
    return(1)
  }
  else{
    return(0)
  }
}
######################

######################
# Funcion de trading
trading_PMP <- function(Historico,v,pond,pI,pO,cap){
  npm <- v
  Regla1_I <- pI   # Porcentaje de capital para comprar titulos para posicion Inicial.
  Regla2_P <- pO   # Se utiliza el P% del L capital restante en cada compra.
  Regla3_W <- Precios_Oanda # Se realiza la misma estrategia para todos los activos en el portafolio.
  Regla4_C <- 0.0025 # Comisiones pagadas por compra.
  Regla5_K <- cap # Capital Inicial.
  # -- El rendimiento de capital en el tiempo 1 es 0
  Historico$R_Cuenta[1] <- 0
  
  # -- Calcular R_Precio
  Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)
  
  ##inicializacion de la cartera
  Historico$Titulos[npm] <- (Regla5_K*Regla1_I)%/%Historico$Precio[npm]
  
  Historico$Titulos_a[npm]<-Historico$Titulos[npm]
  
  # -- Se calculan comisiones iniciales
  Historico$Comisiones[npm] <- Historico$Titulos[npm]*Historico$Precio[npm]*Regla4_C
  Historico$Comisiones_a[npm] <- Historico$Comisiones[npm]
  
  # --- Calcular el valor flotante de la posicion
  Historico$Flotante[npm] <- Historico$Titulos_a[npm]*Historico$Precio[npm]
  
  # -- Todo remanente se dejar? registrado en la cuenta de efectivo.
  Historico$Capital[npm] <- Regla5_K-Historico$Flotante[npm]-Historico$Comisiones[npm]
  
  # -- Calcular el Balance
  Historico$Balance[npm] <- Historico$Flotante[npm]+Historico$Capital[npm]
  
  # -- Iniciamos con una postura de mantener.
  Historico$Operacion[npm] <- "Posicion Inicial"
  
  
  # -- Mensaje inicial
  Historico$Mensaje[npm] <- "Inicializacion de cartera"
  
  
  for(i in (npm+1):(length(Historico$Date))){
    Historico$R_Activo[i] <- round((Historico$Precio[i]/Historico$Precio[1])-1,2)
    if(BorS(Historico$Precio,npm,pond,i)){ #se activa una se??al
      Historico$Capital[i] <- Historico$Capital[i-1]
      if(Historico$Capital[i] > 0){ # Si hay capital
        if(Historico$Capital[i] > 0){#Historico$Precio[i]*Historico$Capital[i]*Regla2_P){ # Si Capital minimo
          Historico$Operacion[i] <- "Compra"
          Historico$Titulos[i]   <- (Historico$Capital[i]*Regla2_P)%/%Historico$Precio[i]
          # tomando en cuenta que se compra la maxima cantidad de titulos que se tiene
          #Historico$Titulos[i] <- floor(Historico$Capital[i]/((1+Regla4_C)*Historico$Precio[i]))
          compra <- Historico$Precio[i]*Historico$Titulos[i]  
          Historico$Comisiones[i] <- compra*Regla4_C
          Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]+Historico$Comisiones[i]
          Historico$Titulos_a[i] <- Historico$Titulos[i-1]+Historico$Titulos[i]
          Historico$Capital[i]<-Historico$Capital[i-1]-compra-Historico$Comisiones[i]
          Historico$Titulos_a[i]<-Historico$Titulos[i]+Historico$Titulos_a[i-1]
          Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
          Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i]
          Historico$Mensaje[i] <- "Se hizo una compra"
          Historico$R_Cuenta[i]<-Historico$Balance[i]/Regla5_K-1
        }
        else{
          Historico$Operacion[i] <- "Compra"
          Historico$Capital[i]<-Historico$Capital[i-1]
          Historico$Titulos[i] <-0
          Historico$Comisiones[i] <-0
          Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]
          Historico$Titulos_a[i]<-Historico$Titulos[i]+Historico$Titulos_a[i-1]
          Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
          Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i]
          Historico$Mensaje[i] <- "Capital insuficiente para cubrir la regla"
          Historico$R_Cuenta[i]<-Historico$Balance[i]/Regla5_K-1
        }
      }
      else { # No hubo capital
        Historico$Operacion[i] <- "Compra"
        Historico$Capital[i]<-Historico$Capital[i-1]
        Historico$Titulos[i] <-0
        Historico$Comisiones[i] <-0
        Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]
        Historico$Titulos_a[i]<-Historico$Titulos[i]+Historico$Titulos_a[i-1]
        Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
        Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i]
        Historico$Mensaje[i] <- "Capital insuficiente"
        Historico$R_Cuenta[i]<-Historico$Balance[i]/Regla5_K-1
      }
    }
    else{
      if(Historico$Titulos_a[i-1] > 0){ #Si hay acciones para vender
        Historico$Operacion[i] <- "Venta"
        Historico$Titulos[i] <- Historico$Titulos_a[i-1]
        # considerando que se venden todos los titulos
        #Historico$Titulos[i] <- floor(Historico$Titulos_a[i-1]*Regla2_P)
        venta <- Historico$Precio[i]*Historico$Titulos[i]
        Historico$Comisiones[i] <- venta*Regla4_C
        Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]+Historico$Comisiones[i]
        Historico$Capital[i]<-Historico$Capital[i-1]+venta-Historico$Comisiones[i]
        Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
        Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i]
        Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]-Historico$Titulos[i] 
        Historico$Mensaje[i] <- "Se hizo una venta"
        Historico$R_Cuenta[i]<-Historico$Balance[i]/Regla5_K-1
      }
      else{
        Historico$Operacion[i] <- "Venta"
        Historico$Mensaje[i] <- "Activos insuficientes"
        Historico$Capital[i]<-Historico$Capital[i-1]
        Historico$Titulos[i] <-0
        Historico$Titulos_a[i]<-Historico$Titulos[i]+Historico$Titulos_a[i-1]
        Historico$Comisiones[i] <-0
        Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]+Historico$Comisiones[i]
        
        Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
        Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i]
        Historico$R_Cuenta[i]<-Historico$Balance[i]/Regla5_K-1
      }
    }
  }
  result<-Historico$R_Cuenta[900]
  return(result)
}
######################




pI=.2
pO=.15
cap=1000000

#Creacion del data frame
Historico   <- c()
Historico <- data.frame("Date" = row.names(Precios_Oanda),
                        "Precio" = Precios_Oanda$Close, 
                        "R_Precio" = 0, 
                        "R_Activo" = 0,
                        "R_Cuenta" = 0, 
                        "Capital" = 0,"Flotante" = 0, "Balance" = 0, "Titulos" = 0,
                        "Titulos_a" = 0,
                        "Operacion" = NA, "Comisiones" = 0,"Comisiones_a" = 0, "Mensaje" = NA)
result<-c()
for(v in 1:30){
  pondt <- runif(v, min=0, max=1)
  pond <- pondt/sum(pondt)
  result[v]<-trading_PMP(Historico,v,pond,pI,pO,cap)
}




### Visualizar el promedio movil
pm<-c()
for(i in (npm+1):(length(Historico$Date))){
  pm[i-npm]<- sum(pond*Historico$Precio[(i-npm):(i-1)]) 
}












