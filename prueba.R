
# -- Borrar todos los elementos del environment
rm(list=ls())
mdir <- getwd()
(library(tictoc))
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
#870df85bef319db8175cddfde64099d6-ecaeac19655a8a30b16a70e7e3421924
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
# -- numeric ------ # Your Account ID 
OA_Ai <- 1010047192278001
# -- character ---- # Your Token
OA_Ak <- "870df85bef319db8175cddfde64099d6-ecaeac19655a8a30b16a70e7e3421924"
Step4 <- OpenTrades(AccountType = OA_At,
                    AccountID = OA_Ai,
                    Token = OA_Ak,
                    Instrument = OA_In)
Step1 <- InstrumentsList(OA_At,OA_Ak,OA_Ai)
Step2 <- ActualPrice(OA_At,OA_Ak,OA_In)
OA_Ot <- "market" 
OA_Sd <- "buy"
OA_Ls <- 1 
Step3 <- NewOrder(AccountType = OA_At,
                  AccountID  = OA_Ai,
                  Token = OA_Ak,
                  OrderType  = OA_Ot,
                  Instrument = OA_In,
                  Count  = OA_Ls,
                  Side   = OA_Sd,
                  SL = trunc(Step2$Ask*0.95), # 5  % loss
                  TP = trunc(Step2$Ask*1.10), # 10 % Profit
                  TS = 100)    