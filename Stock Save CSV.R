if(!require(quantmod)) install.packages("quantmod")
if(!require(DMwR)) install.packages("DMwR")
if(!require(zoo)) install.packages("zoo")
if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")
if(!require(caret)) install.packages("caret")

library(quantmod)
library(DMwR)
library(zoo)
library(forecast)
library(tseries)
library(caret)


############################### ALTERAR
nfuture = 23
stock = "BBAS3.SA"


# Obter dados do YAHOO

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols(stock, from = Sys.Date()-3000,
           to = Sys.Date()-1 ,warnings = FALSE,
           auto.assign = TRUE)


################################# ALTERAR
names(BBAS3.SA) <- c('open','high','low','close','volume','adjusted')
symb = BBAS3.SA
datasymb = data.frame(Cl(symb))


# modelar base de dados Stock + Bovespa
df = data.frame(Cl(symb)) # closed
#df = cbind(df,Date = rownames(datasymb)) # Incluir Data
df['closem1'] = lag(Cl(symb),1) # ultimos 5 closes historico 
df['closem2'] = lag(Cl(symb),2)
df['closem3'] = lag(Cl(symb),3)
df['closem4'] = lag(Cl(symb),4)
df['closem5'] = lag(Cl(symb),5)

data.matrix(df)
df = na.fill(df,"extend") # preencher NA

setwd("/Users/renato/Desktop")
write.csv(df,"stock.csv", row.names = TRUE)
Stock.hex <- h2o.uploadFile(path = "stock.csv", destination_frame = "Stock_new.hex")

