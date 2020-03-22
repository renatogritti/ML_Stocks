if(!require(neuralnet)) install.packages("neuralnet")
if(!require(quantmod)) install.packages("quantmod")
if(!require(DMwR)) install.packages("DMwR")
if(!require(zoo)) install.packages("zoo")
if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")
if(!require(caret)) install.packages("caret")

library(neuralnet)
library(quantmod)
library(DMwR)
library(zoo)
library(forecast)
library(tseries)
library(caret)


############################### ALTERAR
stock = "BBAS3.SA"
nfuture = 200



# Obter dados do YAHOO

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols(stock, from = '2014-01-01',
           to = Sys.Date() ,warnings = FALSE,
           auto.assign = TRUE)

################################# ALTERAR
names(BBAS3.SA) <- c('open','high','low','close','volume','adjusted')
symb = BBAS3.SA


# modelar base de dados
df = data.frame(Cl(symb)) # closed
df['closem1'] = lag(Cl(symb),1) # ultimos 5 closes historico 
df['closem2'] = lag(Cl(symb),2)
df['closem3'] = lag(Cl(symb),3)
df['closem4'] = lag(Cl(symb),4)
df['closem5'] = lag(Cl(symb),5)
df = na.fill(df,"extend") # preencher NA
print(tail(df))

# Dados de Treino e Dados de Teste
particao = createDataPartition(1:dim(df)[1],p=.7)
dadostreino = df[particao$Resample1,]
dadosteste = df[- particao$Resample1,]
dim(dadostreino)
dim(dadosteste)

# Normalizar
df_scale = scale(df)
#print(tail(df_scale))
dadostreino_scale = scale(dadostreino)
dadosteste_scale = scale(dadosteste)

# Modelo
nn = neuralnet(close ~ 
                 closem1 + closem2 + closem3 + closem4 + closem5, 
                 data=dadostreino_scale, hidden= c(6), threshold = 1, stepmax = 5000)
plot(nn)

# Teste para avaliar a acuracidade
teste = predict(nn,dadosteste_scale)
teste = unscale(teste,dadosteste_scale)
accuracy(as.vector(teste), dadosteste[,1])

# Previsao
prev = predict(nn , df_scale)

# DesNormalizar
prev = unscale(prev, df_scale)
#print(prev)

# Plot
plot(as.vector(Cl(symb)), type = "l")
lines(prev, col = "red")
accuracy(as.vector(prev), Cl(symb))

# Future

for (n in c(1:nfuture)) {
  
  m0 = c(last(df[,1]))
  m1 = c(last(df[,1]))
  m2 = c(last(df[,2]))
  m3 = c(last(df[,3]))
  m4 = c(last(df[,4]))
  m5 = c(last(df[,5]))
  
  newrow <- c(m0, m1, m2, m3, m4, m5)
  
  df = rbind (df, newrow, deparse.level = 0)
  print(tail(df))
  
  # Normalizar
  future_scale = scale(df)
  #print(tail(future_scale))
  

  # Previsao
  prevfut = predict(nn , future_scale)
  
  # DesNormalizar
  prevfut = unscale(prevfut, future_scale)
  print(tail(prevfut))
  
  # Incluir na ultima linha
  ultimalinha = nrow(df)
  df[ultimalinha,1] <- last(prevfut)

  
}

future = data.frame(tail(df[,1],nfuture))
#future <- Date=as.Date(rownames(future))

print(tail(future,nfuture))

plot(as.vector(prevfut), type = "l")

