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
library(h2o)

###############################
# INIT H2O
localH2O = h2o.init()
###############################


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



####################
#        H2O
####################

#Carregar dados no H2O
#setwd("/Users/renato/Desktop")
#write.csv(df,"stock.csv", row.names = TRUE)
#Stock.hex <- h2o.uploadFile(path = "stock.csv", destination_frame = "Stock_new.hex")

# Dados de treino e teste
#stock.split <- h2o.splitFrame(data=Stock.hex, ratios=0.75)
#stock.train <- stock.split[[1]]
#stock.test <- stock.split[[2]]

# Gerar Modelo
#stock.glm <- h2o.glm(y = "close",
#                        x = c("closem1", "closem2", "closem3", "closem4","closem5"),
#                        training_frame=stock.train,
#                        family="gaussian",
#                        lambda = 0, compute_p_values = TRUE)

# Testar Modelo
#pred = h2o.predict(object=stock.glm, newdata=stock.test)
#summary(pred, exact_quantiles=TRUE)
#pd = as.matrix (pred)

# Gravar Modelo
#model_path <- h2o.saveModel(object=stock.glm, path=getwd(), force=TRUE)
#print(model_path)

# OU Carregar feito no Flow

h2o_df = as.h2o(df)

model <- h2o.loadModel(file.choose())

# Predict using loaded model DF
pred = h2o.predict(object=model, newdata=h2o_df)
pd = as.matrix (pred)

# Plot
plot(as.vector(Cl(symb)), type = "l")
lines(pd, col = "red")

#Accuracy
accuracy(as.vector(pd), Cl(symb))

########################



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
  #print(tail(df))
  
  # DF H2O
  h2o_df = as.h2o(df)

  # Previsao
  prevfut = h2o.predict(object=model, newdata=h2o_df)
  pd = as.matrix (prevfut)
  
  # Incluir na ultima linha
  ultimalinha = nrow(df)
  df[ultimalinha,1] <- last(pd)

  
}

future = data.frame(tail(df[,1],nfuture))
#future <- Date=as.Date(rownames(future))

print(tail(future,nfuture))

plot(as.vector(prevfut), type = "l")

