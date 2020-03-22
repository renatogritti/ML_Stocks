# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from pandas_datareader import data
import matplotlib as plt
import pandas as pd
from datetime import date, timedelta
import h2o

h2o.init()


stock = "BBAS3.SA"
start_date = "2004-01-01"
end_date = "2019-02-20"

panel_data = data.DataReader(stock, "yahoo", start_date, end_date)

df_stock = panel_data["Close"]
df_stock.describe()

stock = pd.DataFrame(df_stock)
stock["closem1"] = df_stock.shift()
stock["closem2"] = df_stock.shift(2)
stock["closem3"] = df_stock.shift(3)
stock["closem4"] = df_stock.shift(4)
stock["closem5"] = df_stock.shift(5)

stock.fillna(method ='bfill', inplace = True)


###################################################
# H2O
##################################################

h2o_df = h2o.H2OFrame(stock)
localmodelo = "/Users/renato/Desktop/Model"

modelo = h2o.load_model(localmodelo)

pred = modelo.predict(h2o_df)
predict = pred.as_data_frame()

real = stock["Close"]

plt.pyplot.plot(real, label = "Real")
plt.pyplot.plot(predict, label = "Prediction", color = "red")


