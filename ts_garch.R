#***********************************************
# Date: 05/27/2016
# Comment(s): R code for fitting ARMA+GARCH models
# Data File(s): S&P500
#***********************************************

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

library(rugarch) 
library(tseries)
library(forecast)

# Specify an ARMA + GARCH Model
model = ugarchspec(
variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), 
mean.model = list(armaOrder = c(2, 2), include.mean = TRUE), 
distribution.model = "sstd")
# sGARCH: standard GARCH(2,2) for the volatility 
# arma: ARMA(2,2) for the returns
# sstd: standard normal

# Sanity check: explore the model parameters
# S4 object, use @ to access and modify slot
model@model$pars

# Get the S&P500 data (get Adjusted Prices, convert them to rerurns, and then to a data frame)
sp500.prices = get.hist.quote(instrument = "^GSPC",quote = "Adj",provider = c("yahoo"), method = NULL,origin = "1899-12-30", compression = "d",retclass = c("zoo"), quiet = FALSE, drop = FALSE)

plot(sp500.prices)
sp500 = as.data.frame(sp500.prices)
N = length(sp500[,1])sp500.returns = 100 * (log(sp500[2:N,]) - log(sp500[1:(N-1),]))

plot(sp500.returns, type = "l")

tsdisplay(sp500.returns)
tsdisplay(sp500.returns^2) # Strong Spikes

# Fit the model to the data
modelfit = ugarchfit(spec = model, data = sp500.returns)
modelfit
# GARCH Model	: sGARCH(2,2)
# Mean Model	: ARFIMA(2,0,2)
# Distribution	: sstd 
# mu     0.36587
# ar1    0.05878
# ar2    0.05061
# ma1    0.08637
# ma2    0.05164
# omega  0.34633
# alpha1 0.25372
# alpha2 0.52821
# beta1  0.90344
# beta2  0.95013

plot(modelfit)
# Note: you can pass the "which=" argument to the plot() function with the number of the graph.

# Forecast
modelfor = ugarchforecast(modelfit, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0)
# 1 or 3
plot(modelfor)