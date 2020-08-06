#***************************************************************************************
# Date: 08/06/2020
# Comment(s): ACF and PACF R code example.
# Data File(s): From Yahoo Finance use and stock/index, 
#				P2_3_data.txt
# Example Problems: From Forecasting for Economics and Business by Gloria Gonzalez-Rivera
#***************************************************************************************

# Clear all variables and prior sessions
# rm(list = ls(all = TRUE))

# Load Libraries
library(TTR)
library(quantmod)
library(tseries)
library(tis)
library(timeSeries)
library(zoo)
library(stats)
library(forecast)
library(dynlm)
library(PerformanceAnalytics)
library(MASS)

# Load the Data
# AAPL <- getReturns("AAPL", freq = "day")
getSymbols("AAPL")
getSymbols("MSFT")

quartz()
# chartSeries(AAPL$R, subset='last 3 months')
chartSeries(AAPL$AAPL.Adjusted, subset = 'last 3 months') # quantmod
addBBands() # Add Bollinger Bands to current chart, quantmod

chartSeries(MSFT$MSFT.Adjusted, subset = 'last 12 months')
addBBands() 
 
# Compare the following two plot and try to identify what is wrong in the first one.
quartz()
plot(AAPL)

quartz()
plot(AAPL$AAPL.Adjusted)

# Let's look at the ACF and PACF of the 
#	(a) Stock Prices vs 
#	(b) The Returns (using the 1st Difference)

# Note: I need to interpolate because 
# e.g., weekends and holidays return 'NAs' for the stock price.
ts_daily_prices <- get.hist.quote('AAPL', quote = "AdjClose", compression = "d", retclass = "ts") # tseries

# Need to load package "timeSeries"
# The following objects are masked from ‘package:tis’:
# description, interpNA
interp_ts_daily_prices <- interpNA(ts_daily_prices, method = "linear") # ts_daily_prices is an ts object

quartz()
par(mfrow = c(2,1))
acf(interp_ts_daily_prices, main = "ACF of Apple Prices")
acf(diff(interp_ts_daily_prices), main = "ACF of the First Difference of Apple Prices")

quartz()
par(mfrow = c(2,1))
pacf(interp_ts_daily_prices, main = "PACF of Apple Prices")
pacf(diff(interp_ts_daily_prices), main = "PACF of the First Difference of Apple Prices")

quartz()
tsdisplay(diff(interp_ts_daily_prices))

################### PROBLEM 2 #######################
###################  (2.3)    #######################
z = read.table('P2_3_data.txt', header = T)
# The data are quarteryl and start from 1950 Q2, therefore, we need to convert the data.
y = ts(z$gdprowth, start = c(1950,2), freq = 4)
x = ts(z$sp500returns, start = c(1950,2), freq = 4)
# Since there are 4 parts to this question (a-d), I'll lable each model accordingly,
# e.g., ma = model (part a), mb = model (part b), ...
plot(x,y)

# (a) 
ma = lm(y ~ x) # x_t vs. y_t: contemporaneous correlation
summary(ma)

# (b) 
mb = dynlm(y ~ L(x,1)) # dynamic linear model / L stands for the lag model
summary(mb)

# (c) 
mc = dynlm(y ~ L(x,1)+L(x,2)+L(x,3)+L(x,4))
summary(mc)

# (d) 
md = dynlm(y ~ L(x,1)+L(x,2)+L(x,3)+L(x,4)+L(y,1))
summary(md)

# Note:For model selection we should use AIC & BIC, and look at the residuals.
AIC(ma,mb,mc,md)
BIC(ma,mb,mc,md)
# It seems that md is the best model from the summaries above.

################### PROBLEM 3 #######################
###################  (3.7)    #######################

# (a) Compute the daily returns
getSymbols("^GSPC", from = "2006-01-02")

## To compute the regular returns (i.e., without the log)
returns = diff(GSPC[,6]) / lag(GSPC[,6]) * 100

## Note: A faster way to it is like this: 
returns = dailyReturn(GSPC[,6])

quartz()
plot(returns)

# Compute the returns according to the book instructions(CC return):
# Rt = ln(p_t) - ln(p_t-1)
log.returns = log(GSPC[,6]) - log(lag(GSPC[,6]))
quartz()
plot(log.returns)

acf(returns)
pacf(returns)

# (b) Compute the sample moments of the returns: mean, variance, skewness and kurtosis.Plot the historgram.
mean.log.returns = mean(log.returns$GSPC.Adjusted, na.rm = TRUE)
var.log.returns =  var(log.returns$GSPC.Adjusted, na.rm = TRUE)
skew.log.returns = skewness(log.returns$GSPC.Adjusted, na.rm = TRUE) 
kurt.log.returns = kurtosis(log.returns$GSPC.Adjusted, na.rm = TRUE) 

quartz()
truehist(log.returns, col = 'steelblue3', main = "Log of S&P 500 Returns", xlab = "log(Returns)")
# dev.print(device = postscript, "returns.eps", width = 7, height = 7, horizontal = FALSE)
# dev.off()

# (c) Plot Rt against Rt-1, Rt-2, Rt-3, Rt-4
Rt = log.returns$GSPC.Adjusted
Rt_1 = as.numeric(lag(Rt,1))
Rt_2 = as.numeric(lag(Rt,2))
Rt_3 = as.numeric(lag(Rt,3))
Rt_4 = as.numeric(lag(Rt,4))
Rt =as.numeric(Rt)
quartz()
par(mfcol=c(2,2))
plot(Rt_1,Rt,pch=20,col="skyblue4")
plot(Rt_2,Rt,pch=20,col="skyblue4")
plot(Rt_3,Rt,pch=20,col="skyblue4")
plot(Rt_4,Rt,pch=20,col="skyblue4")
# dev.print(device=postscript,"returnst.eps",width=7,height=7, horizontal=FALSE)
# dev.off()
