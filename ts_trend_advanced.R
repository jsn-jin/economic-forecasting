#********************************************************************************
# Date: 8/18/2020
# Comment(s): R code example for fitting/forecasting a trend to ts data.
# Data File(s): beer.csv
#********************************************************************************
# Variable Definitions
# beer = monthly beer production in Australia from Jan 1956 - Aug 1995
#********************************************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
library(forecast)
require(stats)
library(tis)

# Read in the data into a data file
beer = read.csv("beer.csv", header = T, dec = ",", sep = ";") # dec: decimal points
beer = ts(beer[,1], start = 1956, freq = 12)
plot(beer, xlab = "Year", ylab = "Monthly Beer Production")

#-----------------[1] TREND FITTING------------------

# Model 1: Log-quadratic model
lbeer <- log(beer)
t <- seq(1956, 1995 + 8/12, length = length(beer))
t2 <- t^2
quartz()
plot(lbeer, xlab = "Year", ylab = "Log(Beer Production)")
m1 = lm(lbeer ~ t + t2)
lines(t, m1$fit, col = "red3", lwd = 2)

# Model 2: Log-quadratic-periodic
sin.t <- sin(2 * pi * t)
cos.t <- cos(2 * pi * t)
quartz()
plot(lbeer, xlab = "Year", ylab = "Log(Beer Production)")
m2 = lm(lbeer ~ t + t2 + sin.t + cos.t)
lines(t, m2$fit, col = "red3", lwd = 2)

#-----------------[2] MODEL SELECTION------------------
# Compare models using AIC and BIC
AIC(m1,m2)
BIC(m1,m2)

#-----------------[3] Holt-Winters Filter------------------
HoltWinters(beer)

# Holt-Winters exponential smoothing with trend and additive seasonal component.

# Call:
# HoltWinters(x = beer)

# Smoothing parameters:	# optimized for/across the training sample
#  alpha: 0.07532444
#  beta : 0.07434971
#  gamma: 0.143887

# Coefficients:
#             [,1]
# a   146.20776022		# linear trend
# b    -0.05114957		# linear trend
# s1  -11.57020149		# seasonal factors
# s2   16.03979520		# ..
# s3   30.03272865
# s4   36.97419000
# s5   -0.92958782
# s6  -10.56341255
# s7    5.04340160
# s8  -10.01246264
# s9  -10.92703434
# s10 -24.04369122
# s11 -16.46863572	
# s12  -7.76995228		# seasonal factors

quartz()
plot(beer, xlab = "Year", ylab = "Beer Production")
lines(HoltWinters(beer)$fitted[,1], col = "red")

# Try Holt-Winters Prediction
beer.hw <- HoltWinters(beer)
predict(beer.hw, n.ahead = 12)
quartz()
plot(beer, xlim = c(1956,1999), xlab = "Year", ylab = "Beer Production")
lines(predict(beer.hw, n.ahead = 48), col = 2)

### -------------------- Compare Different Models (Optional) ------------------------ ###
# 3 Competing Models
# (1) Holt-Winters
beer.hw <- HoltWinters(beer)
plot(forecast(beer.hw))	# The band is very narrow

# (2) Auto Arima
auto.arima(beer)	# ARIMA(0,1,3)(1,1,2)[12] 
plot(forecast(auto.arima(beer)))		
# Looks very similar to Holt-Winters	. Could be very hard to decide.
							
# (3) ETS
plot(ets(beer))			# ETS(M,A,M) 
plot(forecast(ets(beer)))


accuracy(forecast(beer.hw))
accuracy(forecast(auto.arima(beer)))
accuracy(forecast(ets(beer)))	# error -> cycle in ARIMA
# MAPE: we will go for auto.arima with this one
# However, don't go by training performance. We have to look at the testing.
# auto.arima may overfit the training data.
# TODO: split up the data by training and testing.
# ---------------------------------------------------------------------------------------

# Forecast
quartz()
par(mfrow = c(2,1))
plot(beer, main = "Data", xlab = "Year", ylab = "Beer Production")
plot(forecast(beer), main = "Data with Respective Point and Interval Forecasts", xlab = "Year",  ylab = "Beer Production", shadecols = "oldstyle")
# For further info on parameters: http://www.inside-r.org/packages/cran/forecast/docs/plot.forecast

#-----------------[4] TS Anatomy Plot------------------
# Seasonal Decomposition of Time Series by Loess
quartz()
plot(stl(log(beer), s.window = "periodic"))
forecast(beer)
summary(forecast(beer))
