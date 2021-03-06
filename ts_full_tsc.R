#**********************************************************************************************
# Date: 03/09/2020
# Comment(s): R code for forecasting Trend+Seasonal+Cycle models.
# Data File(s): liquor.dat
#**********************************************************************************************
# Variable Definitions
# Monthly U.S.liquor sales from 1968.01 - 1996.01
#**********************************************************************************************

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
library(stats)
library(forecast)
library(timeSeries)
library(tseries)
library(tis)

# Look at the data and log(data)
data = read.table("liquor.dat")
data_ts = ts(data[,1], start = c(1968, 1), freq = 12)
t = seq(1968, 1996, length = length(data_ts))
lgdata = log(data_ts)
quartz()
par(mfrow = c(2,1))
plot(data_ts, xlab = 'Time', ylab = "Sales", lwd = 2)
plot(lgdata, xlab = 'Time', ylab = "Log (Sales)", lwd = 2)
par(mfrow = c(1,1))

quartz()
plot(stl(lgdata, s.window = "periodic"))


#-------------[1] Model: Quadratic Trend --------------
# Fit a quadrtaic trend model
t2 = t^2
m1 = lm(lgdata ~ t + t2)
quartz()
par(mfrow = c(2,1))
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'skyblue3', xlim = c(1968,1995))
lines(t, m1$fit, col = "red3", lwd = 2)
plot(t, m1$res, ylab = "Residuals", type = 'l', xlab = "Time")

# Look at the ACF and PACF
quartz()
par(mfrow = c(2,1))
acf(m1$res,lag = 36, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(m1$res,lag = 36, main = "Residual Sample Partial Autocorrelations", xlab = "Displacement")


#-------------[2] Model: Quadratic Trend + Seasonality --------------
# Fit a quadrtaic trend + seasonality model (no y-intercept)
m2 = tslm(lgdata ~ 0 + t + t2 + season) # tslm from forecast package
quartz()
par(mfrow = c(2,1))
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'skyblue3')
lines(t, m2$fit, col = "red3", lwd = 2, lty = 2)
plot(t, m2$res, ylab = "Residuals", type = 'l', xlab = "Time", lwd = 2)

# Look at the ACF and PACF
quartz()
par(mfrow = c(2,1))
acf(m2$res, lag = 36, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(m2$res, lag = 36, main = "Residual Sample Partial Autocorrelations", xlab = "Displacement")

# -------------------------------------- TEST -------------------------------------- #
# Here we don't use the tslm method
# We instead we an S-AR(1) model
m2 = Arima(lgdata, xreg = cbind(t, t2), seasonal = list(order = c(1,0,0)))
quartz()
# jpeg("SAR_fit.jpg", width = 600, height = 600)
par(mfrow = c(2,1))
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'skyblue3')
lines(t, m2$fit, col = "red3", lwd = 2, lty = 2)
plot(t, m2$res, ylab = "Residuals", type = 'l', xlab = "Time", lwd = 2)
# dev.off()

# Look at the ACF and PACF
quartz()
# jpeg("acf.jpg", width = 600, height = 600)
par(mfrow = c(2,1))
acf(m2$res, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(m2$res, main = "Residual Sample Partial Autocorrelations", xlab = "Displacement")
# dev.off()

tsdisplay(m2$res)
#----------------------------------------------------------------------------------- #


# Summary of all four plots
quartz()
par(mfrow = c(2,2))
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'skyblue3', main = "log(data) + fit (trend +seasonality)")
lines(t, m2$fit, col = "red3", lwd = 1, lty = 2, main = "Residuals")
plot(t, m2$res, main = "Residuals", ylab = "Residuals", type = 'l', xlab = "Time", lwd = 2)
acf(m2$res, lag = 36, main = "Residual Sample ACF", xlab = "Displacement")
pacf(m2$res, lag = 36, main = "Residual Sample PACF", xlab = "Displacement")
# The residuals + ACF and PACF suggest cycles!


### Important: To check for Seasonality, we need to look at 
### the ACF and PACF but at higher lags (multiples of 12 for monthly obs, mult of 4 for quarterly, etc).
quartz()
par(mfrow = c(2,1))
acf(diff(lgdata, 12), lag = 336, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(diff(lgdata, 12), lag = 336, main = "Residual Sample Partial Autocorrelations",  xlab = "Displacement")

#-------------[3] Model: Quadratic Trend + Cycles --------------
m3 = arima(lgdata, order = c(3,0,0), xreg = cbind(t, t2))

quartz()
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'gray')
lines(fitted(m3), col = "red")
# xreg: trend
# Look at the ACF and PACF


#-------------[4] Model: Seasonality + Cycles --------------
m4 = arima(lgdata, order = c(3,0,0), seasonal = list(order = c(1,0,1)))
# Seasonal AR(1) and
# Seasonal MA(1)

quartz()
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'gray')
lines(fitted(m4), col = "green")

quartz()
par(mfrow = c(2,1))
acf(m4$res, lag = 336, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(m4$res, lag = 336, main = "Residual Sample Partial Autocorrelations", xlab = "Displacement")


#-------------[5] Model: Quadratic Trend + Seasonality + Cycles --------------
m5 = Arima(lgdata, order = c(3,0,0), xreg = cbind(t, t2), seasonal = list(order = c(1,0,1)))

quartz()
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'gray')
lines(fitted(m5), col = "black")

quartz()
par(mfrow = c(2,1))
acf(m5$res, lag = 336, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(m5$res, lag = 336, main = "Residual Sample Partial Autocorrelations", xlab = "Displacement")
# The ACF and PACF look better, but there's still room for improvement. Why??


#-------------[6] Model: Quadratic Trend + Seasonality + Cycles (Integrated) --------------
# Even better if we include 'I' in ARIMA
m6 = Arima(lgdata, order = c(3,1,0), xreg = cbind(t, t2), seasonal = list(order = c(1,0,1)))

quartz()
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'gray')
lines(fitted(m6), col = "red")

quartz()
par(mfrow = c(2,1))
acf(m6$res, lag = 336, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(m6$res, lag = 336, main = "Residual Sample Partial Autocorrelations", xlab = "Displacement")


# -------------------------------------- Forecasting --------------------------------------- #
# Now we can forecast safely

# 1. Forecast using your model:
m4 = Arima(lgdata, order = c(3,1,0), include.drift = TRUE, seasonal = list(order = c(1,0,1)))

quartz()
plot(lgdata, ylab = "Log (Sales)", xlab = "Time", lwd = 2, col = 'gray')
lines(fitted(m4), col = "green")

quartz()
par(mfrow = c(2,1))
acf(m4$res, lag = 336, main = "Residual Sample Autocorrelations", xlab = "Displacement")
pacf(m4$res, lag = 336, main = "Residual Sample Partial Autocorrelations", xlab = "Displacement")
quartz()

plot(forecast(m4, h = 36), shadecols = "oldstyle")


# 2. Let R figure it all out for you - lazy approach but very effective ;-) 
fit = auto.arima(lgdata)

quartz()
plot(forecast(fit, h = 36), shadecols = "oldstyle")


# -------------------------------------- Optional --------------------------------------- #
# Recursive Residuals:
library(strucchange)

# http://www.oga-lab.net/RGM2/func.php?rd_id=strucchange:recresid
# http://svitsrv25.epfl.ch/R-doc/library/strucchange/html/recresid.html
y = recresid(m4$res ~ 1)
quartz()
plot(y, pch = 16, ylab = "Recursive Residuals")
quartz()
plot(efp(m4$res ~ 1, type = "Rec-CUSUM"))

# Cross-Validation
library(cvTools)
library(boot)
library(rpart)
# Please example from: https://stat.ethz.ch/R-manual/R-devel/library/boot/html/cv.glm.html





