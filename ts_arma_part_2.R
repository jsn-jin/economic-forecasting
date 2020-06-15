#***********************************************
# Date: 05/10/2015
# Comment(s): R code for forecasting ARMA(p,q) models.
# Data File(s): caemp.txt
#***********************************************
# Variable Definitions
# Canadian Employment Index (quarterly, seasonally adjusted starting from 1962)
#************************************************

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
library(forecast)

# Read in the data into a ts data file:
caemp = read.table("caemp.txt")
caemp_ts = ts(caemp, start = 1962.1, freq = 4)

# Forecast MA(q) Process
quartz()
plot(caemp_ts, xlab = 'Year', ylab = "Canadian Employment", lwd = 2, xlim = c(1960,2005))
grid()
ma4 = arima(caemp_ts, order = c(0,0,4))
ma4.pred = predict(ma4, n.ahead = 36)
lines(ma4.pred$pred, col = "red")
lines(ma4.pred$pred + ma4.pred$se, col = "blue")
lines(ma4.pred$pred - ma4.pred$se, col = "blue")
# You can also use Arima (case-sensitive)
ma4 = Arima(caemp_ts, order = c(0,0,4))
quartz()
plot(forecast(ma4, h = 36))

# Closer look at the forecast:
quartz()
plot(forecast(ma4, h = 36), xlim = c(1990,2005))
# Note 1: After 4-quarters ahead, the forecast reverts to the mean!
# Note 2: Forecasts more than 4-steps-ahead, are equal to the unconditional mean.
#         Recall that we can't forecast past 'q' for an MA(q) model.
# Note 3: The forcasts contradicts conventional economics, i.e., employment is not
#         expected to increase as strongly. This reflects the short-term memory
# 		  problem with the MA(q) process.

# Forecast AR(p) Process
quartz()
plot(caemp_ts, xlab = 'Year', ylab = "Canadian Employment", lwd = 2, xlim = c(1960,2005))
grid()
ar2 = ar(caemp_ts, FALSE, 2) 
ar2.pred = predict(ar2, n.ahead = 36)
lines(ar2.pred$pred, col = "red")
lines(ar2.pred$pred + ar2.pred$se, col = "blue")
lines(ar2.pred$pred - ar2.pred$se, col = "blue")
# You can also use Arima (case-sensitive)
ar2 = Arima(caemp_ts, order = c(2,0,0))
quartz()
plot(forecast(ar2, h = 36), xlim = c(1990,2005))

# Closer look at the forecast:
quartz()
plot(forecast(ar2, h = 36), xlim = c(1990,2005))
# Note 1: It takes the forecast a lot longer to revert to the unconditional mean!
# Note 2: It is better at capturing the dynamics (persistence) of the data.

# Forecast ARMA(p,q) Porcess
arma1 = Arima(caemp_ts, order = c(2,0,1))
summary(arma1)
quartz()
plot(forecast(arma1, h = 36), xlim = c(1990,2005))
# Note: Basically replicates the results from AR(2) since the
#       MA(q) contribution is negligible. 

# arma2 = Arima(caemp_ts, order = c(3,0,3))
# Note: MA coefficients are not statistically significant

