#***********************************************
# Date: 05/10/2015
# Comment(s): R code for fitting VAR models
# Data File(s): housecomp.dat
#***********************************************
# Variable Definitions
# U.S. monthly seasonally adjusred housing starts and completion
# from 1968.01-1996.06
# housecomp.dat[,2] = housing starts
# housecomp.dat[,3] = hoising completion
#************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.

# Clear all variables and prior sessions
rm(list = ls(all = TRUE))

# Load Libraries
library(forecast)
library(vars) # VARselect()
library(tis)

# Look at the data
data = read.table("housecomp.dat")
starts = ts(data[,2], start = c(1968, 1), freq = 12)
comps = ts(data[,3], start = c(1968, 1), freq = 12)

quartz()
plot(starts, ylab = "Housing Starts and Completions")
nberShade()
lines(starts)
lines(comps, col = "blue")
legend("topright", legend = c("Starts", "Completions"), text.col = c("black", "blue"), bty = "n")


# Look at the ACF, PACF, and CCF (cross-correlation function)
quartz()
tsdisplay(starts, main = "Housing Starts")
quartz()
tsdisplay(comps, main = "Housing Completions")
quartz()
ccf(starts, comps, ylab = "Cross-Correlation Function", main = "Starts and Completions CCF")
# Completions are maximally correlated with starts lagged by 6-12 months.


# Fit a VAR(p) model to the data
# Note, we need to combine the variables into 1 data frame first:
y = cbind(starts, comps)
# y_ts = ts.union(starts, comps) # You can also use this function
# We need a dataframe
y_tot = data.frame(y)

# Run VARselect first. Identify the model.
VARselect(y_tot, lag.max = 10)
# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#      4      4      3      4 

# To fit a VAR(p) model, simply call 'VAR' and set p = value
y_model = VAR(y_tot, p = 4)
summary(y_model)
# We interpret the coefficients in the usual way, but now have a
# system of equations. For example, for VAR(1) we have:
# y1 = c11 y(1,t-1) + c12 y(2,t-1)
# y2 = c21 y(1,t-1) + c22 y(2,t-1)
# The ourtput from summary are cij, cov, and corr.

# Plot the fit and orginal data
quartz()
plot(y_model)
# pdf("varplot.jpg", width = 800, height = 800) 
# plot(y_model)
# dev.off() 

# Look at ACF and PACf
quartz()
par(mfrow=c(2,1))
acf(residuals(y_model)[,1])
pacf(residuals(y_model)[,1])

quartz()
par(mfrow=c(2,1))
acf(residuals(y_model)[,2])
pacf(residuals(y_model)[,2])

# comps = starts(t-k) + comps(t-k)
quartz()
tsdisplay(residuals(y_model)[,2],main = "comps = starts(t-k) + comps(t-k)")

# Impulse Response Function
irf(y_model)
# pdf("irfplot.jpg", width = 800, height = 800) 
quartz()
plot(irf(y_model, n.ahead = 36))
# Note: you can pass the "which=" argument to the plot() function with the number of the graph.
# dev.off() 

# Forecast
# holdout_matrix = hold out data
# var.predict = predict(object = y_model, n.ahead = 52, dumvar = holdout_matrix);
var.predict = predict(object = y_model, n.ahead = 52)
quartz()
plot(var.predict)
dev.print(device = postscript, "forecast.eps", width = 7, height = 7, horizontal = FALSE)
dev.off()

# Granger Test
# Does LA granger-cause Riverside?
grangertest(comps ~ starts, order = 4)

# Variance Decomposition (Forecast Error Variance Decomposition)
quartz()
plot(fevd(y_model, n.ahead = 5))
# How much of the variation is explained by the other series
# Completion doesn't help me anything in explaining Start
# As move to more distant horizons (from 1 to 5), Start starts to explain more Completions
# The Start influences the Completion series

# CUSUM Plot
quartz()
plot(stability(y_model, type = "Rec-CUSUM"), plot.type = "single")
# Not breaking / cross the bound
# Not bad
# Claim success

