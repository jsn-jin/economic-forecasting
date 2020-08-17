
#********************************************************************************
# Date: 08/17/2020
# Comment(s): R code example for fitting/forecasting a trend to ts data.
# Data File(s): labordata.dat
#********************************************************************************
# Variable Definitions
# male = labor force male participate rate = y (response variable)
# female = labor force female participate rate = y (response variable)
# t =  time (monthly observations from 1948-1991)
#********************************************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries 
library(forecast)
library(stats)
library(tis)

# NOTE: to add recession bands:
# Example: Presidents Approval Rating
# plot(presidents, type = 'n', ylab = "Presidents Approval Rating")
# nberShade()
# lines(presidents)

# Read in the data into a data file and attach names:
z <- read.table("labordata.dat")
names(z) <- c("male", "female", "total")
attach(z)

# Convert data to time series format:
male_ts <- ts(male, start = c(1948,1), freq = 12)
t <- seq(1948, 1991, length = length(male_ts)
ts.plot(male_ts)
# detach(z)


#-------------[1] TREND FITTING--------------

# Linear Fit
m1 = lm(male_ts ~ t)
quartz()
par(mfrow = c(2,1))
plot(male_ts, ylab = "Participation Rate (Male)", xlab = "Time", lwd = 2, col = 'skyblue3', ylim = c(75,84), xlim = c(1968,1995))
# The next commands adds the U.S. recession bands
nberShade()
lines(male_ts, lwd = 2, col = "skyblue3")
lines(t, m1$fit, col = "red3", lwd = 2)
plot(t, m1$res, ylab = "Residuals", type = 'l', xlab = "Time")
par(mfrow = c(1,1))

#-------------[*] TIME DUMMY TESTING--------------
mtest1 <- tslm(male_ts ~ trend)
mtest1

TIME <- seq(1:length(male_ts)) # time dummy
mtest2<- lm(male_ts ~ TIME)
mtest2

par(mfrow = c(1,1))
plot(male_ts, ylab = "Participation Rate (Male)", xlab = "Time", lwd = 2, col = 'skyblue3', ylim = c(75,84), xlim = c(1968,1995))
lines(t, m1$fit, col = "red3", lwd = 2)
lines(t, mtest$fit, col = "purple", lwd = 2) # same line
lines(t, mtest2$fit, col = "green", lwd = 2) # same line
#------------------------------------------------------------------------------------------

# Quadratic Fit
m2 = lm(male_ts ~ t + I(t^2))
quartz()
par(mfrow = c(2,1))
plot(male_ts, ylab = "Participation Rate (Male)", xlab = "Time", lwd = 2, col = 'skyblue3', ylim = c(75,84), xlim = c(1968,1995))
lines(t, m2$fit, col = "red3", lwd = 2)
plot(t, m2$res, ylab = "Residuals", type = 'l', xlab = "Time")
par(mfrow = c(2,1))

# Log-Linear Fit
m3 = lm(log(male_ts) ~ t)  
quartz()
par(mfrow = c(2,1))
plot(log(male_ts), ylab = "Participation Rate (Male)", xlab = "Time", lwd = 2, col = 'skyblue3', xlim = c(1968,1995))
lines(t, m3$fit, col = "red3", lwd = 2)
plot(t, m3$res, ylab = "Residuals", type = 'l', xlab = "Time")

# Exponential Fit
ds = data.frame(x = t, y = male_ts)
par(mfrow = c(2,1))
plot(male_ts, ylab = "Participation Rate (Male)", xlab = "Time", lwd = 2, col = 'skyblue3', ylim = c(75,84), xlim = c(1968,1995))
# lines(t,m1$fit, col = "green", lwd = 2)
m4 = nls(y ~ exp(a + b * t), data = ds, start = list(a = 0, b = 0))
lines(ds$x, predict(m4, list(x = ds$x)), col = "red3", lwd = 2)
plot(t, residuals(m4), ylab = "Residuals", type = 'l',xlab = "Time")
summary(m4)


#-------------[2] MODEL SELECTION--------------
# Compare models using AIC and BIC
AIC(m1,m2,m3,m4)
BIC(m1,m2,m3,m4)

quartz()
plot(stl(male_ts, s.window = "periodic")) # Seasonal + Trend + Remainder (cycles)
# stl() is super powerful


#-------------[3] TREND FORECASTING--------------
# Linear
tn = data.frame(t = seq(1992,1999))
pred = predict(lm(male_ts ~ t), tn, se.fit = TRUE)
# plot(c(male_ts,pred$fit), type = 'l', xlim = c(1940,2000))
pred.plim = predict(lm(male_ts ~ t), tn, level = 0.95, interval = "prediction")
pred.clim = predict(lm(male_ts ~ t), tn, level = 0.95, interval = "confidence")
plot(male_ts, ylab = "Participation Rate (Male)", xlab = "Time", lwd = 2, col = 'skyblue3', ylim = c(70,84), xlim = c(1968,1999))
matplot(tn$t, cbind(pred.clim, pred.plim[,-1]), lty = c(1,1,1,3,3), type = "l", lwd = 2, ylab = "predicted y", xlab = "Time", add = TRUE)

# dev.print(device = postscript, "tsfig.eps", width = 7, height = 7, horizontal = FALSE)
# dev.off()

# Quadratic
pred = predict(lm(male_ts ~ t + I(t^2)), tn, se.fit = TRUE)
# plot(c(male_ts,pred$fit), type = 'l', xlim = c(1940,2000))
pred.plim = predict(lm(male_ts ~ t + I(t^2)), tn, level = 0.95, interval = "prediction")
pred.clim = predict(lm(male_ts ~ t + I(t^2)), tn, level = 0.95, interval = "confidence")
plot(male_ts, ylab = "Participation Rate (Male)", xlab = "Time", lwd = 2, col = 'skyblue3', ylim = c(70,84), xlim = c(1968,1999))
matplot(tn$t, cbind(pred.clim, pred.plim[,-1]), lty = c(1,1,1,3,3), type = "l", lwd = 2, ylab = "predicted y", xlab = "Time", add = TRUE)


#-------------[4] Holt-Winters Filter--------------
hwfit <- HoltWinters(male_ts)
quartz()
hwpred <- predict(hwfit, 60, prediction.interval = TRUE, level = 0.5)
plot(hwfit, hwpred, ylab = "Participation Rate (Male)", xlab = "Time", xlim = c(1948,1999))
# lines(predict(hwfit, n.ahead = 60), lwd = 1.5, col = 'blue')
# plot(hwfit, ylab = "Participation Rate (Male)", xlab = "Time", lwd = 1, col = 'lack', xlim = c(1948,1999))
# lines(predict(hwfit, n.ahead = 60), lwd = 1.5, col = 'blue')

