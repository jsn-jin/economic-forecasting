#********************************************************************************
# Date: 08/19/2020
# Comment(s): R code example for fitting/forecasting a seasonality to ts data.
# Data File(s): beer.csv, and housing.dat
#********************************************************************************
# Variable Definitions
# beer = monthly beer production in Australia from Jan 1956 - Aug 1995
# house = monthly housing starts from 1946 to 1993
#********************************************************************************

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
library(forecast)
require(stats)
library(seasonal)
library(fpp2)

# Basic Seasonality Example using 'tslm'
y = ts(rnorm(120, 0, 3) + 20 * sin(2 * pi * (1:120) / 12), frequency = 12)
fit1 = tslm(y ~ trend )
fit2 = tslm(y ~ season)
fit3 = tslm(y ~ trend + season)

quartz()
par(mfrow = c(3,1))
plot(y, main = "Time Series Data: Trend")
lines(fit1$fitted.values, col="red")
plot(y, main = "Time Series Data: Seasonality")
lines(fit2$fitted.values, col="red")
plot(y,main="Time Series Data: Trend + Seasonality")
lines(fit3$fitted.values, col="red")

#-------------------- BEER EXAMPLE --------------------
# Read in the beer data into a data file
beer = read.csv("beer.csv", header = T, dec = ",", sep = ";")
beer = ts(beer[,1], start = 1956, freq = 12)
t <- seq(1956, 1995 + 8/12, length = length(beer))
quartz()
plot(beer, xlab = "Year", ylab = "Monthly Beer Production")

# Lets look at the beer data once again:
lbeer = log(beer)
quartz()
plot(beer, xlab = "Year", ylab = "Monthly Beer Production")
quartz()
plot(lbeer, xlab = "Year", ylab = "Log(Beer Production)")

# Compare 3 different fit models:
fit1 = tslm(lbeer ~ trend )
fit2 = tslm(lbeer ~ season)
fit3 = tslm(lbeer ~ trend + season)

#--------------------- DUMMY TEST ---------------------
testmd1 = lm(lbeer~time(lbeer))
testmd1

dummy_trend = seq(1:length(beer)) # Create time dummy

testmd2 = lm(lbeer~dummy_trend)
testmd2

fit1

# Note: testmd2 and fit1 give the same coefficients.
# tslm uses time dummy

plot(lbeer, xlab = "Year", ylab = "Log(Beer Production)", lwd = 2, col = 'skyblue3')
lines(t, testmd1$fit, col = "red3", lwd = 5)
lines(t, testmd2$fit, col = "purple", lwd = 3) # same line
lines(t, fit1$fit, col = "green", lwd = 1) # same line

#-------------------------------------------------------

quartz()
par(mfrow = c(3,1))
plot(lbeer, main = "Time Series Data: Trend")
lines(fit1$fitted.values, col = "red")
plot(lbeer, main = "Time Series Data: Seasonality")
lines(fit2$fitted.values, col = "red")
plot(lbeer, main = "Time Series Data: Trend + Seasonality")
lines(fit3$fitted.values, col = "red")

AIC(fit1,fit2,fit3)
BIC(fit1,fit2,fit3)

# Compute forecasts based on the 3 fit models:
quartz()
par(mfrow = c(3,1))
plot(forecast(fit1, h = 60), main = "Model 1: Forecast Trend")
lines(fit1$fitted.values, col = "red")
plot(forecast(fit2, h = 60), main = "Model 2: Forecast Seasonality")
lines(fit2$fitted.values, col = "red")
plot(forecast(fit3, h = 60), main = "Model 3: Forecast Trend + Seasonality")
lines(fit3$fitted.values, col = "red")

# (Optional) The forecast above can be improved considerably via 'ets'
fit = ets(lbeer) # ExponenTial Smoothing state space model
quartz()
plot(fit)
accuracy(fit)
plot(forecast(fit, level = c(50, 80, 95)))

# Plot the seasonal factors:
quartz()
# jpeg("seasonal_factor.jpg", width = 600, height = 600)
fit = tslm(lbeer ~ season + 0)
plot(fit$coef, type = 'l', ylab = 'Seasonal Factors', xlab = "Season", lwd = 2, main = "Plot of Seasonal Factors")
# dev.off()

#---------------- Book Example: Housing Starts ----------------
house <- read.table("housing.dat")
house_ts <- ts(house[,1], start = 1946, freq = 12)
t <- seq(1946, 1993.12, length = length(house_ts))
quartz()
plot(house_ts)
quartz()
plot(house_ts[200:256], type = "l") # Zoom in

# Seasonal Decomposition of Time Series by Loess (STL)
quartz()
plot(stl(house_ts, s.window = "periodic"))
quartz()
plot(stl(house_ts, s.window = 11))

forecast(house_ts)
summary(forecast(house_ts))

fit1 = tslm(house_ts ~ trend )
fit2 = tslm(house_ts ~ season) 
fit3 = tslm(house_ts ~ trend + season)

quartz()
par(mfrow = c(3,1))
plot(forecast(fit1, h = 24), main = "Model 1: Forecast Trend Only")
lines(fit1$fitted.values, col = "red")
plot(forecast(fit2, h = 24), main = "Model 2: Forecast Seasonality Only")
lines(fit2$fitted.values, col = "red")
plot(forecast(fit3, h = 24), main = "Model 3: Forecast Trend + Seasonality")
lines(fit3$fitted.values, col = "red")
AIC(fit1,fit2,fit3)
BIC(fit1,fit2,fit3)

# Plot the seasonal factors:
quartz()
par(mfrow = c(2,1))
plot(fit2$coef, type = 'l', ylab = 'Seasonal Factors', xlab = "Season", lwd = 2, main = "Plot of Seasonal Factors")
hist(fit2$res, main = "Histogram of Residuals", col = "skyblue3")

# (Optional) We can improve the forecast using ets
fit = ets(house_ts)
quartz()
plot(fit)
accuracy(fit)
plot(forecast(fit, level = c(50,80,95), h = 12))

# -------------------- (Optional) -------------------- #
# Seasonality Decomposition using the library 'seasonal'
# See: https://otexts.com/fpp2/components.html

# Classical Decompositions: Multiplicative and Additive
elecequip %>% decompose(type = "multiplicative") %>%
  autoplot()

elecequip %>% decompose(type = "additive") %>%
  autoplot()

# More Advnaced Decomposition: X11
# Need to load seasonal package
elecequip %>% seas(x11 = "") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")

# STL
elecequip %>% stl(t.window = 13, s.window = "periodic", robust = TRUE) %>%
  autoplot()
  
plot(elecequip, col = "grey", lwd = 2, ylab = "Total")
lines(seasadj(fit), col = "blue", lwd = 2)
lines(trendcycle(fit), col = "red",lwd = 2)
legend(2000, 75, c("Data", "Seasonally Adjusted","Trend"), fill = c("black", "blue", "red"), cex = 1, bty = 'n')

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

# Forecast
fit <- stl(elecequip, t.window = 13, s.window = "periodic", robust = TRUE)
  
fit %>% seasadj() %>% naive() %>%
  autoplot() + 
  ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")
  
fit %>% forecast(method = "naive") %>% 
  autoplot() + 
  ylab("New orders index")

