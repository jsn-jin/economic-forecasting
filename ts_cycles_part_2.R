#***********************************************
# Date: 04/22/2015
# Comment(s): R code example for characterizing cycles in ts data.
# Data File(s): The observations are generated via random walks.
#***********************************************

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Random Walk Simulation, Zt ~ N(0,1)
 
#1. Xt = Xt-1 + Zt (Random Walk)
x1 = e = rnorm(1000)
for (t in 2:1000) {
	x1[t] = x1[t-1] + e[t]
}
quartz()
tsdisplay(x1)
quartz()
acf(diff(x1))

#2. Xt = Zt (White Noise)
x2 = e = rnorm(1000)
for (t in 2:1000) {
	x2[t] = e[t]
}
quartz()
tsdisplay(x2) # White Noise
quartz()
acf(diff(x2))

#3. Xt = -0.3Xt-1 + Zt (AR1)
x3 = e = rnorm(1000)
for (t in 2:1000) {
	x3[t] = -0.3 * x3[t-1] + e[t]
}
quartz()
tsdisplay(x3)
quartz()
acf(diff(x3))

#3.1. Xt = 0.9Xt-1 + 0.8Xt-2 + Zt (AR2)
x3.1 = e = rnorm(1000)
for (t in 3:1000) {
	x3.1[t] = 0.8 * x3.1[t-1] + 0.8 * x3.1[t-2] + e[t]
}
quartz()
tsdisplay(x3.1)
quartz()
acf(diff(x3.1))

#4. Xt = sin(pi * t/3) + Zt
x4 = e = rnorm(1000)
for (t in 2:1000) {
	x4[t] = sin(pi * t/3) + e[t]
}
par(mfrow = c(3,1))
plot(x4, type = 'l', main = "Original Time Series of the Data")
acf(x4, main = "ACF")
acf(diff(x4), main = "ACF (First Difference)", ylab = expression(ACF(Delta)))

# Plot the autocorrelation, autocovariance, and partial autocorrelation functions
quartz()
par(mfrow = c(2,2))
plot(x4, type ='l')
acf(x4, type = "correlation")
acf(x4, type = "covariance")
acf(x4, type = "partial")

# Plot the autocorrlation function of the First and Second difference
quartz()
par(mfrow = c(2,1))
acf(diff(x4))
acf(diff(diff(x4)))



