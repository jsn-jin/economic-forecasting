#***********************************************
# Comment(s): R code for fitting MAPA (Multiple Aggregation Prediction Algorithm) Model
# Data File(s): housecomp.dat
#***********************************************
# Variable Definitions
# Original Data are from: http://www.freddiemac.com/finance/fmhpi/archive.html
# MSA (1975-Current), MSA=Metropolitan State Area
# hpi.xls[,1] = Los Angles County -House Price Index
# hpi.xls[,2] = Riverside County -House Price Index
# We will be using a new library called 'MAPA'
#************************************************

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
library(readxl)
library(MAPA)
set.seed(1)

data = read_excel("hpi.xls", col_names = FALSE)
LA_ts = ts(data[[1]], start = 1975, freq = 12)
RI_ts = ts(data[[2]], start = 1975, freq = 12)

plot(LA_ts)
lines(RI_ts, col = "red")

# Detailed view of the data at each temporal aggregation level (daily, monthly, quarterly, annual)
# Note: paral = 2 --> Run in parallel cluster mode with e.g., in my case 4 cores

png("mapa.png", width = 450, height = 450)
mapasimple(LA_ts, outplot = 2, paral = 2)
dev.off()

# Dynamic Fit to the data
# mapaest: MAPA Estimation
# Estimates MAPA and saves all fitted models e.g. k = 1 to k = 12
mapafit <- mapaest(LA_ts, paral = 2)
png("mapaets.png", width = 450, height = 450)
plot(mapafit)
dev.off()
# ETS (Error, Trend, Seasonal / ExponenTial Smoothing)
# N  (None)
# A  (Additive)
# Ad (Additive Damped)
# M  (Multiplicative)
# Md (Multiplicative Damped)
# For example ETS(M,A,N): Addtive trend method with multilicative errors
# For exmaple ETS(M,A,M): Holt-Winters

# MAPA Forecast
mapafor(y = LA_ts, mapafit = mapafit, ifh = 12, fh = 0)


# Fitted + Forecast with Error Bands
png("mapaall.png", width = 450, height = 450)
mapa(LA_ts, conf.lvl = c(0.8,0.9,0.95,0.99), outplot = 1)
dev.off()

