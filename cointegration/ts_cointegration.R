## package
library(tseries)
library(urca)
library(AER)
library(vars)

data("PepperPrice")
plot(PepperPrice, plot.type = "single", col = 1:2)
legend("topleft", c("black", "white"), bty = "n",col = 1:2, lty = rep(1,2))


## unit root tests
# Augmented Dickey-Fuller Test
# H0: non-stationary
# H1: stationary
adf.test(log(PepperPrice[, "white"]))
# data:  log(PepperPrice[, "white"])
# Dickey-Fuller = -1.744, Lag order = 6, p-value = 0.6838
# alternative hypothesis: stationary
# fail to reject the null
adf.test(log(PepperPrice[, "black"]))
# data:  log(PepperPrice[, "black"])
# Dickey-Fuller = -1.7002, Lag order = 6, p-value = 0.7023
# alternative hypothesis: stationary
# fail to reject the null


# Phillips-Perron Unit Root Test
# H0: non-stationary
# H1: stationary
pp.test(log(PepperPrice[, "white"]), type = "Z(t_alpha)")


# Elliott, Rothenberg \& Stock Unit Root Test
# H0: non-stationary
# H1: stationary
pepper_ers <- ur.ers(log(PepperPrice[, "white"]), type = "DF-GLS", model = "const", lag.max = 4)
summary(pepper_ers)


## stationarity tests
# Computes the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for the null hypothesis that x is level or trend stationary.
# H0: stationary
# H1: non-stationary
kpss.test(log(PepperPrice[, "white"]))
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for the null hypothesis that x is level or trend stationary.






model = lm(PepperPrice[, "white"] ~ PepperPrice[, "black"])
adf.test(model$residual) # Stationary





## cointegration

# Computes the Phillips-Ouliaris test for the null hypothesis that x is not cointegrated.
po.test(log(PepperPrice))

# Johansen Test for VAR
# Null: there is no cointegration

VARselect(PepperPrice) # K = 2

pepper_jo <- ca.jo(log(PepperPrice), ecdet = "const", type = "trace", K = 2)
summary(pepper_jo)

pepper_jo@teststat[2]
pepper_jo@teststat[1]
pepper_jo@cval

pepper_jo2 <- ca.jo(log(PepperPrice), ecdet = "const", type = "eigen", K = 2)
summary(pepper_jo2)

# 		   test   10pct  5pct   1pct
# r <= 1 | 3.66   7.52   9.24   12.97	<= I think this is wrong
# r = 0 	 | 17.26  17.85  19.96  24.60

# The null hypothesis of no cointegration is rejected ????; hence the Johansen test
# confirms the results from the initial two-step approach.









