
## <--------------------------------------------------------------------------->
## Example 1: ARMA(2, 1) model estimation.
## <--------------------------------------------------------------------------->
## This example shows how to fit an ARMA(2, 1) model using this Kalman
## filter implementation (see also stats' makeARIMA and KalmanRun).
library(FKF)
n <- 1000
## Set the AR parameters
ar1 <- 0.6
ar2 <- 0.2
ma1 <- -0.2
sigma <- sqrt(0.2)

## Sample from an ARMA(2, 1) process
a <- arima.sim(model = list(ar = c(ar1, ar2), ma = ma1), n = n,
innov = rnorm(n) * sigma)

## Create a state space representation out of the four ARMA parameters
arma21ss <- function(ar1, ar2, ma1, sigma) {
	Tt <- matrix(c(ar1, ar2, 1, 0), ncol = 2)
	Zt <- matrix(c(1, 0), ncol = 2)
	ct <- matrix(0)
	dt <- matrix(0, nrow = 2)
	GGt <- matrix(0)
	H <- matrix(c(1, ma1), nrow = 2) * sigma
	HHt <- H %*% t(H)
	a0 <- c(0, 0)
	P0 <- matrix(1e6, nrow = 2, ncol = 2)
	return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt,HHt = HHt))
}

## The objective function passed to 'optim'
objective <- function(theta, yt) {
	sp <- arma21ss(theta["ar1"], theta["ar2"], theta["ma1"], theta["sigma"])
	ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
	Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
	return(-ans$logLik)
}

theta <- c(ar = c(0, 0), ma1 = 0, sigma = 1)
fit <- optim(theta, objective, yt = rbind(a), hessian = TRUE)
fit

## Confidence intervals
rbind(fit$par - qnorm(0.975) * sqrt(diag(solve(fit$hessian))),
fit$par + qnorm(0.975) * sqrt(diag(solve(fit$hessian))))

## Filter the series with estimated parameter values
sp <- arma21ss(fit$par["ar1"], fit$par["ar2"], fit$par["ma1"], fit$par["sigma"])
ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = rbind(a))

## Compare the prediction with the realization
plot(ans, at.idx = 1, att.idx = NA, CI = NA)
lines(a, lty = "dotted")

## Compare the filtered series with the realization
plot(ans, at.idx = NA, att.idx = 1, CI = NA)
lines(a, lty = "dotted")

## Check whether the residuals are Gaussian
plot(ans, type = "resid.qq")

## Check for linear serial dependence through 'acf'
plot(ans, type = "acf")



## <--------------------------------------------------------------------------->
## Example 2: Local level model for the Nile's annual flow.
## <--------------------------------------------------------------------------->
## Transition equation:
## alpha[t+1] = alpha[t] + eta[t], eta[t] ~ N(0, HHt)
## Measurement equation:
## y[t] = alpha[t] + eps[t], eps[t] ~ N(0, GGt)
y <- Nile
y[c(3, 10)] <- NA # NA values can be handled

## Set constant parameters:
dt <- ct <- matrix(0)
Zt <- Tt <- matrix(1)
a0 <- y[1] # Estimation of the first year flow
P0 <- matrix(100) # Variance of 'a0'

## Estimate parameters:
fit.fkf <- optim(c(HHt = var(y, na.rm = TRUE) * .5,
GGt = var(y, na.rm = TRUE) * .5),
fn = function(par, ...)
-fkf(HHt = matrix(par[1]), GGt = matrix(par[2]), ...)$logLik,
yt = rbind(y), a0 = a0, P0 = P0, dt = dt, ct = ct,
Zt = Zt, Tt = Tt, check.input = FALSE)

## Filter Nile data with estimated parameters:
fkf.obj <- fkf(a0, P0, dt, ct, Tt, Zt, HHt = matrix(fit.fkf$par[1]),
GGt = matrix(fit.fkf$par[2]), yt = rbind(y))
## Compare with the stats' structural time series implementation:
fit.stats <- StructTS(y, type = "level")
fit.fkf$par
fit.stats$coef

## Plot the flow data together with fitted local levels:
plot(y, main = "Nile flow")
lines(fitted(fit.stats), col = "green")
lines(ts(fkf.obj$att[1, ], start = start(y), frequency = frequency(y)), col = "blue")
legend("top", c("Nile flow data", "Local level (StructTS)", "Local level (fkf)"),
col = c("black", "green", "blue"), lty = 1)

## <--------------------------------------------------------------------------->
## Example 3: Population Growth Rate
## <--------------------------------------------------------------------------->
# https://idontgetoutmuch.wordpress.com/2014/09/09/fun-with-extended-kalman-filters-4/

# Logistic growth function

logistG <- function(r, p, k, t){
  k * p * exp(r*t) / (k + p * (exp(r*t) - 1))
}

k <- 100
p0 <- 0.1*k
r <- 0.2
deltaT <- 0.1

# Let's create some sample data:
set.seed(12345)

obsVariance <- 25
nObs = 250
nu <- rnorm(nObs, mean=0, sd=sqrt(obsVariance)) 
pop <- c(p0, logistG(r, p0, k, (1:(nObs-1))*deltaT)) + nu

Estimate <- data.frame(Rate=rep(NA, nObs), Population = rep(NA,nObs))

library(numDeriv)

a <- function(x, k, deltaT){
  c(r = x[1],
    logistG(r = x[1], p = x[2], k, deltaT))
}
G <- t(c(0, 1))

# Evolution error
Q <- diag(c(0, 0))
# Observation error
R <- obsVariance
# Prior
x <- c(r, p0)
Sigma <- diag(c(144, 25))

for(i in 1:nObs){
  # Observation
  xobs <- c(0, pop[i])
  y <- G %*% xobs
  # Filter  
  SigTermInv <- solve(G %*% Sigma %*% t(G) + R)
  xf <- x + Sigma %*% t(G) %*%  SigTermInv %*% (y - G %*% x)
  Sigma <- Sigma - Sigma %*% t(G) %*% SigTermInv %*% G %*% Sigma 

  A <- jacobian(a, x=x, k=k, deltaT=deltaT)   
  K <- A %*% Sigma %*% t(G) %*% solve(G %*% Sigma %*% t(G) + R)
  Estimate[i,] <- x

  # Predict
  x <- a(x=xf, k=k, deltaT=deltaT) + K %*% (y - G %*% xf)
  Sigma <- A %*% Sigma %*% t(A) - K %*% G %*% Sigma %*% t(A) + Q
}

# Plot output
op <- par(mfrow = c(2,1))
time <- c(1:nObs) * deltaT
plot(y = pop, x = time, t = 'l', main = "Population growth", 
     xlab = "Time", ylab = "Population")
curve(logistG(r, p0, k, x), from = 0, to = max(time), col = 2, add = TRUE, lwd = 1)
lines(y = Estimate$Population, x = time, col = "orange", lwd = 2)
legend("bottomright", legend = c("Data", "Actual", "Estimate"), bty = "n", col = c("black", "red", "orange"), lty = 1, lwd = 2)

plot(y = Estimate$Rate, x = time, t = 'l', main = "Estimated growth rate", xlab = "Time", ylab = "Rate", col = "orange", lwd = 2)

abline(h = r, col = adjustcolor("red", alpha = 0.5), lwd = 2)

legend("topright", legend = c("Actual", "Estimate"), bty = "n", col = c("red", "orange"), lty = 1, lwd = 2)
par(op)


