# For details on "thief" seehttps://robjhyndman.com/hyndsight/thief/

library(forecast)
library(thief)
library(ggplot2)

fc <- thief(USAccDeaths)
autoplot(fc)
total <- AEdemand[,12]

# Construct all temporal aggregates
totalagg <- tsaggregates(total)
autoplot(totalagg, main = "Total Emergency Admissions")

base <- list()
for(i in seq_along(totalagg))
  base[[i]] <- forecast(auto.arima(totalagg[[i]]),
                 h = 2 * frequency(totalagg[[i]]), level = 80)

# Reconcile forecasts
reconciled <- reconcilethief(base)

# Plot original and reconciled forecasts
par(mfrow = c(2,3), mar = c(3,3,1,0))
for(i in 6:1){
	plot(reconciled[[i]], main = names(totalagg)[i], xlim = c(2010.5, 2017.5), flwd = 1)
	lines(base[[i]]$mean, col = "red")
}