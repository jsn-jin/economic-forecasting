#### Co-integration and pairs trading

# Source: http://faculty.chicagobooth.edu/ruey.tsay/teaching/bs41202/sp2017/lec10-17.pdf

# Another good example of pairs trading can be found here: 
# https://www.quantstart.com/articles/Cointegrated-Augmented-Dickey-Fuller-Test-for-Pairs-Trading-Evaluation-in-R

library(urca) # for ca.jo()
library(forecast)
require(stats)

da = read.table("d-bhp0206.txt", header = T)
da1 = read.table("d-vale0206.txt", header = T)
bhp = log(da$adjclose)
vale = log(da1$adjclose)
bhp1 = ts(bhp, frequency = 252, start = c(2002,127))
vale1 = ts(vale, frequency = 252, start = c(2002,127))

# Plot the two series
quartz()
plot(bhp1, type = 'l', col = "blue", ylim = c(0.45,3.7), ylab = "Stock Price ($)", main = "Pairs Trading Example")
lines(vale1, type = 'l', col = "red")
legend(2003, 3.5, c("BHP", "Vale"), fill = c("blue", "red"), cex = 1.2, bty = 'n')

quartz()
plot((bhp1 - mean(bhp1)) / sd(bhp1), type = 'l', col = "blue", ylab = "Z", main = "Pairs Trading Example (Standardized)")
lines((vale1 - mean(vale1)) / sd(vale1), type = 'l', col = "red")
legend(x = "topleft", c("BHP", "Vale"), fill = c("blue", "red"), cex = 1.2, bty = 'n')

# Try a simple regression of S1 on S2
mreg = lm(bhp ~ vale)
summary(mreg)

# Perform the Johansen Test
x = cbind(bhp, vale)
m1 = ar(x)
m1$order # 2
m2 = ca.jo(x, K = 2) # 1 and -\gamma
summary(m2)
m3 = ca.jo(x, K = 2, type = c("trace"))
summary(m3)


# Compute the co-intergration expression
wt = bhp1 - 0.718 * vale1
quartz()
plot(wt, type = 'l', ylab = "wt = BHP - 0.72Vale", main = "Co-integration Relation")
abline(h = c(1.82), col = "red")
legend(2003, 1.95, c("wt", "E[wt] = 1.82"), fill = c("black", "red"))

quartz()
tsdisplay(wt)


# Using ETS
quartz()
plot(forecast(wt)) # From ETS
abline(h = c(1.82), col = "red")


