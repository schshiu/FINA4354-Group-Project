# FINA 4354 Project
# Group 2
# Fu Xipeng          3035447805	
# Kong Wong Kai      3035478373
# Tan Zhini          3035478361
# Shiu Chung Haang   3035483653

rm(list = ls())
options(scipen = 999) # <- prevent using scientific notation

#-------------------------------------------------------------------------------

# 1 - Library preparation
# Check if client's computer has the library downloaded,
# then load the required library
list.of.library <- c('xts', 'quantmod', 'ggplot2', 'lubridate')
for (i in list.of.library) {
  print(i)
  if (i %in% rownames(installed.packages()) == FALSE) {
    install.packages(i, character.only = TRUE)
  }
  library(i, character.only = TRUE)
}

# Set working directory if needed
# setwd("~/")

rm(list.of.library, i) #Free up memory

#-------------------------------------------------------------------------------

# 2 - Data processing
# 2.1 - S&P500 data downloading
# S&P500 Index: Underlying
# 10 year for graph plotting, 3 year for parameter calculation
SP500.raw.full <- na.locf(getSymbols("^GSPC",
                                     from = Sys.Date() - years(10),
                                     auto.assign = FALSE))
# S&P500 Total Return Index:
SP500TR.raw <- na.locf(getSymbols("^SP500TR", 
                                  from = Sys.Date() - years(3),
                                  auto.assign = FALSE))
# S&P500 ETF: Hedging
SPY <- na.locf(getSymbols("SPY",
                          from = Sys.Date() - years(3),
                          auto.assign = FALSE))

#-------------------------------------------------------------------------------

# 2.2 - risk-free rate downloading
# We prepare the 1m, 3m, 6m, 1y version of risk-free rate
# If we change the tenor t, we can adopt a different RF rate below
#DGS1MO <- na.locf(getSymbols("DGS1MO", src = "FRED", auto.assign = FALSE))
#DGS3MO <- na.locf(getSymbols("DGS3MO", src = "FRED", auto.assign = FALSE))
DGS6MO <- na.locf(getSymbols("DGS6MO", src = "FRED", auto.assign = FALSE))
#DGS1YR <- na.locf(getSymbols("DGS1", src = "FRED", auto.assign = FALSE))

#-------------------------------------------------------------------------------

# 2.3 - Storing data to local repository
# change the xts into dataframe
data.path <- "./data"
write.csv(data.frame(row.names = index(SP500.raw.full), 
                     coredata(SP500.raw.full)),
          file = file.path(data.path, 'SP500.raw.full.csv'))
write.csv(data.frame(row.names = index(SP500TR.raw), coredata(SP500TR.raw)),
          file = file.path(data.path, 'SP500TR.raw.csv'))
write.csv(data.frame(row.names = index(SPY), coredata(SPY)),
          file = file.path(data.path, 'SPY.csv'))
write.csv(data.frame(row.names = index(DGS6MO), coredata(DGS6MO)),
          file = file.path(data.path, 'DGS6MO.csv'))

#-------------------------------------------------------------------------------

# 2.4 - Loading data from local repository

SP500.raw.full <- as.xts(read.csv(file = file.path(data.path, 
                                                   'SP500.raw.full.csv'),
                                  row.names = 1))
SP500TR.raw <- as.xts(read.csv(file = file.path(data.path, 'SP500TR.raw.csv'),
                               row.names = 1))
SPY <- as.xts(read.csv(file = file.path(data.path, 'SPY.csv'),
                       row.names = 1))
DGS6MO <- as.xts(read.csv(file = file.path(data.path, 'DGS6MO.csv'),
                          row.names = 1))

rm(data.path)

#-------------------------------------------------------------------------------

# 3 - Parameter setting
# 3.1 Find the dividend yield with S&P 500
# Get Dividend Yield approximation:
SP500.raw = window(SP500.raw.full, start = Sys.Date() - years(3))
SP500.DayRet <- dailyReturn(SP500.raw$GSPC.Adjusted, 
                            subset = NULL,
                            type = 'log')
SP500TR.DayRet <- dailyReturn(SP500TR.raw$SP500TR.Adjusted,
                              subset = NULL, 
                              type = 'log')
# Get daily return of 2020:
dividend.yield <- sum((SP500TR.DayRet['2020'] - SP500.DayRet['2020']) *
                        SP500.raw['2020',"GSPC.Adjusted"])/
  SP500.raw['2020-12-31',"GSPC.Adjusted"]
q <- as.numeric(coredata(dividend.yield[1]))

rm(SP500TR.DayRet)

#-------------------------------------------------------------------------------

# 3.2 - Find other parameters for the model
n <- nrow(DGS6MO)
# the last day's risk-free rate: note that the rate is in %
r <- as.numeric(coredata(DGS6MO$DGS6MO[n])) / 100

n <- nrow(SP500.raw)
# the last day's adjusted index:
S <- as.numeric(coredata(SP500.raw$GSPC.Adjusted[n]))
sigma <- as.numeric(sd(dailyReturn(SP500.raw$GSPC.Adjusted)) * sqrt(252))
t <- 0.5

rm(n)  #remove unused variables

#-------------------------------------------------------------------------------

# 3.3 - Find strike prices & step spans

miu <- as.numeric(mean(dailyReturn(SP500.raw$GSPC.Adjusted)))
# Total expected return in the tenor:
total.miu <- miu * t * 252
cat(total.miu)
# The total miu in the 6mo period is approx. 9%
# So, S&P500 investors generally expect a return of 9%

l1 <- 0.7      # Barrier level
l2 <- 0.85     # Strike level if DI European put triggered
# period of floating loss: l*S ~ g1*S
g1 <- 1 + total.miu             # g1*FV ~ g2*FV is the 1st step
g2 <- 1 + total.miu * 2         # g2*FV ~ g3*FV is the 2nd step
g3 <- 1 + total.miu * 3         # > g3*FV is the 3rd step (ceiling)

#-------------------------------------------------------------------------------

# 4 - Financial models
# 4.1 - Pricing Functions of options used
# S = Spot Price, K = Strike Price, L = Barrier Price
# r = Expected Return, q = Dividend Yield
# sigma = volatility, t = time to maturity

# Value of d1
fd1 <- function(S, K, r, q, sigma, t) {
  d1 <- (log(S / K) + (r - q + 0.5 * sigma ^ 2) * t) / (sigma * sqrt(t))
  return(d1) #d2 <- d1 - sigma*sqrt(t)
}

# Price of European call
fBS.call.price <- function(S, K, r, q, sigma, t) { 
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- S * exp(-q * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  return(price)
}

# Price of European put
fBS.put.price <- function(S, K, r, q, sigma, t) {
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- K * exp(-r * t) * pnorm(-d2) - S * exp(-q * t) * pnorm(-d1)
  return(price)
}

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Price of down-and-in barrier put option
# barrier (L) = l1 * S, strike (K) = l2 * S
fBS.DI.put.price <- function(S, K, L, r, q, sigma, t){
  const = (L / S) ^ (2 * (r-q) / (sigma ^ 2) - 1)
  # here r is replaced by r-q
  short.forward = exp(-r) * L - S
  # the discount process does not involve q
  price = short.forward + fBS.call.price(S, L, r, q, sigma, t) - 
    const * fBS.call.price(L ^ 2 / S, L, r, q, sigma, t)
  return(price)
}
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Price of digital call option
fBS.digital.call.price <- function(S, K, r, q, sigma, t) {
  price <- exp(-r * t) * pnorm(fd1(S, K, r, q, sigma, t) - sigma * sqrt(t))
  return(price)
}

#-------------------------------------------------------------------------------

# 4.2 - Price calculation of each option (and stock per se)
# long stock: S
# D&I barrier:
# DI.put <- fBS.DI.put.price(S, l1*S, l2*S, r, q, sigma, t)
DI.put <- 30
# short call at FV:
call <- fBS.call.price(S, g1*S, r, q, sigma, t)
# 1st digital call:
digital.one <- fBS.digital.call.price(S, g2*S, r, q, sigma, t)
# 2nd digital call:
digital.two <- fBS.digital.call.price(S, g3*S, r, q, sigma, t)

# Check the prices of the segments of the portfolio
cat(S, DI.put, call, digital.one, digital.two, '\n')

#-------------------------------------------------------------------------------

# 4.3 - Exploration of step sizes

# If we take total product price = S, then
# h3 = (call - DI.put) / (0.5 * digital.one + 0.5 * digital.two) / S
# Alternatively: total product price = 0.99 * S (1% price as transaction cost)
h3 = (call - DI.put - 0.01 * S) /
     (0.5 * digital.one + 0.5 * digital.two) / S
h2 = 0.5 * h3
cat("h2 =", h2, "\nh3 =", h3, "\n")

# check of equality
total.price = S + DI.put - call + h2 * S * digital.one + 
  (h3-h2) * S * digital.two
cat("total price =", total.price, "S =", S, "\n")
cat("total price / S =", total.price / S, "\n")

#-------------------------------------------------------------------------------

# 5 - Graph plotting
# 5.1 - S&P 500 market trend plot & return Q-Q plot

# Last 10 year S&P500 bar chart
barChart(SP500.raw.full, theme = "white", bar.type = "hlc")
# Last 3 years S&P500 daily return
qqnorm(SP500.DayRet)
rm(SP500.DayRet)

#-------------------------------------------------------------------------------

# 5.2 - Plot expected payoff graph
# 5.2.1 - Preparation

minprice <- 0
maxprice <- 2 * S
prices <- seq(minprice, maxprice, 1)
n <- length(prices)
rm(minprice, maxprice)

payoff.stock <- vector(mode = "numeric", n)
payoff.DI.put <- vector(mode = "numeric", n)
payoff.call <- vector(mode = "numeric", n)
# the two digital calls are in total amount
payoff.first.digital.call <- vector(mode = "numeric", n)
payoff.second.digital.call <- vector(mode = "numeric", n)

for (i in 1:n) {
  payoff.stock[i] = prices[i]
  # payoff.DI.put[i]: different in two cases
  payoff.call[i] = - max(prices[i] - g1*S, 0) #short call
  payoff.first.digital.call[i] = h2 * S * if(prices[i] > g2*S) 1 else 0
  payoff.second.digital.call[i] = (h3 - h2) * S * if(prices[i] > g3*S) 1 else 0
}

#-------------------------------------------------------------------------------

# 5.2.2 - Profit graph (split up) When the barrier is not triggered
payoff.DI.put = rep(0, times = n)

profit.stock <- payoff.stock - as.vector(S)
profit.DI.put <- payoff.DI.put - as.vector(DI.put)
profit.call <- payoff.call - as.vector(call)
profit.first.digital.call <- payoff.first.digital.call - as.vector(digital.one)
profit.second.digital.call <- 
  payoff.second.digital.call - as.vector(digital.two)

profit.overall <- rowSums(cbind(profit.stock,
                                profit.DI.put,
                                profit.call,
                                profit.first.digital.call,
                                profit.second.digital.call))

# Generate a data_frame all vectors
# in order to plot the strategy payoffs using ggplot
results <- data.frame(cbind(profit.stock,
                            profit.DI.put,
                            profit.call,
                            profit.first.digital.call,
                            profit.second.digital.call,
                            profit.overall))

# When the barrier is not triggered:
results.notrigger <- results
results.notrigger[1:floor(l1*S), ] <- NA

# please add points to S, l1*S, l2*S, etc
ggplot(results.notrigger, aes(x=prices)) + 
  geom_line(linetype = "dashed", aes(y = profit.stock, color = "Stock")) + 
  geom_line(linetype = "dashed", 
            aes(y = profit.DI.put, color = "DI European Put")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.call, color = "European Call")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.first.digital.call, color = "First Digital")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.second.digital.call, color = "Second Digital")) +
  geom_line(aes(y = profit.overall, color="Profit")) +
  scale_colour_manual("", 
                      breaks = c("Stock", "DI European Put", "European Call", 
                                 "First Digital", "Second Digital", "Profit"),
                      values = c("darkred", "darkorange", "violet",  
                                 "darkgreen", "darkblue", "black")) + 
  xlab("Underlying Price") +
  ylab("Profit") +
  ggtitle("Product Profit at t = T (Not Triggered")

#-------------------------------------------------------------------------------

# 5.2.3 - Profit graph (split up) When the barrier is triggered

# When the barrier is triggered:
for (i in 1:n) {
  payoff.DI.put[i] = max(l2 * S - prices[i], 0)
}

profit.stock <- payoff.stock - as.vector(S)
profit.DI.put <- payoff.DI.put - as.vector(DI.put)
profit.call <- payoff.call - as.vector(call)
profit.first.digital.call <- payoff.first.digital.call - as.vector(digital.one)
profit.second.digital.call <- 
  payoff.second.digital.call - as.vector(digital.two)

profit.overall <- rowSums(cbind(profit.stock,
                                profit.DI.put,
                                profit.call,
                                profit.first.digital.call,
                                profit.second.digital.call))

# Generate a data_frame all vectors
# in order to plot the strategy payoffs using ggplot
results <- data.frame(cbind(profit.stock,
                            profit.DI.put,
                            profit.call,
                            profit.first.digital.call,
                            profit.second.digital.call,
                            profit.overall))

# please add points to S, l1*S, l2*S, etc
ggplot(results, aes(x=prices)) + 
  geom_line(linetype = "dashed", aes(y = profit.stock, color = "Stock")) + 
  geom_line(linetype = "dashed", 
            aes(y = profit.DI.put, color = "DI European Put")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.call, color = "European Call")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.first.digital.call, color = "First Digital")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.second.digital.call, color = "Second Digital")) +
  geom_line(aes(y = profit.overall, color="Profit")) +
  scale_colour_manual("", 
                      breaks = c("Stock", "DI European Put", "European Call", 
                                 "First Digital", "Second Digital", "Profit"),
                      values = c("darkred", "darkorange", "violet",  
                                 "darkgreen", "darkblue", "black")) + 
  xlab("Underlying Price") +
  ylab("Profit") +
  ggtitle("Product Profit at t = T (Triggered)")

# cleaning up
rm(payoff.stock, payoff.DI.put, payoff.call,
   payoff.first.digital.call, payoff.second.digital.call, payoff.overall)
rm(profit.stock, profit.DI.put, profit.call, 
   profit.first.digital.call, profit.second.digital.call, profit.overall)
