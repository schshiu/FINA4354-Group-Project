# FINA 4354 Project
# Group 2
# Fu Xipeng          3035447805	
# Kong Wong Kai      3035478373
# Tan Zhini          3035478361
# Shiu Chung Haang   3035483653

rm(list = ls())
options(scipen = 999) # <- prevent using scientific notation

#===============================================================================

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
# !!! In the following code, we assume the WD is
#     the "code" folder under repository root !!!

rm(list.of.library, i) #Free up memory

#===============================================================================

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

# RDS file saving:
data.path <- "../data"
saveRDS(SP500.raw.full, file = file.path(data.path, 'SP500.raw.full.rds'))
saveRDS(SP500TR.raw, file = file.path(data.path, 'SP500TR.raw.rds'))
saveRDS(SPY, file = file.path(data.path, 'SPY.rds'))
saveRDS(DGS6MO, file = file.path(data.path, 'DGS6MO.rds'))

# CSV saving (more visible data):
# change the xts into dataframe
data.path <- "../data"
write.csv(data.frame(row.names = index(SP500.raw.full), 
                     coredata(SP500.raw.full)),
          file = file.path(data.path, 'SP500.raw.full.csv'))
write.csv(data.frame(row.names = index(SP500TR.raw), coredata(SP500TR.raw)),
          file = file.path(data.path, 'SP500TR.raw.csv'))
write.csv(data.frame(row.names = index(SPY), coredata(SPY)),
          file = file.path(data.path, 'SPY.csv'))
write.csv(data.frame(row.names = index(DGS6MO), coredata(DGS6MO)),
          file = file.path(data.path, 'DGS6MO.csv'))

rm(data.path)

#-------------------------------------------------------------------------------

# 2.4 - Loading data from local repository

# RDS file loading:
data.path <- "../data"
SP500.raw.full <- readRDS(file = file.path(data.path, 'SP500.raw.full.rds'))
SP500TR.raw <- readRDS(file = file.path(data.path, 'SP500TR.raw.rds'))
SPY <- readRDS(file = file.path(data.path, 'SPY.rds'))
DGS6MO <- readRDS(file = file.path(data.path, 'DGS6MO.rds'))

# CSV file loading:
data.path <- "../data"
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

#===============================================================================

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

cat("Approximated Dividend Yield(q):", 100 * q, "%\n")
rm(SP500TR.DayRet)

#-------------------------------------------------------------------------------

# 3.2 - Find other parameters for the model
n <- nrow(DGS6MO)
# the last day's risk-free rate: note that the rate is in %
r <- as.numeric(coredata(DGS6MO$DGS6MO[n])) / 100
cat("Risk-free rate(r):", 100 * r, "%\n")

n <- nrow(SP500.raw)
# the last day's adjusted index:
S <- as.numeric(coredata(SP500.raw$GSPC.Adjusted[n]))
sigma <- as.numeric(sd(dailyReturn(SP500.raw$GSPC.Adjusted)) * sqrt(252))
t <- 0.5  # tenor
cat("Current S&P500 index(S):", S, "\n")
cat("Volatility(sigma):", sigma, "\n")
cat("Tenor(t):", t, "year(s)\n")

rm(n)  #remove unused variables

#-------------------------------------------------------------------------------

# 3.3 - Find strike prices & step ranges

miu <- as.numeric(mean(dailyReturn(SP500.raw$GSPC.Adjusted)))
# Total expected return in the tenor:
total.miu <- miu * t * 252
cat("Expected return during tenor t =", t, ":", total.miu, "\n")

# period of floating loss: l*S ~ g1*S
l1 <- 0.70    # Barrier level
l2 <- 0.85    # Strike level if DI European put triggered
g1 <- 1 + total.miu             # g1*FV ~ g2*FV is the 1st step
g2 <- 1 + total.miu * 2         # g2*FV ~ g3*FV is the 2nd step
g3 <- 1 + total.miu * 3         # > g3*FV is the 3rd step (ceiling)
cat("l1 =", l1, "\t", "l2 =", l2, "\n")
cat("g1 =", g1, "\t", "g2 =", g2, "\t", "g3 =", g3, "\n")

#===============================================================================

# 4 - Financial models
# 4.1 - Pricing Functions of options used
# S = Spot Price, K = Strike Price, L = Barrier Price
# r = Expected Return, q = Dividend Yield
# sigma = volatility, t = time to maturity

# Value of d1
fd1 <- function(S, K, r, q, sigma, t) {
  d1 <- (log(S / K) + (r - q + 0.5 * sigma ^ 2) * t) / (sigma * sqrt(t))
  return(d1)
}

# Value of d2
fd2 <- function(S, K, r, q, sigma, t) {
  d2 <- fd1(S, K, r, q, sigma, t) - sigma * sqrt(t)
  return(d2)
}

# Price of European call
fBS.call.price <- function(S, K, r, q, sigma, t) { 
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- fd2(S, K, r, q, sigma, t)
  price <- S * exp(-q * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  return(price)
}

# Price of European put
fBS.put.price <- function(S, K, r, q, sigma, t) {
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- fd2(S, K, r, q, sigma, t)
  price <- K * exp(-r * t) * pnorm(-d2) - S * exp(-q * t) * pnorm(-d1)
  return(price)
}

# Price of Down-and-In Barrier Put Option
# Barrier (L) = l1 * S, Strike (K) = l2 * S
fBS.DI.put.price <- function(S, K, L, r, q, sigma, t) {
  const <- (L / S) ^ (2 * (r - q - sigma ^ 2 /2) / (sigma ^ 2))
  price <- const * 
    (fBS.call.price(L ^ 2/ S, K, r, q, sigma, t) - 
       fBS.call.price(L ^ 2/ S, L, r, q, sigma, t) -
       (L - K) * exp(-r * t) * pnorm(fd2(L, S, r, q, sigma, t))) * (K > L) +
    (fBS.put.price(S, min(L, K), r, q, sigma, t) -
       (min(L, K) - K) * exp(-r * t) * 
       pnorm(-fd2(S, min(L, K), r, q, sigma, t)))
  return(price)
}

# Price of digital call option
fBS.digital.call.price <- function(S, K, r, q, sigma, t) {
  price <- exp(-r * t) * pnorm(fd1(S, K, r, q, sigma, t) - sigma * sqrt(t))
  return(price)
}

#-------------------------------------------------------------------------------

# 4.2 - Price calculation of each option (and stock per se)
# long stock: S
# D&I barrier:
DI.put <- fBS.DI.put.price(S, l1*S, l2*S, r, q, sigma, t)
# short call at FV:
call <- fBS.call.price(S, g1*S, r, q, sigma, t)
# 1st digital call:
digital.one <- fBS.digital.call.price(S, g2*S, r, q, sigma, t)
# 2nd digital call:
digital.two <- fBS.digital.call.price(S, g3*S, r, q, sigma, t)

# Check the prices of the segments of the portfolio
cat("Replicating portfolio contents & prices:\n")
cat("Long stock:\t\t\t", S, "\n")
cat("Long down-and-in Eur put:\t", DI.put, "\n")
cat("Short Eur call:\t\t\t", -call, "\n")
cat("Long first digital call:\t", digital.one, "(h*S times)\n")
cat("Long second digital call:\t", digital.two, "(h*S times)\n")

#-------------------------------------------------------------------------------

# 4.3 - Exploration of step sizes

# Each step size is h
# If we take total product price = S (no transaction cost), then
# h = (call - DI.put) / (digital.one + digital.two) / S
# Alternatively: total product price = 0.98 * S (2% price as transaction cost)
h = (call - DI.put - 0.02 * S) / (digital.one + digital.two) / S
cat("Step size(h):", h, "\n")

# check of equality
(digital.one * h * S)
total.price = S + DI.put - call + (digital.one + digital.two) * h * S
cat("Total price =", total.price, "S =", S, "\n")
cat("Total price / S =", total.price / S, "\n")

#===============================================================================

# 5 - Hedging formulas
# delta is change of option price against change of underlying price
# In this section, t = tenor, tau = current time = 0

# 5.1 - Delta of European Call/Put at tau = 0
# Beware of the Callput variable when using the functions 
fBS.callput.delta <- function(CallPut, S, K, r, q, sigma, t) {
  d1 <- fd1(S, K, r, q, sigma, t)
  if (CallPut == 'Call') {
    delta <- exp(-q * t) * pnorm(d1)
  }
  else {
    delta <- -exp(-q * t) * pnorm(-d1)
  }
  return(delta)
}

# 5.2 - Delta of Digital Call
fBS.digital.call.delta <- function(S, K, e, q, sigma, t) {
  const <- exp(-r * t) / (sigma * S * t)
  delta <- const * dnorm(fd2(S, K, r, q, sigma, t))
  return(delta)
}

# 5.3 - Delta of DI European put, by approximation approach
fBS.DI.put.delta.approx <- function(S, K, H, r, q, sigma, t) {
  h <- 0.000001
  upper <- fBS.DI.put.price(S + h / 2, K, H, r, q, sigma, t)
  lower <- fBS.DI.put.price(S - h / 2, K, H, r, q, sigma, t)
  delta.approx <- (upper - lower) / h
}

# 5.4 - Delta of DI European put, by formula
fBS.DI.put.delta <- function(S, K, L, r, q, sigma, t) {
  v <- (r - q - 0.5 * sigma ^ 2)
  const1 <- (L / S) ^ (2 * v / sigma^2) 
  const1.diff <- L ^ (2 * v / sigma^2) * 
    (-2 * v / sigma^2) * S ^ (-2 * v / sigma^2 -1)
  #C(L^2/S,K)
  call1 <- fBS.call.price(L^2/S, K, r, q, sigma, t)
  delta1 <- (L^2 / S^2) * fBS.callput.delta('Call', L^2/S, K, r, q, sigma, t) 
  #C(L^2/S,L)
  call2 <- fBS.call.price(L^2/S, L, r, q, sigma, t)
  delta2 <- (L^2 / S^2) * fBS.callput.delta('Call', L^2/S, L, r, q, sigma, t) 
  #(L-K)e^(-rt)N(d2(L,S))
  const2 <- (L - K) * exp(-r * t) * pnorm(fd2(L, S, r, q, sigma, t))
  const2.diff <- (L - K) * exp(-r * t) / (S * sigma * sqrt(t)) * 
    dnorm(fd2(L, S, r, q, sigma, t))
  
  deltas.diff <- -delta1 + delta2 + const2.diff
  part1.diff <- const1 * deltas.diff
  
  deltas.nodiff <- call1 - call2 - const2
  part2.diff <- const1.diff * deltas.nodiff
  
  part1 <- part1.diff + part2.diff
  
  part2 <- fBS.callput.delta('Put', S, L, r, q, sigma, t) + 
    (L - K) * exp(-r * t) * dnorm(-1 * fd2(S, L, r, q, sigma, t)) / 
    (S * sigma * sqrt(t))
  
  return(part1 + part2)
}

# 5.5 - Calculation of component delta
# Delta of long 1 stock is 1, trivially
delta.DI.put <- fBS.DI.put.delta(S, l2*S, l1*S, r, q, sigma, t)
delta.DI.put.approx <- fBS.DI.put.delta.approx(S, l2*S, l1*S, r, q, sigma, t)
delta.call <- -fBS.callput.delta('Call', S, g1*S, r, q, sigma, t)
delta.digital.one <- fBS.digital.call.delta(S, g2*S, r, q, sigma, t)
delta.digital.two <- fBS.digital.call.delta(S, g3*S, r, q, sigma, t)

cat("Delta values of each component:\n")
cat("Long Stock:\t1\n")
cat("Long DI European Put (Calculated):\t", delta.DI.put, "\n")
cat("(Or alternatively:)\n")
cat("Long DI European Put (approximated):\t", delta.DI.put.approx, "\n")
cat("Short European call:\t\t", delta.call, "\n")
cat(h*S, "x Long First Digital Call(s):\t", delta.digital.one, "\n")
cat(h*S, "x Long Second Digital Call(s):\t", delta.digital.two, "\n")
cat("Total delta (stock position required):\t",
    sum(1, delta.DI.put, delta.call, 
        h * S * delta.digital.one, h * S * delta.digital.two))

#===============================================================================

# 6 - Graph plotting
# 6.1 - S&P 500 market trend plot & return Q-Q plot

# Last 10 year S&P500 bar chart
png(file = "../graphs/S&P500_Trend_Last_10_Years.png", 
    width = 800, height = 500)
barChart(SP500.raw.full, theme = "white.mono", bar.type = "hlc")
dev.off()

# Last 3 years S&P500 daily return
png(file = "../graphs/S&P500_Return_QQPlot_Last_10_Years.png", 
    width = 800, height = 500)
qqnorm(SP500.DayRet)
dev.off()
rm(SP500.DayRet)

#-------------------------------------------------------------------------------

# 6.2 - Plot expected payoff graph
# 6.2.1 - Preparation

minprice <- 0.5 * S
maxprice <- 1.5 * S
prices <- seq(minprice, maxprice, 1)
n <- length(prices)

# Payoff setting
payoff.stock <- vector(mode = "numeric", n)
payoff.DI.put.notrigger <- vector(mode = "numeric", n)
payoff.DI.put.trigger <- vector(mode = "numeric", n)
payoff.call <- vector(mode = "numeric", n)
payoff.first.digital.call <- vector(mode = "numeric", n)
payoff.second.digital.call <- vector(mode = "numeric", n)

for (i in 1:n) {
  payoff.stock[i] = prices[i]
  # DI Put: Different payoffs when triggered or not
  payoff.DI.put.notrigger[i] = 0
  payoff.DI.put.trigger[i] = max(l2 * S - prices[i], 0)
  payoff.call[i] = - max(prices[i] - g1*S, 0) #short call
  # h * S number of digital calls
  payoff.first.digital.call[i] = h * S * if(prices[i] > g2*S) 1 else 0
  payoff.second.digital.call[i] = h * S * if(prices[i] > g3*S) 1 else 0
}

# Profit setting
profit.stock <- payoff.stock - as.vector(S)
profit.DI.put.notrigger <- payoff.DI.put.notrigger - as.vector(DI.put)
profit.DI.put.trigger <- payoff.DI.put.trigger - as.vector(DI.put)
profit.call <- payoff.call + as.vector(call)
profit.first.digital.call <- payoff.first.digital.call - 
  h * S * as.vector(digital.one)
profit.second.digital.call <- payoff.second.digital.call - 
  h * S * as.vector(digital.two)

#-------------------------------------------------------------------------------

# 6.2.2 - Profit graph (split up) when the barrier is not triggered
profit.notrigger.overall <- rowSums(cbind(profit.stock,
                                          profit.DI.put.notrigger,
                                          profit.call,
                                          profit.first.digital.call,
                                          profit.second.digital.call,
                                          as.vector(-0.02 * S)))
# Transaction cost is removed

# Generate a dataframe for all vectors
# in order to plot the strategy payoffs using ggplot
results.notrigger <- data.frame(cbind(profit.stock,
                                      profit.DI.put.notrigger,
                                      profit.call,
                                      profit.first.digital.call,
                                      profit.second.digital.call,
                                      profit.notrigger.overall))

# Cut off the part lower than barrier
# This causes some warnings, but no impact on plotting
results.notrigger[1:(floor(l1*S - minprice)), ] <- NA

# please add points to S, l1*S, l2*S, etc
ggplot(results.notrigger / S, aes(x = prices / S)) + 
  geom_line(linetype = "dashed", aes(y = profit.stock, color = "Stock")) + 
  geom_line(linetype = "dashed", 
            aes(y = profit.DI.put.notrigger, color = "DI European Put")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.call, color = "European Call")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.first.digital.call, color = "First Digital")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.second.digital.call, color = "Second Digital")) +
  geom_line(aes(y = profit.notrigger.overall, color="Total Profit")) +
  scale_colour_manual("", 
                      breaks = c("Stock", "DI European Put", "European Call", 
                                 "First Digital", "Second Digital", 
                                 "Total Profit"),
                      values = c("darkred", "darkorange", "violet",  
                                 "darkgreen", "darkblue", "black")) + 
  xlab("Underlying Price") +
  ylab("Profit") +
  ggtitle("Product Profit Percentage (R) at Maturity (Not Triggered)") + 
  xlim(0.5, 1.5) + ylim(-0.5, 0.5)

ggsave("../graphs/ProfitPlotNotTriggered.png", width = 9.8, height = 8)

#-------------------------------------------------------------------------------

# 6.2.3 - Profit graph (split up) when the barrier is triggered

profit.trigger.overall <- rowSums(cbind(profit.stock,
                                        profit.DI.put.trigger,
                                        profit.call,
                                        profit.first.digital.call,
                                        profit.second.digital.call,
                                        as.vector(-0.02 * S)))

# Generate a dataframe for all vectors
# in order to plot the strategy payoffs using ggplot
results.trigger <- data.frame(cbind(profit.stock,
                                    profit.DI.put.trigger,
                                    profit.call,
                                    profit.first.digital.call,
                                    profit.second.digital.call,
                                    profit.trigger.overall))

ggplot(results.trigger / S, aes(x = prices / S)) + 
  geom_line(linetype = "dashed", aes(y = profit.stock, color = "Stock")) + 
  geom_line(linetype = "dashed", 
            aes(y = profit.DI.put.trigger, color = "DI European Put")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.call, color = "European Call")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.first.digital.call, color = "First Digital")) +
  geom_line(linetype = "dashed", 
            aes(y = profit.second.digital.call, color = "Second Digital")) +
  geom_line(aes(y = profit.trigger.overall, color="Total Profit")) +
  scale_colour_manual("", 
                      breaks = c("Stock", "DI European Put", "European Call", 
                                 "First Digital", "Second Digital", 
                                 "Total Profit"),
                      values = c("darkred", "darkorange", "violet",  
                                 "darkgreen", "darkblue", "black")) + 
  xlab("Underlying Price") +
  ylab("Profit") +
  ggtitle("Product Profit Percentage (R) at Maturity (Not Triggered)") + 
  xlim(0.5, 1.5) + ylim(-0.5, 0.5)

ggsave("../graphs/ProfitPlotTriggered.png", width = 9.8, height = 8)

#-------------------------------------------------------------------------------

# 6.2.4 - Combined total profit when triggered/not triggered
# Complete 6.2.2 & 6.2.3 first

combined <- data.frame(cbind(profit.trigger.overall, profit.notrigger.overall))
combined[1:(floor(l1*S - minprice)), 2] <- NA

ggplot(combined / S, aes(x = prices / S)) + 
  geom_line(linetype = "dashed", 
            aes(y = profit.notrigger.overall, color="Before Triggering")) + 
  geom_line(aes(y = profit.trigger.overall, color="After Triggering")) + 
  scale_colour_manual("", 
                      breaks = c("Before Triggering", "After Triggering"),
                      values = c("darkred", "black")) + 
  xlab("Underlying Price") +
  ylab("Profit") +
  ggtitle("Product Profit Percentage (R) at Maturity, before & after triggering"
  ) + 
  annotate(geom = "label", x = 1, y = -0.25, size = 3, 
           label = "When the safety level is triggered") + 
  annotate(geom = 'label', x = 1.2, y = 0, size = 3,
           label = "Ladder Step 1, 2, 3") +
  annotate(geom = "point", x = c(l2, g1, g2, g3), 
           y = c(-628/S, 376/S, 580/S, 785/S), 
           size = 7, shape= 21,
           fill="transparent") +
  annotate(geom = "segment", linetype = "dashed", 
           x = l2, xend = l2 + 0.00000001, 
           y = -0.5, yend = 0.5,
           colour = "blue") +
  xlim(0.5, 1.5) + ylim(-0.5, 0.5)

ggsave("../graphs/ProfitPlotCombined.png", width = 9.8, height = 8)

#-------------------------------------------------------------------------------

# 6.3 - Plot delta graph
# 6.3.1 - Preparation

minprice <- 0.5 * S
maxprice <- 1.5 * S
prices <- seq(minprice, maxprice, 1)
n <- length(prices)

# delta setting

graph.delta.DI.put <- vector(mode = "numeric", n)
graph.delta.call <- vector(mode = "numeric", n)
graph.delta.digital.one <- vector(mode = "numeric", n)
graph.delta.digital.two <- vector(mode = "numeric", n)
graph.delta.overall <- vector(mode = "numeric", n)

for (i in 1:n) {
  graph.delta.DI.put[i] = fBS.DI.put.delta(prices[i], l2*S, l1*S, r, q, sigma, t)
  graph.delta.call[i] = -1 * fBS.callput.delta('Call', prices[i], g1*S, r, q, sigma, t)
  graph.delta.digital.one[i] = fBS.digital.call.delta(prices[i], g2*S, r, q, sigma, t)
  graph.delta.digital.two[i] = fBS.digital.call.delta(prices[i], g3*S, r, q, sigma, t)
  graph.delta.overall[i] = graph.delta.DI.put[i] + graph.delta.call[i] + 
                          graph.delta.digital.one[i] + graph.delta.digital.two[i]
}
#-------------------------------------------------------------------------------

# 6.3.2 - Delta graph

results.delta <- data.frame(cbind(graph.delta.DI.put, 
                                  graph.delta.call, 
                                  graph.delta.digital.one, 
                                  graph.delta.digital.two,
                                  graph.delta.overall))

# please add points to S, l1*S, l2*S, etc
ggplot(results.delta, aes(x = prices)) + 
  geom_line(linetype = "dashed", 
            aes(y = graph.delta.DI.put, color = "DI Put delta")) + 
  geom_line(linetype = "dashed", 
            aes(y = graph.delta.call, color = "European Call delta")) +
  geom_line(linetype = "dashed", 
            aes(y = graph.delta.digital.one, color = "First Digital delta")) +
  geom_line(linetype = "dashed", 
            aes(y = graph.delta.digital.two, color = "Second Digital delta")) +
  geom_line(linetype = "solid",
            aes(y = graph.delta.overall, color="Total Delta")) +
  scale_colour_manual(breaks = c("DI Put delta", "European Call delta", 
                                 "First Digital delta", "Second Digital delta", 
                                 "Total Delta"),
                      values = c("violet", "darkorange","darkgreen", "darkblue", 
                                 "black")) + 
  xlab("Underlying Price") +
  ylab("Delta") +
  ggtitle("Delta at Maturity") 

#ggsave("../graphs/ProfitPlotNotTriggered.png", width = 9.8, height = 8)
#-------------------------------------------------------------------------------


# cleaning up
rm(prices)
rm(payoff.stock, payoff.DI.put.notrigger, payoff.DI.put.trigger, payoff.call, 
   payoff.first.digital.call, payoff.second.digital.call)
rm(profit.stock, profit.DI.put.notrigger, profit.DI.put.trigger, profit.call, 
   profit.first.digital.call, profit.second.digital.call,
   profit.notrigger.overall, profit.trigger.overall)

