#FINA 4354 Project
rm(list = ls())
options(scipen = 999) #<-prevent using scientific notation

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
rm(list.of.library, i) #Free up memory

#-------------------------------------------------------------------------------

# 2 - Data processing
# 2.1 - S&P500 data downloading
#S&P500 Index:
SP500.raw <- na.locf(getSymbols("^GSPC",
                                from = Sys.Date() - years(3),
                                auto.assign = FALSE))
#S&P500 Total Return Index:
SP500TR.raw <- na.locf(getSymbols("^SP500TR", 
                                  from = Sys.Date() - years(3),
                                  auto.assign = FALSE))

#S&P500 ETF:
SPY <- na.locf(getSymbols("SPY", 
                          from = Sys.Date() - years(3), 
                          auto.assign = FALSE))

#-------------------------------------------------------------------------------

# 2.2 - risk-free rate downloading
# We prepare the 1m, 3m, 6m, 1y version of risk-free rate
# If we want to change the tenor, we can change t correspondingly
#DGS1MO <- na.locf(getSymbols("DGS1MO", src = "FRED", auto.assign = FALSE))
#DGS3MO <- na.locf(getSymbols("DGS3MO", src = "FRED", auto.assign = FALSE))
DGS6MO <- na.locf(getSymbols("DGS6MO", src = "FRED", auto.assign = FALSE))
#DGS1YR <- na.locf(getSymbols("DGS1", src = "FRED", auto.assign = FALSE))

#-------------------------------------------------------------------------------
# 2.3 - Storing data to local repository
# change the xts into dataframe

userpath <- getwd()
write.csv(data.frame(row.names = index(SP500.raw), coredata(SP500.raw)),
          file = file.path(userpath, 'SP500.raw.csv'))
write.csv(data.frame(row.names = index(SP500TR.raw), coredata(SP500TR.raw)),
          file = file.path(userpath, 'SP500TR.raw.csv'))
write.csv(data.frame(row.names = index(SPY), coredata(SPY)),
          file = file.path(userpath, 'SPY.csv'))
write.csv(data.frame(row.names = index(DGS6MO), coredata(DGS6MO)),
          file = file.path(userpath, 'DGS6MO.csv'))

# 2.4 - Loading data from local repository

list.of.rawdata = c('SP500.raw', 'SP500TR.raw', 'SPY', 'DGS6MO')
for (i in list.of.rawdata) {
  userpath <- getwd()
  filename <- paste(i, sep = "", '.csv')
  i <- read.csv(file = file.path(userpath, filename), row.names = 1)
}

rm(filename, i, list.of.rawdata, userpath)

#-------------------------------------------------------------------------------

# 3 - Parameter setting
# 3.1 Find the dividend yield with S&P 500
#Get Dividend Yield approximation:
SP500.DayRet <- dailyReturn(SP500.raw$GSPC.Adjusted, 
                            subset = NULL,
                            type = 'log')
SP500TR.DayRet <- dailyReturn(SP500TR.raw$SP500TR.Adjusted,
                              subset = NULL, 
                              type = 'log')
#Get daily return of 2020:
dividend.yield <- sum((SP500TR.DayRet['2020'] - SP500.DayRet['2020']) *
                        SP500.raw['2020',"GSPC.Adjusted"])/
  SP500.raw['2020-12-31',"GSPC.Adjusted"]
q <- as.numeric(coredata(dividend.yield[1]))

rm(SP500.DayRet, SP500TR.DayRet)

#-------------------------------------------------------------------------------

# 3.2 - Find other parameters for the model
n <- nrow(DGS6MO)
#the last day's risk-free rate: note that the rate is in %
r <- as.numeric(coredata(DGS6MO$DGS6MO[n])) / 100

n <- nrow(SP500.raw)
#the last day's adjusted index:
S <- as.numeric(coredata(SP500.raw$GSPC.Adjusted[n]))
sigma <- as.numeric(sd(dailyReturn(SP500.raw$GSPC.Adjusted)) * sqrt(252))
t <- 0.5

rm(n)  #remove unused variables

#-------------------------------------------------------------------------------

# 3.3 - Find strike prices & step spans

miu <- as.numeric(mean(dailyReturn(SP500.raw$GSPC.Adjusted)))
total.miu <- miu * 126  # half year
cat(total.miu)
#The total miu in the 6mo period is approx. 8%
#So, S&P500 investors generally expect a return of 8%

# lower bound: L = l*S
l <- 0.7
# period of floating loss: l*S ~ g1*S
g1 <- 1 + total.miu * 0.5       # g1*FV ~ g2*FV is the 1st step
g2 <- 1 + total.miu             # g2*FV ~ g3*FV is the 2nd step
g3 <- 1 + total.miu * 1.5       # > g3*FV is the 3rd step (ceiling)

#-------------------------------------------------------------------------------

# 4 - Financial models
# 4.1 - Pricing Functions of options used
#S = Spot Price, K = Strike Price, L = Barrier Price
#r = Expected Return, q = Dividend Yield
#sigma = volatility, t = time to maturity

#Value of d1
fd1 <- function(S, K, r, q, sigma, t) {
  d1 <- (log(S / K) + (r - q + 0.5 * sigma ^ 2) * t) / (sigma * sqrt(t))
  return(d1) #d2 <- d1 - sigma*sqrt(t)
}

#Price of European call
fBS.call.price <- function(S, K, r, q, sigma, t) { 
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- S * exp(-q * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  return(price)
}

#Price of European put
fBS.put.price <- function(S, K, r, q, sigma, t) {
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- K * exp(-r * t) * pnorm(-d2) - S * exp(-q * t) * pnorm(-d1)
  return(price)
}

#Price of down-and-in barrier option
fBS.DI.barrier.price <- function(S, L, r, q, sigma, t){
  const = (L / S) ^ (2 * (r-q) / (sigma ^ 2) - 1)
  # here r is replaced by r-q
  short.forward = exp(-r) * L - S
  # the discount process does not involve q
  price = short.forward + fBS.call.price(S, L, r, q, sigma, t) - 
    const * fBS.call.price(L ^ 2 / S, L, r, q, sigma, t)
  return(price)
}

#Price of digital call option
fBS.digital.call.price <- function(S, K, r, q, sigma, t) {
  price <- exp(-r * t) * pnorm(fd1(S, K, r, q, sigma, t) - sigma * sqrt(t))
  return(price)
}

#-------------------------------------------------------------------------------

# 4.2 - Price calculation of each option (and stock per se)
# long stock: S
# D&I barrier:
DI.barrier <- fBS.DI.barrier.price(S, l*S, r, q, sigma, t)
# short call at FV:
call <- fBS.call.price(S, g1*S, r, q, sigma, t)
# 1st digital call:
digital.one <- fBS.digital.call.price(S, g2*S, r, q, sigma, t)
# 2nd digital call:
digital.two <- fBS.digital.call.price(S, g3*S, r, q, sigma, t)

# Check the prices of the segments of the portfolio
cat(S, DI.barrier, call, digital.one, digital.two, '\n')

#-------------------------------------------------------------------------------

# 4.3 - Exploration of step sizes

# We set that:
# h1 = 0, since it is connected with the slope
# h2 <- 0.5 * h3
# h3 is the unknown to be solved

#total stock price = S + DI.barrier - call + (h2-h1) * S * digital.one + 
#(h3-h2) * S * digital.two
#If we take total product price = S, then
#h3 = (call - DI.barrier)/(0.5*dig.one + 0.5*dig.two)/S

h3 = (call - DI.barrier)/(0.5*digital.one + 0.5*digital.two)/S
h2 = 0.5*h3

cat("h2 =", h2, "\nh3 =", h3, "\n")

#check of equality
total.price = S + DI.barrier - call + h2*S*digital.one + 
  (h3-h2)*S*digital.two
cat("total price =", total.price, "S =", S, "\n")

#-------------------------------------------------------------------------------

# 5 - Graph plotting
# 5.1 - Plot expected payoff graph

minprice <- 0
maxprice <- 2.0 * S 
prices <- seq(minprice, maxprice, 1)
n <- length(prices)

payoff.stock <- vector(mode = "numeric", n)
payoff.DI.barrier <- vector(mode = "numeric", n)
payoff.call <- vector(mode = "numeric", n)
# the two digital calls are in total amount
payoff.first.digital.call <- vector(mode = "numeric", n)
payoff.second.digital.call <- vector(mode = "numeric", n)
rm(n, maxprice, minprice)

#Note: it is unable to plot barrier options on a graph at t = T
#because it is path dependent
#For illustration purpose, we use a (long) European put option instead

for (i in 1:length(prices)) {
  payoff.stock[i] = prices[i]
  payoff.DI.barrier[i] = max(l*S - prices[i], 0)
  payoff.call[i] = - max(prices[i] - g1*S, 0) #short call
  payoff.first.digital.call[i] = h2 * S * if(prices[i] > g2*S) 1 else 0
  payoff.second.digital.call[i] = (h3 - h2) * S * if(prices[i] > g3*S) 1 else 0
}

#Payoff graph
payoff.overall <- rowSums(cbind(payoff.stock,
                                payoff.DI.barrier,
                                payoff.call,
                                payoff.first.digital.call,
                                payoff.second.digital.call))

ggplot(results, aes(x=prices)) + 
  geom_line(linetype = "dashed", aes(y = payoff.stock, color = "Stock Price")) +
  geom_line(aes(y = payoff.overall, color="Payoff")) +
  scale_colour_manual("", 
                      breaks = c("Stock Price", "Payoff"), 
                      values = c("darkblue", "black")) + 
  xlab("Undelying Price") +
  ylab("Payoff") +
  ggtitle("Product Payoff at t = T")

#Profit graph split up
profit.stock <- payoff.stock - as.vector(S)
profit.DI.barrier <- payoff.DI.barrier - as.vector(DI.barrier)
profit.call <- payoff.call - as.vector(call)
profit.first.digital.call <- payoff.first.digital.call - as.vector(digital.one)
profit.second.digital.call <- payoff.second.digital.call - as.vector(digital.two)

profit.overall <- rowSums(cbind(profit.stock,
                               profit.DI.barrier,
                               profit.call,
                               profit.first.digital.call,
                               profit.second.digital.call))

# Generate a data_frame all vectors in order to plot the strategy payoffs using ggplot
results <- data.frame(cbind(profit.stock,
                            profit.DI.barrier,
                            profit.call,
                            profit.first.digital.call,
                            profit.second.digital.call))

ggplot(results, aes(x=prices)) + 
  geom_line(linetype = "dashed", aes(y = profit.stock, color = "Stock")) + 
  geom_line(linetype = "dashed", aes(y = profit.DI.barrier, color = "DIBarrier")) +
  geom_line(linetype = "dashed", aes(y = profit.call, color = "EuropeanCall")) +
  geom_line(linetype = "dashed", aes(y = profit.first.digital.call, color = "FirstDigital")) +
  geom_line(linetype = "dashed", aes(y = profit.second.digital.call, color = "SecondDigital")) +
  geom_line(aes(y = profit.overall, color="Profit")) +
  scale_colour_manual("", 
                      breaks = c("Stock", "DIBarrier", "EuropeanCall", "FirstDigital",
                                 "SecondDigital", "Profit"),
                      values = c("darkred", "darkorange", "violet",  
                                 "darkgreen", "darkblue", "black")) + 
  xlab("Undelying Price") +
  ylab("Profit") +
  ggtitle("Product Profit") 

#cleaning up
rm(payoff.stock, payoff.DI.barrier, payoff.call,
   payoff.first.digital.call, payoff.second.digital.call, payoff.overall)
rm(profit.stock, profit.DI.barrier, profit.call, 
   profit.first.digital.call, profit.second.digital.call, profit.overall)
