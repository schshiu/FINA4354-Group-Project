
#FINA 4354 Project
rm(list = ls())
options(scipen = 999) #<-prevent using scientific notation

#-------------------------------------------------------------------------------
# 1 - Preparation steps
# Check if client's computer has the library downloaded,
# then load the required library
list.of.library <- c('xts', 'quantmod', 'ggplot2')
# Note: 'lubridate' can be added if date calculation is needed
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
# 2.1 - Find the dividend yield with S&P 500
#Raw Data
SP500.raw <- na.locf(getSymbols("^GSPC",
                                from = "2018-01-01", 
                                auto.assign = FALSE)) #S&P500 Index
SP500TR.raw <- na.locf(getSymbols("^SP500TR", 
                                  from = "2018-01-01", 
                                  auto.assign = FALSE)) #S&P500 Total Return Index
SPY <- na.locf(getSymbols("SPY", 
                          from = "2018-01-01", 
                          auto.assign = FALSE))

#Get daily return of 2020
SP500.DayRet <- dailyReturn(SP500.raw$GSPC.Adjusted, 
                            subset = NULL,
                            type = 'log')
SP500TR.DayRet <- dailyReturn(SP500TR.raw$SP500TR.Adjusted,
                              subset = NULL, 
                              type = 'log')

#-------------------------------------------------------------------------------
# 2.2 - find risk-free rate
# We prepare the 1m, 3m, 6m, 1y version of risk-free rate
# If we want to change the tenor, we can change t correspondingly
#DGS1MO <- na.locf(getSymbols("DGS1MO", src = "FRED", auto.assign = FALSE))
#DGS3MO <- na.locf(getSymbols("DGS3MO", src = "FRED", auto.assign = FALSE))
DGS6MO <- na.locf(getSymbols("DGS6MO", src = "FRED", auto.assign = FALSE))
#DGS1YR <- na.locf(getSymbols("DGS1", src = "FRED", auto.assign = FALSE))

#-------------------------------------------------------------------------------
# 2.3 - Data storage and loading
# Misc: Storing data to local repository
# change the xts into dataframe

list.of.rawdata <- c('SP500.raw', 'SPY', 'DGS6MO')

write.csv(data.frame(row.names = index(SP500.raw), coredata(SP500.raw)),
          file = file.path(getwd(), 'SP500.raw.csv'))
write.csv(data.frame(row.names = index(SPY), coredata(SPY)),
          file = file.path(getwd(), 'SPY.csv'))
write.csv(data.frame(row.names = index(SP500TR.DayRet), coredata(SP500TR.DayRet)),
          file = file.path(getwd(), 'SP500TR.DayRet.csv'))
write.csv(data.frame(row.names = index(DGS6MO), coredata(DGS6MO)),
          file = file.path(getwd(), 'DGS6MO.csv'))

# Misc: Loading data from local repository
for (i in list.of.rawdata) {
  userpath <- getwd()
  filename <- paste(i, sep = "", '.csv')
  i <- read.csv(file = file.path(userpath, filename), row.names = 1)
}

rm(filename, i, list.of.rawdata)
#-------------------------------------------------------------------------------

# 3 - Financial Model
# 3.1 Get parameters

#Get Dividend Yield approximation
dividend.yield <- sum((SP500TR.DayRet['2020'] - SP500.DayRet['2020']) *
                        SP500.raw['2020',"GSPC.Adjusted"])/
                  SP500.raw['2020-12-31',"GSPC.Adjusted"]
q <- as.numeric(coredata(dividend.yield[1]))

#normal parameters
n <- nrow(DGS6MO)
r <- as.numeric(coredata(DGS6MO$DGS6MO[n])) / 100
#the last day's risk-free rate: note that the rate is in %
n <- nrow(SP500.raw)
# we use the SP500 index itself as underlying, not SPY
# SPY can be used as a tool to hedge
S <- as.numeric(coredata(SP500.raw$GSPC.Adjusted[n]))#the last day's adjusted index
miu <- as.numeric(mean(dailyReturn(SP500.raw$GSPC.Adjusted)))
#miu is for estimation purpose, not for simulation
sigma <- as.numeric(sd(dailyReturn(SP500.raw$GSPC.Adjusted)) * sqrt(252))
t <- 0.5

# Other parameters to determine
FV <- S         # the "face value" FV sets a standard to other parameters
# it is set at S (initial price), so the call option at F is at the money

#miu = 0.00062896
total.miu <- miu * 126
cat(total.miu)
#The total miu in the 6mo period is approx. 8%
#So, S&P500 investors generally expect a return of 8%
#We set g1 = 1.04 (half the expected return), g2 = 1.08 (expected return)

l <- 0.7        # lower bound: L = l*FV
g1 <- 1.04       # FV ~ g1*FV is the 1st step
g2 <- 1.08       # g1*FV ~ g2*FV is the 2nd step
# > g2*FV is the 3rd step

# step sizes h1, h2 and h3:
# key factors to be determined. To be introduced later

rm(n)  #remove unused variables

#-------------------------------------------------------------------------------
# 3.2 Option Pricing Functions
#S = Spot Price, K = Strike Price, L = Barrier Price
#r = Expected Return, q = Dividend Yield
#sigma = volatility, t = time to maturity

fd1 <- function(S, K, r, q, sigma, t) {
  d1 <- (log(S / K) + (r - q + 0.5 * sigma ^ 2) * t) / (sigma * sqrt(t))
  return(d1) #d2 <- d1 - sigma*sqrt(t)
}

fBS.call.price <- function(S, K, r, q, sigma, t) { 
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- S * exp(-q * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  return(price)
}

fBS.put.price <- function(S, K, r, q, sigma, t) {
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- K * exp(-r * t) * pnorm(-d2) - S * exp(-q * t) * pnorm(-d1)
  return(price)
}

fBS.DI.barrier.price <- function(S, L, r, q, sigma, t){
  const = (L / S) ^ (2 * (r-q) / (sigma ^ 2) - 1)
  # here r is replaced by r-q
  short.forward = exp(-r) * L - S
  # the discount process does not involve q
  price = short.forward + fBS.call.price(S, L, r, q, sigma, t) - 
    const * fBS.call.price(L ^ 2 / S, L, r, q, sigma, t)
  return(price)
}

fBS.digital.call.price <- function(S, K, r, q, sigma, t) {
  price <- exp(-r * t) * pnorm(fd1(S, K, r, q, sigma, t) - sigma * sqrt(t))
  return(price)
}

#-------------------------------------------------------------------------------
# 3.4 - Price calculation of each option (and stock per se)
# long stock: S
DI.barrier <- fBS.DI.barrier.price(S, l*FV, r, q, sigma, t) # D&I barrier
call <- fBS.call.price(S, FV, r, q, sigma, t)               # short call at FV
digital.one <- fBS.digital.call.price(S, FV, r, q, sigma, t)
# 1st digital call
digital.two <- fBS.digital.call.price(S, g1*FV, r, q, sigma, t)
# 2nd digital call
digital.three <- fBS.digital.call.price(S, g2*FV, r, q, sigma, t)
# 3rd digital call
cat(S, DI.barrier, digital.one, digital.two, digital.three, '\n')

#-------------------------------------------------------------------------------
# 3.5 - Exploration of step size hi (i = 1, 2, 3)

# We set that h1, h2 are fractions of h3
# so there is only 1 unknown in 1 equation, and h3 is the target to be found
# assume h1 = k1*h3, h2 = k2*h3

#total stock price = S + DI.barrier - call + h1*digital.one + (h2-h1)*digital.two + 
# (h3-h2)*digital.three
#= S + DI.barrier - call + (k1*dig.one + (k2-k1)*dig.two + (1-k2)*dig.three)*h3
#we take total stock price = FV = S
#h3 = (call - DI.barrier)/(k1*dig.one + (k2-k1)*dig.two + (1-k2)*dig.three)

#value take for k1, k2
k1 <- 0.3
k2 <- 0.6

h3 = (call - DI.barrier)/(k1*digital.one + (k2-k1)*digital.two + 
                            (1-k2)*digital.three)
# the 3rd step's size: (h3 - h2)*FV
h1 <- k1 * h3  # the 2nd step's size: (h2 - h1)*FV
h2 <- k2 * h3  # the 1st step's size: h1*FV

cat("h1 =", h1, "\nh2 =", h2, "\nh3 =", h3, "\n")

#check of equality
total.price = S + DI.barrier - call + h1*digital.one + (h2-h1)*digital.two + 
  (h3-h2)*digital.three
cat("total price =", total.price, "S = ", S, "\n")


#-------------------------------------------------------------------------------
# Plot expected payoff graph
#Note that barrier option cannot be expressed in t = T plot,
#we applied a normal put option to replace the down-and-in option
minprice <- 0.1 * S 
maxprice <- 1.9 * S 
prices <- seq(minprice, maxprice, 1)
n <- length(prices)
BarrierOptionPayoff <- vector(mode = "numeric", n)
EuropeanShortCallPayoff <- vector(mode = "numeric", n)
FirstDigitalCallPayoff <- vector(mode = "numeric", n)
SecondDigitalCallPayoff <- vector(mode = "numeric", n)
ThirdDigitalCallPayoff <- vector(mode = "numeric", n)
rm(n)

# Payoff Function
#Graph_BarrierPayoff <- prices - as.vector(k_2) - as.vector(BarrierOptionPrice)
#for (i in 1:length(prices)) {
#  if (prices[i] <= k_1) {
#    Graph_BarrierPayoff[i] <- k_1 - k_2 - as.vector(BarrierOptionPrice)
#  }
#}
EuropeanLongCallPayoff <- prices - as.vector(k_2)
EuropeanShortCallPayoff <- as.vector(k_3) - prices 
EuropeanLongPutPayoff <- as.vector(k_1) - prices
EuropeanShortPutPayoff <- prices - as.vector(k_2)
for (i in 1:length(prices)) {
  if (prices[i] <= k_3) {
    FirstDigitalCallPayoff[i] = 0
    SecondDigitalCallPayoff[i] = 0
    ThirdDigitalCallPayoff[i] = 0
  }
  else if (prices[i] <= k_4) {
    FirstDigitalCallPayoff[i] = k_3 - S
    SecondDigitalCallPayoff[i] = 0
    ThirdDigitalCallPayoff[i] = 0
  }
  else if (prices[i] <= k_5) {
    FirstDigitalCallPayoff[i] = k_3 - S
    SecondDigitalCallPayoff[i] = k_4 - S
    ThirdDigitalCallPayoff[i] = 0
  }
  else {
    FirstDigitalCallPayoff[i] = k_3 - S
    SecondDigitalCallPayoff[i] = k_4 - S
    ThirdDigitalCallPayoff[i] = k_5 - S
  }
}

Graph_LongCallPayoff <- pmax(0, EuropeanLongCallPayoff) - as.vector(EuropeanLongCallPrice)
Graph_ShortCallPayoff <- pmin(0, EuropeanShortCallPayoff) - as.vector(EuropeanShortCallPrice)
Graph_LongPutPayoff <- pmax(0, EuropeanLongPutPayoff) - as.vector(EuropeanLongPutPrice)
Graph_ShortPutPayoff <- pmin(0, EuropeanShortPutPayoff) - as.vector(EuropeanShortPutPrice)
Graph_FirstDigitalPayoff <- FirstDigitalCallPayoff - as.vector(FirstDigitalCallPrice)
Graph_SecondDigitalPayoff <- SecondDigitalCallPayoff - as.vector(SecondDigitalCallPrice)
Graph_ThirdDigitalPayoff <- ThirdDigitalCallPayoff - as.vector(ThirdDigitalCallPrice)

OverallPayoff <- rowSums(cbind(Graph_LongCallPayoff,
                               Graph_ShortCallPayoff,
                               Graph_LongPutPayoff,
                               Graph_ShortPutPayoff,
                               Graph_FirstDigitalPayoff,
                               Graph_SecondDigitalPayoff, 
                               Graph_ThirdDigitalPayoff))

# Generate a data_frame all vectors in order to plot the strategy payoffs using ggplot
results <- data.frame(cbind(Graph_LongCallPayoff,
                            Graph_ShortCallPayoff,
                            Graph_LongPutPayoff,
                            Graph_ShortPutPayoff,
                            Graph_FirstDigitalPayoff,
                            Graph_SecondDigitalPayoff, 
                            Graph_ThirdDigitalPayoff))

ggplot(results, aes(x=prices)) + 
  geom_line(linetype = "dashed", aes(y = Graph_LongCallPayoff, color = "LongCall")) + 
  geom_line(linetype = "dashed", aes(y = Graph_ShortCallPayoff, color = "ShortCall")) +
  geom_line(linetype = "dashed", aes(y = Graph_LongPutPayoff, color = "LongPut")) +
  geom_line(linetype = "dashed", aes(y = Graph_ShortPutPayoff, color = "ShortPut")) +
  geom_line(linetype = "dashed", aes(y = Graph_FirstDigitalPayoff, color = "FirstDigital")) +
  geom_line(linetype = "dashed", aes(y = Graph_SecondDigitalPayoff, color = "SecondDigital")) +
  geom_line(linetype = "dashed", aes(y = Graph_ThirdDigitalPayoff, color = "ThirdDigital")) +
  geom_line(aes(y = OverallPayoff, color="Payoff")) +
  scale_colour_manual("", 
                      breaks = c("LongCall", "ShortCall", "LongPut", "ShortPut",
                                 "FirstDigital", "SecondDigital", "ThirdDigital", "Payoff"),
                      values = c("darkred", "darkorange", "violet", "brown", 
                                 "darkgreen", "darkblue", "darkgrey", "black")) + 
  xlab("Undelying Price") +
  ylab("Payoff") +
  ggtitle("Product Payoff")  
