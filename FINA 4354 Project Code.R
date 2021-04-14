#FINA 4354 Project

# 1. Preparation settings
library("xts")
library("quantmod")
library("lubridate")
setwd("/Users/fuxipeng/1/港大学习资料/FINA4354/FINA 4354 GP")
# the working directory has to be set before running on other computers

#-------------------------------------------------------------------------------

# 2. Download data and initial analysis
# 2.1. Download underlying data

# we have downloaded SPY for the last 10 years, 
# but only the last 3 years are for simulation purposes
Und.price.raw <- getSymbols("SPY", from = Sys.Date() - years(x = 10), 
                            auto.assign = FALSE)

# Use graph to show trend
barChart(Und.price.raw, theme = "white.mono", bar.type = "hlc")
# A general upward trend, but there are few but large falls

# filtering out needed data
Und.price <- na.approx(window(Und.price.raw, start = Sys.Date() - years(x = 3)), 
                       na.rm = FALSE)
# na.approx will reduce errors
# na.locf will fill the gap at the end
Und.return <- coredata(dailyReturn(Und.price))  #this is a vector

# QQ plot analysis
qqnorm(Und.return)
# we can see the return is not strictly normally distributed
# there are LHS -ve and RHS +ve outliers, and the main line is not straight

# 2.2 find risk-free rate
# we use FRED 6-month Treasury Constant Maturity Rate (DGS6MO) as proxy
# this is dynamic, so we use downloaded data instead of pre-set data
RFrate.raw <- getSymbols("DGS6MO", from = Sys.Date() - months(x = 1), src = "FRED", auto.assign = FALSE)
# this "from" doesn't work for no reason
RF.rate <- mean(last(RFrate.raw, 10))
# we use the average of last 10 obs

#-------------------------------------------------------------------------------

# 2- Storing data to, and loading data from local repository
# determines the saving filename and directory
saveRDS(Und.price, file = "/data/Und_Prices_Processed")
# is this dependent path after I set working directory?

Und.price <- readRDS("/data/Und_Prices_Processed")
# this save file is not a tabular file
# different from traditional read.csv command

#-------------------------------------------------------------------------------

# 3- Financial analysis
# 3.2 find parameters miu and sigma
Und.r.miu <- mean(Und.return)
Und.r.sigma <- sd(Und.return) / sqrt(1/252)  # annualized

# 3.3 set parameters
n <- nrow(Und.price)
S0 <- coredata(Und.price$SPY.Adjusted[n])   #the last day's adjusted price
T <- 1  # time till maturity
K <- S0 # at the money call option
r <- 0  # where can i find risk free interest rate???

# 3.4 For European call option:
# price is determined by Black-Scholes Formula
d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
d2 <- d1 - sigma * sqrt(T)
(C0 <- pnorm(d1) * S0 - pnorm(d2) * K * exp(-RF.rate * T))
