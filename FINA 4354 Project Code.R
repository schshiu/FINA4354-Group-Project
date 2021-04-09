#FINA 4354 Project

# 1- Downloading data
library("xts")
library("quantmod")
Und.price.raw <- getSymbols("SPY", from = "2018-01-01", auto.assign = FALSE)
# if possible I want data from "past X years", I forgot, is this possible?! 
# e.g. Today is 7 Apr, so I want data from 2018-04-07
# i found that HSI has very high outliers of returns, up to 10^5, , dont know why
Und.price <- na.locf(Und.price.raw, na.rm = FALSE)      #fill up the NA values
# i think na.locf function may cause some errors, but na.approx does not apply to
# the last terms
head(Und.price, 3)
cat("------------------------------------------------------------\n")
tail(Und.price, 3)     #inspect data

# 1.2 find risk-free rate
# we use FRED 1-Year Treasury Constant Maturity Rate (DGS1) as proxy
# this is dynamic, so we use downloaded data instead of pre-set data
RFrate.raw <- getSymbols("DGS1", from = "2020-01-01", src = "FRED", auto.assign = FALSE)
# this "from" doesn't work for no reason
RF.rate <- 0.06 # change later

#-------------------------------------------------------------------------------

# 2- Storing data to local repository

# change the xts into dataframe
output_file =  data.frame(row.names = index(Und.price.raw) , coredata(Und.price.raw))

# determines the saving filename and directory
userpath = getwd()
write.csv(output_file, file = file.path(userpath, "output.csv"))
# i dont know how to write dependent paths

#-------------------------------------------------------------------------------

# 3- Loading data from local repository
Und.price <- read.csv(file = file.path(userpath, "output.csv"), row.names = 1)
# different from traditional read.csv command

#-------------------------------------------------------------------------------

# 4- Financial analysis
# 4.1 find returns
Und.return <- coredata(dailyReturn(Und.price))  #this is a vector
head(Und.return, 3)
cat("------------------------------------------------------------\n")
tail(Und.return, 3)     #inspect data

# 4.2 find parameters miu and sigma
Und.ret.miu <- mean(Und.return)
Und.ret.sigma <- sd(Und.return) / sqrt(1/252)  # annualized

# 4.3 set parameters
n <- nrow(Und.price)
S0 <- coredata(Und.price$SPY.Adjusted[n])   #the last day's adjusted price
T <- 1  # time till maturity
K <- S0 # at the money call option
r <- 0  # where can i find risk free interest rate???

# 4.4 For European call option:
# price is determined by Black-Scholes Formula
d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
d2 <- d1 - sigma * sqrt(T)
(C0 <- pnorm(d1) * S0 - pnorm(d2) * K * exp(-RF.rate * T))

