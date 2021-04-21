#FINA 4354 Project
rm(list = ls())
options(scipen = 999) #<-prevent using scientific notation

#-------------------------------------------------------------------------------
# 1 - Preparation steps
# Check if client's computer has the library downloaded, then load the required library
list_of_library <- c('xts', 'quantmod', 'ggplot2')
# Note: 'lubridate' can be added if date calculation is needed
for (i in list_of_library) {
  print(i)
  if (i %in% rownames(installed.packages()) == FALSE) {
    install.packages(i, character.only = TRUE)
  }
  library(i, character.only = TRUE)
}
rm(list_of_library, i) #Free up memory

#-------------------------------------------------------------------------------
# 2 - Data download
# 2.1 - Find the dividend yield with S&P 500
#Raw Data
SP500_raw <- na.locf(getSymbols("^GSPC",
                               from = "2018-01-01", 
                               auto.assign = FALSE)) #S&P500 Index
SP500TR_raw <- na.locf(getSymbols("^SP500TR", 
                                 from = "2018-01-01", 
                                 auto.assign = FALSE)) #S&P500 Total Return Index
SPY <- na.locf(getSymbols("SPY", 
                          from = "2018-01-01", 
                          auto.assign = FALSE))
#Get daily return of 2020
SP500_DayRet <- dailyReturn(SP500_raw$GSPC.Adjusted, 
                                         subset = NULL,
                                         type = 'log')
SP500TR_DayRet <- dailyReturn(SP500TR_raw$SP500TR.Adjusted,
                                              subset = NULL, 
                                          type = 'log')
#Get Dividend Yield approximation
DividendYield <- sum((SP500TR_DayRet['2020'] - SP500_DayRet['2020']) *
                      SP500_raw['2020',"GSPC.Adjusted"])/
                      SP500_raw['2020-12-31',"GSPC.Adjusted"]

# 2.2 find risk-free rate
# We prepare the 1m, 3m, 6m, 1y version of risk-free rate
#DGS1MO <- na.locf(getSymbols("DGS1MO", src = "FRED", auto.assign = FALSE))
#DGS3MO <- na.locf(getSymbols("DGS3MO", src = "FRED", auto.assign = FALSE))
DGS6MO <- na.locf(getSymbols("DGS6MO", src = "FRED", auto.assign = FALSE))
#DGS1YR <- na.locf(getSymbols("DGS1", src = "FRED", auto.assign = FALSE))

rm(SP500TR_DayRet, SP500TR_raw)
#-------------------------------------------------------------------------------

# 3 - Financial Model
# 3.1 Get parameters
n <- nrow(DGS6MO)
r <- as.numeric(coredata(DGS6MO$DGS6MO[n]))             #the last day's risk-free rate
n <- nrow(SP500_raw)
S <- as.numeric(coredata(SPY$SPY.Adjusted[n]))          #the last day's adjusted price
sigma <- as.numeric(sd(dailyReturn(SPY$SPY.Adjusted)) * sqrt(252))
q <- as.numeric(coredata(DividendYield[1]))
t <- 0.5

rm(n)  #remove unused variables

# 3.2 Option Pricing Functions
  #S = Spot Price, K = Strike Price, H = Barrier Price
  #r = Expected Return, q = Dividend Yield
  #sigma = volatility, t = time to maturity

fd1 <- function(S, K, r, q, sigma, t) {
  d1 <- (log(S / K) + (r - q + 0.5 * sigma ^ 2) * t) / (sigma * sqrt(t)) 
  return(d1) #d2 <- d1 - sigma*sqrt(t)
}

fBSCallOptionPrice <- function(S, K, r, q, sigma, t) { 
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- S * exp(-q * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  return(price)
}

fBSPutOptionPrice <- function(S, K, r, q, sigma, t) {
  d1 <- fd1(S, K, r, q, sigma, t)
  d2 <- d1 - sigma * sqrt(t) 
  price <- K * exp(-r * t) * pnorm(-d2) - S * exp(-q * t) * pnorm(-d1)
  return(price)
}

fBSUpAndOutBarrierOptionPrice <- function(S, K, H, r, q, sigma, t) {
  if (K >= H) {
    return (0)
    }
  else {
    A <- fBSCallOptionPrice(S, K, r, q, sigma, t)
    B <- fBSCallOptionPrice(S, H, r, q, sigma, t)
    C <- (H - K) * exp(-r * t) * pnorm(fd1(S, H, r, q, sigma, t) - sigma * sqrt(t))
    v <- r - q - sigma ^ 2 /2 
    D <- (H / S) ^ (2 * v / (sigma ^ 2))
    E <- fBSCallOptionPrice(H ^ 2 / S, K, r, q, sigma, t)
    F1 <- fBSCallOptionPrice(H ^ 2 / S, H, r, q, sigma, t)
    G <- (H - K) * exp(-r * t) * pnorm(fd1(H, S, r, q, sigma, t) - sigma * sqrt(t))
    price  <- A - B - C - D * (E - F1 - G)
    return(price)  
    }
}

fBSDownAndOutBarrierOptionPrice <- function(S, K, H, r, q, sigma, t) {
  A <- fBSCallOptionPrice(S, max(H, K), r, q, sigma, t)
  v <- r - q - sigma ^ 2 /2 
  B <- (H / S) ^ (2 * v / (sigma ^ 2))
  C <- fBSCallOptionPrice(H ^ 2 / S, max(H, K), r, q, sigma, t)
  D <- (max(H, K) - K) * exp(-r * t)
  E <- pnorm(fd1(S, max(H, K), r, q, sigma, t) - sigma * sqrt(t))
  F1 <- pnorm(fd1(H ^ 2 / S, max(H, K), r, q, sigma, t) - sigma * sqrt(t))
  price <- A - B * C + D * (E - B * F1)
  return(price)
  }

fBSDigitalCallOption <- function(S, K, r, q, sigma, t) {
  price <- exp(-r * t) * pnorm(fd1(S, K, r, q, sigma, t) - sigma * sqrt(t))
  return(price)
}

#-------------------------------------------------------------------------------
# Plot expected payoff graph
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

k_1 <- S * 0.6  # Low Barrier to protect 
k_2 <- S * 1.05 # First Strike (Long Call) 
k_3 <- S * 1.1  # Second Strike (Short Call & Digital Call)
k_4 <- S * 1.15 # Third Strike (Digital Call)
k_5 <- S * 1.2  # Last Strike (Digital Call)

# Prices of every options
#BarrierOptionPrice <- fBSDownAndOutBarrierOptionPrice(S, k_2, k_1, r, q, sigma, t)
EuropeanLongCallPrice <- fBSCallOptionPrice(S, k_2, r, q, sigma, t)
EuropeanShortCallPrice <- -1 * fBSCallOptionPrice(S, k_3, r, q, sigma, t)
EuropeanLongPutPrice <- fBSPutOptionPrice(S, k_1, r, q, sigma, t)
EuropeanShortPutPrice <- -1 * fBSPutOptionPrice(S, k_2, r, q, sigma, t)
FirstDigitalCallPrice <- fBSDigitalCallOption(S, k_3, r, q, sigma, t)
SecondDigitalCallPrice <- fBSDigitalCallOption(S, k_4, r, q, sigma, t)
ThirdDigitalCallPrice <- fBSDigitalCallOption(S, k_5, r, q, sigma, t)

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


#-------------------------------------------------------------------------------
# Misc: Storing data to local repository
# change the xts into dataframe
#list_of_rawdata <- c('SP500_raw')
for (i in list_of_rawdata) {
  output_file <-  data.frame(row.names = index(i) , coredata(i))
  # determines the saving filename and directory
  userpath <- getwd()
  filename <- paste(i ,sep = "", '.csv')
  write.csv(output_file, file = file.path(userpath, filename))
}
#-------------------------------------------------------------------------------
# Misc: Loading data from local repository
for (i in list_of_rawdata) {
  userpath <- getwd()
  filename <- paste(i ,sep = "", '.csv')
  read.csv(file = file.path(userpath, filename), row.names = 1)
}
#-------------------------------------------------------------------------------
