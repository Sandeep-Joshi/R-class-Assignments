# ==================== Variance and Realized Volatility =======================

# Clear console
cat("\014") 
# Set working directory
setwd("D:/Stevens/Sem 3/FE515/Week 7")  # Comment this line

# get data
library(quantmod)
spy <- getSymbols("SPY", auto.assign = F)
spy <- data.frame(spy)
spy_rtn <- diff(log(spy$SPY.Adjusted))
plot.ts(spy_rtn)

# variance
var(spy_rtn)
# [1] 0.0001862294

# realized variance
realized_var <- sum(spy_rtn^2)/length(spy_rtn)
realized_var

# realized volatility
realized_vol <- sqrt(realized_var)
realized_vol


# ==================== Implied Volatility, Volatility Smile ===================

rm(list = ls())

### Now I am trying to make a plot of volatility smile

# get option data
spy_opt_chain <- getOptionChain("SPY", Exp = "2015-12-04")
spy_calls <- spy_opt_chain$calls
spy_puts <- spy_opt_chain$puts
# get stock price
spy_quote <- getQuote("SPY")


# save to csv in case yahoo API doesn't work
write.csv(x = spy_calls, file = "spy_calls.csv", row.names = F)
write.csv(x = spy_puts, file = "spy_puts.csv", row.names = F)
write.csv(x = spy_quote, file = "spy_quote.csv", row.names = F)


## read from csv file
# spy_calls <- read.csv("spy_calls.csv")
# spy_puts <- read.csv("spy_puts.csv")
# spy_quote <- read.csv("spy_quote.csv")


## use call options as an example
## we want to find out the implied volatility of these values:
CallVec <- spy_calls$Last

# with the following parameters:
# strikes
S0 <- spy_quote$Last
r <- 0.01
Maturity <- (as.Date("2015-10-30") - as.Date("2015-10-19"))/365
StrikeVec <- spy_calls$Strike



# calculate the implied vol
library(RQuantLib)
?EuropeanOptionImpliedVolatility
EuropeanOptionImpliedVolatility(type = "call", value = CallVec[1], underlying = S0, strike = StrikeVec[1], dividendYield = 0, riskFreeRate = 0.01, maturity = Maturity, volatility = 0.1)
# Error occured
# Error in europeanOptionImpliedVolatilityEngine(type, value, underlying,  : 
# root not bracketed: f[1e-007,4] -> [4.413856e-001,9.542064e+000]

## Moneyness in (0.95, 1.05)
idx <- (StrikeVec/S0 > 0.96 & StrikeVec/S0 < 1.04)
idx
StrikeVec <- StrikeVec[idx]
CallVec <- CallVec[idx]

# what is the data structure of this implied volatility?
tmp_res <- EuropeanOptionImpliedVolatility(type = "call", value = CallVec[1], underlying = S0, strike = StrikeVec[1], dividendYield = 0, riskFreeRate = 0.01, maturity = Maturity, volatility = 0.1)
typeof(tmp_res)
# [1] "double"
as.numeric(tmp_res)


# get the volatility smile
implied_vol_vec <- c()
for(i in 1:length(StrikeVec))
{
    tmp_res <- EuropeanOptionImpliedVolatility(type = "call", 
                                               value = CallVec[i], 
                                               underlying = S0, 
                                               strike = StrikeVec[i], 
                                               dividendYield = 0, 
                                               riskFreeRate = 0.01, 
                                               maturity = Maturity, 
                                               volatility = 0.1)
    implied_vol_vec[i] <- as.numeric(tmp_res)
    
    
}

plot.ts(implied_vol_vec)
# volatility smirk...



# Let's try puts
rm(list=ls())

# start over
spy_puts <- read.csv("spy_puts.csv")
spy_quote <- getQuote("SPY")

PutVec <- spy_puts$Last
S0 <- spy_quote$Last
r <- 0.01
Maturity <- (as.Date("2015-10-30") - as.Date("2015-10-19"))/365
StrikeVec <- spy_puts$Strike


# moneyness
idx <- (StrikeVec/S0 > 0.96 & StrikeVec/S0 < 1.04)
idx
StrikeVec <- StrikeVec[idx]
PutVec <- PutVec[idx]

implied_vol_vec <- c()
for(i in 1:length(StrikeVec))
{
    tmp_res <- EuropeanOptionImpliedVolatility(type = "put", 
                                               value = PutVec[i], 
                                               underlying = S0, 
                                               strike = StrikeVec[i], 
                                               dividendYield = 0, 
                                               riskFreeRate = 0.01, 
                                               maturity = Maturity, 
                                               volatility = 0.1)
    implied_vol_vec[i] <- as.numeric(tmp_res)
    
    
}

plot.ts(implied_vol_vec)
