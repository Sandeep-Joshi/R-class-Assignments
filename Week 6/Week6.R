# Agenda
# 1. Return
# 2. Autocorrelation
# 3. Geometrical Brownian Motion
#     - Brownian Motion
#     - simulation
#     - estimating the parameters


# ========================== Return ===========================

# download data
library(quantmod)
spy <- getSymbols("SPY", auto.assign = F)
spy <- data.frame(spy)

spy_price <- spy$SPY.Adjusted

# simple return
spy_pt <- spy_price[-1]     # delete the first element
spy_pt_1 <- spy_price[-length(spy_price)]
spy_simple_rtn <- spy_pt/spy_pt_1 - 1

# log return
spy_log_rtn <- diff(log(spy_price))

## verify the relationship between single period and multiple periods simple returns
# single period simple returns
spy_simple_rtn
# simple return of many periods
spy_simple_rtn_kperiods <- spy_price[length(spy_price)]/spy_price[1] - 1


# ========================== 2. Auto correlation function ===========================

# now we already have the log return series of SPY
spy_price
spy_log_rtn

# calculate the auto correlation function 
acf(spy_log_rtn)
acf(spy_log_rtn^2)

# function acf returns an object
spy_acf <- acf(spy_log_rtn)
mode(spy_acf)
# [1] "list"


names(spy_acf)
# [1] "acf"    "type"   "n.used" "lag"    "series" "snames"


# An object of class "acf", which is a list with the following elements:
spy_acf$acf     # a matrix of the value of autocorrelation functions
spy_acf$type    # [1] "correlation"
spy_acf$n.used  # number of observations
spy_acf$lag     # up to lag 33 by default
spy_acf$series  # variable name of the series
spy_acf$snames  # The series names for a multivariate time series.



# ========================== 3. Brownian Motion ===========================


## Brownian Motion
steps <- 252
Maturity <- 1
dt <- Maturity / steps

epsilon_vec <- rnorm(steps + 1) # a vector of standard normal observations
print(epsilon_vec)
dwt_vec <- epsilon_vec * sqrt(dt)

# dwt_vec are the increments of Brownian Motions
# mean 0 and variance dt
hist(dwt_vec)
mean(dwt_vec)
sd(dwt_vec)^2
dt

# the path of Wt is the cumulative sum of dwt_vec
wt_vec <- cumsum(dwt_vec)
plot.ts(wt_vec)


# ========================== 4. Geometrical Brownian Motion ===========================

rm(list = ls())

## parameters
r <- 0.05
sigma <- 0.2
Maturity <- 1
steps <- 252
S0 <- 100



## method 1: Euler Method
dt <- Maturity / steps
epsilon_t_vec <- rnorm(steps)
epsilon_t_vec <- append(0, epsilon_t_vec)
print(epsilon_t_vec)
dwt_vec <- epsilon_t_vec * sqrt(dt)
print(dwt_vec)
St_vec <- c()
St_vec[1] <- S0
print(St_vec)
for(i in 1:steps)
{
    dwt <- dwt_vec[i+1]
    St_vec[i+1] <- St_vec[i] + r * St_vec[i] * dt + sigma * St_vec[i] * dwt
}


## method 2: Solution to GBM
dt <- Maturity / steps
epsilon_t_vec <- rnorm(steps)
epsilon_t_vec <- append(0, epsilon_t_vec)
cum_wt_vec <- cumsum(epsilon_t_vec) * sqrt(dt)
ST <- S0 * exp(r*Maturity + cum_wt_vec[steps + 1] * sigma)


# ========================== 5. Estimate mu and sigma based on one Path of GBM ===========================

source("GBM.R")
st_vec <- simOne_keep_path(S0, r, Maturity, steps, sigma)
st_rtn <- diff(log(st_vec))
plot.ts(st_rtn)

dt <- Maturity / steps
sigma <- sd(st_rtn) / sqrt(dt)
mu <- mean(st_rtn) / dt + sigma^2 / 2

mu
sigma