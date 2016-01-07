#------------------------------------#
#     A S S I G N M E N T   4        #
#------------------------------------#

cat("\014")

# Question 1
# Returns and autocorrelation

# Download stock for 2 years
rm(list = ls())
library(quantmod)

getSymbols(Symbols = "MSFT", from = "2013-10-01", to = "2015-10-01")
# of all the various prices given in the data fram I am choosing the adjusted price
# to calculate all the values in the tasklist
# msft.price <- data.frame(MSFT)$MSFT.Adjusted
msft <- data.frame(MSFT)
# head(msft)
# msft <- msft[0:5,]  # For testing purposes


# This is in ascending order.. flipping it
msft <- msft[nrow(msft):1,]

# making another column for n - 1 prices vector as for n enteries we will have n - 1 deltas
msft.shift_price <- msft$MSFT.Adjusted[1:length(msft$MSFT.Adjusted) - 1]
# make another column for t -1 prices so that we get two vectors to get delta
msft.shift_price1 <- msft$MSFT.Adjusted[2:length(msft$MSFT.Adjusted)]
length = length(msft.shift_price1)
# Calculating simple return
msft.simple_return <- (msft.shift_price - msft.shift_price1)/ msft.shift_price1  # Rt
# head(msft.simple_return)
# Calculating log return
msft.log_return <- log(msft.shift_price) - log(msft.shift_price1)       # rt
# head(msft.log_return)
# Calculating multiperiod simple return
msft.simple_return_m <- msft.shift_price/ msft$MSFT.Adjusted[length(msft$MSFT.Adjusted)] - 1

# Calculating multi period log return
msft.log_return_m <- log(msft.shift_price/ msft$MSFT.Adjusted[length(msft$MSFT.Adjusted)])
#print(msft.log_return_m)

# Re-Calculating multiperiod returns based on simple returns calculated above
size <- length(msft.simple_return)   # should be length - 1
msft.simple_return_m2 <- double(size)
msft.log_return_m2 <- double(size)

for (i in 1:size)
{
    msft.simple_return_m2[i] <- prod((msft.simple_return[i:size] + 1)) - 1
    print (msft.simple_return_m2[i])
}

for (i in 1:size)
{
    msft.log_return_m2[i] <- sum(msft.log_return[i:size])
    print (msft.log_return_m2[i])
}

result <- cbind.data.frame(msft$MSFT.Adjusted[1:size], msft.shift_price, msft.shift_price1, 
                 msft.simple_return, msft.simple_return_m, msft.simple_return_m2,
                 msft.log_return_m, msft.log_return_m2)

# Comparing columns for simple_return_m and simple_return_m2 and log_return_m and log_return_m2 we could qualify
# the relationship between single and multiperiod.


# Auto correlation function
# log return series
# Reusing the log vector here from earlier endeavors and employing acf function
# msft.log_return
acf_log <- acf(msft.log_return)
print(acf_log)
# Squared log return series
acf_log_sq <- acf(msft.log_return^2)
print(acf_log_sq)





# Question 2
# Geomatrical Brownian Motion
# Disclaimer: Used code provided in lecture 6'

rm(list = ls())
cat("\014")
## parameters
r <- 0.05
sigma <- 0.2
Maturity <- 1
steps <- 252
S0 <- 100
iterations <- 20
result = matrix(nrow = 20, ncol = 2)

# Start the loop
for (i in 1:iterations)
{
    set.seed(i)  # Set seed so we get same rnorm
    
    ## method 1: Euler Method
    dt <- Maturity / steps
    epsilon_t_vec <- rnorm(steps)
    epsilon_t_vec <- append(0, epsilon_t_vec)
    dwt_vec <- epsilon_t_vec * sqrt(dt)
    St_vec <- c()
    St_vec[1] <- S0
    for(j in 1:steps)
    {
        dwt <- dwt_vec[j+1]
        St_vec[j+1] <- St_vec[j] + r * St_vec[j] * dt + sigma * St_vec[j] * dwt
    }
    result[i, 1] = St_vec[steps+1]
    
    set.seed(i)
    ## method 2: Solution to GBM
    dt <- Maturity / steps
    epsilon_t_vec <- rnorm(steps)
    epsilon_t_vec <- append(0, epsilon_t_vec)
    cum_wt_vec <- cumsum(epsilon_t_vec) * sqrt(dt)
    ST <- S0 * exp(r*Maturity + cum_wt_vec[steps + 1] * sigma)
    
    result[i, 2] = ST
}

# Check terminal values by both methods side by side
print (result)
