rm(list = ls())
library(Sim.DiffProc)   # simulate GBMs

source("params.R")
source("delta.R")
source("BlackScholes.R")

# simulate sample data
simData <- GBM(N = Steps, M = NPaths, x0 = S0, t0 = 0, T = Maturity, theta = Interest, sigma = Sigma)
simData <- t(simData) # take the transpose, each row is one path

plot.ts(simData[1, ], main = "One Path of Simulated Stock TS", ylab = "Price")

## steps of rehedging:

# 1. Sell option, get premium as money market account(MMA)
# 2. Calculate initial delta
# 3. Delta hedge the option by buying stocks
# 4. MMA = cash - stock * share
# 5. In a for loop, rehedge the option in several intermediate points
# 6. Claculate the option payoff
# 7. Return the P&L(value of MMA)



# =================== function ======================

# we want a function:
# take one sample path and number of rehedging points as parameters
# return the PNL of hedging

## Parameters

# onePath: one path of stock prices
# rehedging: number of rehedging
# optShare: number of shares of options to sell

onePath <- simData[1, ]
rehedging <- 5
optShare <- 1

# =================== function start ======================
# =================== function start ======================

optPrice <- call.BS(S = S0, K = Strike, r = Interest, Maturity = Maturity, sigma = Sigma)
hedgingPoints <- seq(from = 1, to = Steps + 1, by = ceiling((Steps + 1)/(rehedging+1)))
deltaVec <- calcDelta(onePath[hedgingPoints], Interest, Sigma, Strike, Maturity, hedgingPoints * dt)

# step 1
MMA <- optShare * optPrice

# step 2 - 4
stkAmount <- optShare * deltaVec[1]
MMA <- MMA - onePath[hedgingPoints[1]] * stkAmount
MMA <- MMA * exp(Interest * dt)

# step 5
for(i in 2:length(hedgingPoints))
{
    newStkAmount <- deltaVec[i] * optShare
    MMA <- MMA - (newStkAmount - stkAmount) * onePath[hedgingPoints[i]]
    MMA <- MMA * exp(Interest * dt)
    
    stkAmount <- newStkAmount
}

# step 6 - 7
optPayoff <- pmax(onePath[Steps + 1] - Strike, 0)
MMA <- MMA - optPayoff * optShare
MMA <- MMA + stkAmount * onePath[Steps + 1]

# ================= end function ====================
# ================= end function ====================


discreteHedgingBS <- function(onePath, rehedging, optShare = 1)
{
    optPrice <- call.BS(S = S0, K = Strike, r = Interest, Maturity = Maturity, sigma = Sigma)
    hedgingPoints <- seq(from = 1, to = Steps + 1, by = ceiling((Steps + 1)/(rehedging+1)))
    deltaVec <- calcDelta(onePath[hedgingPoints], Interest, Sigma, Strike, Maturity, hedgingPoints * dt)
    
    # step 1
    MMA <- optShare * optPrice
    
    # step 2 - 4
    stkAmount <- optShare * deltaVec[1]
    MMA <- MMA - onePath[hedgingPoints[1]] * stkAmount
    MMA <- MMA * exp(Interest * dt)
    
    # step 5
    for(i in 2:length(hedgingPoints))
    {
        newStkAmount <- deltaVec[i] * optShare
        MMA <- MMA - (newStkAmount - stkAmount) * onePath[hedgingPoints[i]]
        MMA <- MMA * exp(Interest * dt)
        
        stkAmount <- newStkAmount
    }
    
    # step 6 - 7
    optPayoff <- pmax(onePath[Steps + 1] - Strike, 0)
    MMA <- MMA - optPayoff * optShare
    MMA <- MMA + stkAmount * onePath[Steps + 1]
    
    return(MMA)
}



rm(list = ls())

source("params.R")
source("delta.R")
source("BlackScholes.R")
discreteHedgingBS(onePath = simData[1, ], rehedging = 5)

Interest <- 0
PNLVec <- c()
for(i in 1:NPaths)
{
    PNLVec[i] <- discreteHedgingBS(onePath = simData[i, ], rehedging = 253)
}
sd(PNLVec)
