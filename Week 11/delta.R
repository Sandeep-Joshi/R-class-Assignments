# calculate delta: N(d1)
calcDelta <- function(currPrice, interest, sigma, strike, maturity, currenttime)
{
    d1 <- 1/sqrt(sigma * (maturity-currenttime)) * (log(currPrice/strike) + (interest + sigma^2/2) * (maturity-currenttime))
    delta <- pnorm(d1)    
    return(delta)
}

# deltaVec <- c()
# dt <- Maturity / Steps
# for(i in 1:Steps)
# {
#     currT <- i * dt
#     tmpDelta <- calcDelta(currPrice = simData[3, i], interest = Interest, sigma = Sigma, strike = Strike, maturity = Maturity, currenttime = currT)
#     deltaVec[i] <- tmpDelta    
# }
# deltaVec
# plot.ts(deltaVec)