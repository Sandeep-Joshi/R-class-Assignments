#------------------------------------#
#     A S S I G N M E N T   6        #
#------------------------------------#

# Clear console
cat("\014") 
# Set working directory
setwd("D:/Stevens/Sem 3/FE515/Week 10")  # Comment this line
library(quantmod)

# S: Current stock price
# K: Option striking price/ Excercise price
# r: Risk free interest rate
call.BS<-function(S, K, r, Maturity,sigma)
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity))
    d2<-d1-sigma*sqrt(Maturity)
    return(S*pnorm(d1)-K*exp(-r*Maturity)*pnorm(d2))
}

dcall.BS<-function(S, K, r, Maturity,sigma)
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity))
    d2<-d1-sigma*sqrt(Maturity)
    dd1 <- (sigma^2*Maturity^(3/2) - (log(S/K)+(r+sigma^2/2)*Maturity)*sqrt(Maturity))/(sigma^2*Maturity)
    dd2 <- d1-sqrt(Maturity)
    r <- (S*dnorm(d1)*dd1 - K*exp(-r*Maturity)*dnorm(d2)*dd2)
    #print(r)
    return(r)
}


put.BS<-function(S, K, r, Maturity,sigma)
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity));
    d2<-d1-sigma*sqrt(Maturity);
    return(K*exp(-r*Maturity)*pnorm(-d2)-S*pnorm(-d1))
}

dput.BS<-function(S, K, r, Maturity,sigma)
{
    d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity))
    d2<-d1-sigma*sqrt(Maturity)
    dd1 <- (sigma^2*Maturity^(3/2) - (log(S/K)+(r+sigma^2/2)*Maturity)*sqrt(Maturity))/(sigma^2*Maturity)
    dd2 <- d1-sqrt(Maturity)
    return(K*exp(-r*Maturity)*dnorm(-d2)*(dd2) - S*dnorm(-d1)*(dd1))
}

vega <- function(S, K, r, Maturity, sigma)
{
    d1 <- (log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity))
    r <- S * sqrt(Maturity)*pnorm(d1)
    print(paste('Vega',r))
    return(r)
}

impvol <- function(S, K, r, Maturity, sigma, type, stock){
    #print (paste("sigma:",sigma))
    tmp = sigma         # initial guess.. used as in EuropeanOptionImpliedVolatility
    epsilon = 0.01  # convergent condition
    count <- 0
    res <- 0
    # repeat till desired convergence using Newton's method
    while(count < 100)
    {
        if (type == 'C'){
            # res <- call.BS(S, K, r, Maturity, sigma) - stock  # pass delta here
            res <- call.BS(S, K, r, Maturity, sigma) / dcall.BS(S, K, r, Maturity, sigma)
            #print(paste(res,tmp))
            res <- tmp - res
        } else {
            #res <- put.BS(S, K, r, Maturity, sigma) -stock
            res <- tmp - put.BS(S, K, r, Maturity, sigma) / dput.BS(S, K, r, Maturity, sigma)
        }
        #print(paste(res, tmp))
        if (abs(res) < epsilon) # res - tmp
        {
            print("Converged!")
            return(res)
            break
        }
        
        tmp <- res
        count <- count+1
    }
    return(res)
}

APPL <- getOptionChain("SPY", "2015-12-04")
APPL_QUOTE <- getQuote("SPY") # "MARK"
head(APPL$calls)
head(APPL$puts)
print(APPL$puts)
print(APPL_QUOTE)

# Calls are out of money with strike less than stock
# data_calls <- subset(APPL$calls, Strike <=  APPL_QUOTE$Open)
data_calls <- subset(APPL$calls, Strike/APPL_QUOTE$Open > 0.96 & Strike/APPL_QUOTE$Open < 1.04)

# puts are out of money with strike more than stock
#data_puts <- subset(APPL$puts, Strike >=  APPL_QUOTE$Open)
print(paste('value', APPL_QUOTE$Open))
data_puts <- subset(APPL$puts, APPL_QUOTE$Open/Strike > 0.96 & APPL_QUOTE$Open/Strike < 1.04 & Bid < 6.4)
print(data_puts)
# call implied volatility
res_call <- c()
res2_call <- c()
res_put <- c()
res2_put <- c()

Maturity <- as.numeric((as.Date("2015-12-04") - Sys.Date())/365)

S <- APPL_QUOTE$Open
r <- 0.01 # Assumed as didn't find in the pdf
init <- 0.1

# For Calls ('S, K, r, Maturity, sigma, type, stock)
for (i in 1:nrow(data_calls)){
    res_call[i] <- impvol(S, data_calls$Strike[i], r, Maturity, init, 'C', data_calls$Last[i])
    
    tmp <- EuropeanOptionImpliedVolatility(type = "call", 
                                           value = data_calls$Last[i], 
                                           underlying = S, 
                                           strike = data_calls$Strike[i], 
                                           dividendYield = 0, 
                                           riskFreeRate = r, 
                                           maturity = Maturity, 
                                           volatility = init)
    res2_call[i] <- as.numeric(tmp)
}

#print(res2_call)
#print(res_call)

# For Puts
for (i in 1:nrow(data_puts)){
    res_put[i] <- impvol(S, data_puts$Strike[i], r, Maturity, init, 'P', data_puts$Last[i])
    print(paste(data_puts$Last[i], data_puts$Strike[i]))
    
    tmp <- EuropeanOptionImpliedVolatility(type = "put", 
                                           value = data_puts$Last[i], 
                                           underlying = S, 
                                           strike = data_puts$Strike[i], 
                                           dividendYield = 0, 
                                           riskFreeRate = r, 
                                           maturity = Maturity, 
                                           volatility = init)
    res2_put[i] <- as.numeric(tmp)
}

# Plot the result
print((res_put))
print((res2_put))

par(mfrow=c(2,2))
# Call in our method
plot(data_calls$Strike, res_call, typ="l", col="green",
     main=c("Call", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")
lines(data_calls$Strike, res_call, col="blue")
abline(v=S, col="red", lty=2)
legend("bottomleft", cex=0.9, c("Ask", "Bid"), lty=1, col=c("blue", "green"))

# Put with our method
plot(data_puts$Strike, res_put, typ="l", col="green",
     main=c("Put", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")
lines(data_puts$Strike, res_put, col="blue")
abline(v=S, col="red", lty=2)
legend("bottomleft", cex=0.9, c("Ask", "Bid"), lty=1, col=c("blue", "green"))

# Call with built in method
plot(data_calls$Strike, res2_call, typ="l", col="green",
     main=c("EuropeanImpliedVol. Method Call", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")
lines(data_calls$Strike, res2_call, col="blue")
abline(v=S, col="red", lty=2)
legend("bottomleft", cex=0.9, c("Last"), lty=1, col=c("blue"))

# put with built in method
plot(data_puts$Strike, res2_put, typ="l", col="green",
     main=c("EuropeanImpliedVol. Method Put", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")
lines(data_puts$Strike, res2_put, col="blue")
abline(v=S, col="red", lty=2)
legend("bottomleft", cex=0.9, c("Last"), lty=1, col=c("blue"))
