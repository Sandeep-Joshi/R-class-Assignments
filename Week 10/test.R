#Black-Scholes Function
BS <-
    function(S, K, T, r, sig, type="C"){
        d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
        d2 <- d1 - sig*sqrt(T)
        if(type=="C"){
            value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
        }
        if(type=="P"){
            value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
        }
        return(value)
    }


## Function to find BS Implied Vol using Bisection Method
implied.vol <-
    function(S, K, T, r, market, type){
        sig <- 0.20
        sig.up <- 1
        sig.down <- 0.001
        count <- 0
        err <- BS(S, K, T, r, sig, type) - market 
        
        ## repeat until error is sufficiently small or counter hits 1000
        while(abs(err) > 0.00001 && count<1000){
            if(err < 0){
                sig.down <- sig
                sig <- (sig.up + sig)/2
            }else{
                sig.up <- sig
                sig <- (sig.down + sig)/2
            }
            err <- BS(S, K, T, r, sig, type) - market
            count <- count + 1
        }
        
        ## return NA if counter hit 1000
        if(count==1000){
            return(NA)
        }else{
            return(sig)
        }
    }

# read in data
dat <- read.csv("SPX_data.csv")

## calculate implied vol for Call
S <- 1082.74
T <- 28/365
r <- 0.01

n <- dim(dat)[1]
c.vol.Ask <- rep(0,n)
c.vol.Bid <- rep(0,n)
p.vol.Ask <- rep(0,n)
p.vol.Bid <- rep(0,n)

for(i in 1:n){
    c.vol.Ask[i] <- implied.vol(S, dat$K[i], T, r, dat$C.Ask[i], "C")
    c.vol.Bid[i] <- implied.vol(S, dat$K[i], T, r, dat$C.Bid[i], "C")
    p.vol.Ask[i] <- implied.vol(S, dat$K[i], T, r, dat$P.Ask[i], "P")
    p.vol.Bid[i] <- implied.vol(S, dat$K[i], T, r, dat$P.Bid[i], "P")
}

# plot Volatility Smile and Open Interest
par(mfrow=c(2,2))
plot(dat$K, c.vol.Bid, typ="l", col="green", xlim=c(800, 1350), ylim= c(.2, .60),
     main=c("S&P 500 June Call", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")
lines(dat$K, c.vol.Ask, col="blue")
abline(v=S, col="red", lty=2)
legend("bottomleft", cex=0.9, c("Ask", "Bid"), lty=1, col=c("blue", "green"))

plot(dat$K, p.vol.Bid, typ="l", col="green", xlim=c(800, 1350), ylim= c(.2, .6),
     main=c("S&P 500 June Put", "Volatility Smile"), xlab="Strike", ylab="Implied Vol")
lines(dat$K, p.vol.Ask, col="blue")
abline(v=S, col="red", lty=2)
legend("bottomleft", cex=0.9, c("Ask", "Bid"), lty=1, col=c("blue", "green"))

plot(dat$K, dat$C.Open, xlim=c(800, 1350), ylim=c(0, 260000), type="h",
     main="Open Interest", xlab="Strike", ylab="Open Interest")
abline(v=S, col="red", lty=2)

plot(dat$K, dat$P.Open, xlim=c(800, 1350), ylim=c(0, 260000), type="h",
     main="Open Interest", xlab="Strike", ylab="Open Interest")
abline(v=S, col="red", lty=2)