# appropriate names and spaces
# good style

myVec1 <- c(4, 5, 6)        # numeric
myVec2 <- c(TRUE, FALSE)    # logical
myVec3 <- c('a', 'b', 'c')  # character

myMat <- matrix(x, nrow = 3, ncol = 3, byrow=T) # creating matrix

l <- list("John", 12345, "Male")

# bad style

a<-c(4,5,6)
aa<-c(TRUE,FALSE)
abc<-c('a','b','c')

dd<-matrix(abc,nrow=1,byrow=T)
ee<-list("John",12345,"Male")




# indention
# good style

integer <- 0
sum <- 0
while (integer <= 100)
{
    if (sum > 3000)     # break loop using if statement
        break
    sum = sum + integer
    print(sum)
    integer = integer + 1
}


# bad style
integer<-0
sum<-0
while(integer<=100)
{
    if (sum > 3000)     # break loop using if statement
        break
    sum = sum + integer
    print(sum)
    integer = integer + 1
}





# for function you need proper names and indent as well
# good style
call.BS <- function(S, K, r, Maturity, sigma)
{
    d1 <- ( log(S/K) + ( r + (sigma^2) / 2) * Maturity ) / ( sigma * sqrt(Maturity) )
    d2 <- d1 - sigma * sqrt(Maturity)
    
    return( S * pnorm(d1) - K * exp(-r*Maturity) * pnorm(d2) )
}

# bad style
call.BS<-function(S,K,r,Maturity,sigma)
{d1<-(log(S/K)+(r+(sigma^2)/2)*Maturity)/(sigma*sqrt(Maturity))
d2<-d1-sigma*sqrt(Maturity)
S*pnorm(d1)-K*exp(-r*Maturity)*pnorm(d2)}


#------------------------------------#
#     A S S I G N M E N T   2        #
#------------------------------------#

# Set working directory
cat("\014") 
setwd("D:/Stevens/Sem 3/FE515/Week 2")

# Read the csv files
dow <- read.csv("DOW.csv")
sp500 <- read.csv("SP500.csv")
# check the column names
names(dow)
names(sp500)
# get the length of the second vector to avoid calc inside loop
sp500_size <- length(sp500$Ticker.symbol)
# mandatory for loop
linebreak = ''
answer = ''
for (ticker in dow$Ticker)
{
    for (index in 1:sp500_size)
    {
        if (ticker == sp500$Ticker.symbol[index])
        {
            print(paste(ticker, index, sep = '--'))
            
            # Another way of doing things but curiously enough it suppresses 
            # linefeed character if it's at the end. Any place else works
            # which is rather inconvenient
            #if (length(answer) > 0)
            #{
            #    linebreak = "\n"
            #}
            #answer <- cat(answer, linebreak, ticker, "--", index)
            break # to quickly exit as soon we find first match
        }
    }
}

# Without using loop
print("Without using loop")
matches <- match(dow$Ticker, sp500$Ticker.symbol)
cat(paste("\n", dow$Ticker, '--', matches))

