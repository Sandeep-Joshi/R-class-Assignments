#------------------------------------#
#     A S S I G N M E N T   2        #
#------------------------------------#

# Clear console
cat("\014") 
# Set working directory
setwd("D:/Stevens/Sem 3/FE515/Week 2")  # Comment this line

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


# Fair Dice
fair_Dice <- function(rolls)
{
    result <- NULL
    dice_total <- 0
    for (roll in 1:rolls)
    {
        tmp <- sample(x=c(1,2,3,4,5,6), size = 1, replace = T, prob = rep(1/6, 6))
        if (roll > 1)
        {
            result <- c(result, dice_total/ (roll - 1))
        }
        dice_total <- dice_total + tmp  # this is done after we calculate the n-1 mean as asked in prob
    }
    
   plot(2:rolls, result, type = "l")
   cat("For ", rolls, " rolls, series converged at :", result[length(result)])
}
fair_Dice(1000)

# Loaded Dice
loaded_Dice <- function(rolls, prob)
{
    result <- NULL
    dice_total <- 0
    # Compute the last probablity if only 5 or less
    diff = 6 - length(prob)
    if (diff > 0)
    {
        prob <- append(prob, rep(((1 - sum(prob))/diff), diff)) # distributing remanant probablities
    }
    
    for (roll in 1:rolls)
    {
        tmp <- sample(x=c(1,2,3,4,5,6), size = 1, replace = T, prob = prob)
        dice_total <- dice_total + tmp  # this is done after we calculate the n-1 mean
    }
    
    return(dice_total/rolls)
}

# Only four probablities are given in the function call
cat("Mean of all rolls is:", loaded_Dice(1000, c(1/5, 1/5, 1/4, 1/8)))  


# Highest payoff

# We know that max expectation value for a dice roll is 3.5, so we will
# employ using decision pivot at value 4 i.e if first roll is less than 4
# we will roll again. Explanation is in the Report

fair_Dice <- function(rolls, pivot)
{
    result <- NULL
    dice_total <- 0
    for (roll in 1:rolls)
    {
        tmp <- sample(x=c(1,2,3,4,5,6), size = 1, replace = T, prob = rep(1/6, 6))
        if (tmp < pivot) 
        {
            # re-roll
            tmp <- sample(x=c(1,2,3,4,5,6), size = 1, replace = T, prob = rep(1/6, 6))
        }
        dice_total <- dice_total + tmp
    }
    return(dice_total/rolls)
    
}
rolls = 10000
cat("For ", rolls, " rolls, series average is: ", fair_Dice(rolls, 4))



