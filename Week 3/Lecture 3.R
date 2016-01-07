# Agenda

# 1. Data Frame
# 2. Vectorized Operation in R
# 3. Apply Functions
# 4. Generating Random Variables


# ======================= Data Frame ===================================

# two vectors
kids <- c("Joe", "Jill")
ages <- c(11, 12)

typeof(kids)
typeof(ages)

# create a dataframe
df <- data.frame(kids, ages)
df
# create a list
df2 <- list(kids, ages)

# both are list
typeof(df)
typeof(df2)

# subsetting data frames
df$kids
df$ages

# strings as factors
df <- data.frame(kids, ages)
df$kids                         # factors

df <- data.frame(kids, ages, stringsAsFactors = F)
df$kids                         # strings


## create data frames by reading stock data
aapl <- read.csv("AAPL.csv", stringsAsFactors = F)
head(aapl)
typeof(aapl)

# subsetting a dataframe
aapl$Date
aapl$Adj.Close

aapl[1]
aapl[[1]]

# Question: what is the difference between above two subsetting methods?




# columns in a dataframe must have the same size
apply(X = aapl, MARGIN = 2, FUN = length)
# ======================= Vectorized Operation =========================

rm(list = ls())
# will simplify your code and increase the performance

x <- 1:4
y <- 6:9

# want to do x + y

# using for loop
result <- c()
for (i in x)
{
    result[i] = x[i] + y[i]
}
result


# apply "+" on vectors
result <- x + y
result
rbind(x, y, result)


# another example
x <- 1:4
x > 2

# make the result prettier
rbind(x, x>2)
is.matrix(cbind(x, x>2))



# Q: How to show TRUE/FALSE in a similar table


data.frame(x, x > 2)



# more examples
x + y
x * y
x / y
x == y


# user defined functions
mySquare <- function(x)
{
    return (x^2)
}


x <- 1:4
y <- 6:9

mySquare(x)
mySquare(y)

## Example: Vectorized operation with the coin flipping example

No.heads <- 0
result.Vec <- NULL
for (flips in 1:1000)
{
    # x = c(1, 0), 1 means head, 0 means tail
    tmp <- sample(x=c(1, 0), size=1, replace=T, prob=c(0.5, 0.5))   
    # add tmp to number of head, if tmp = 1..., if tmp = 0, ...
    No.heads <- No.heads + tmp      
    result.Vec <- c(result.Vec,No.heads/flips)
}
plot(1:1000, result.Vec, type="l")     # produce a figure

## use vectorized operation instead of a for loop
?cumsum

sims <- sample(c(1, 0), size = 1000, replace = T, prob = c(0.5, 0.5))
cum_sims <- cumsum(sims)            # cumulative Sums
result.Vec <- cum_sims / (1:1000)   # vectorized operation

plot.ts(result.Vec)



# ======================== apply functions ==========================

lapply(X = ..., FUN = ...)
sapply(X = ..., FUN = ...)


# lapply takes two arguments: a list x, a function or a name of function.
# If x is not a list, it will be coerced to a list using as.list().
# lapply always returns a list, regardless of the class of the input.

?lapply

x <- 1:10
lapply(x, mySquare)

# sapply will simplify the result of lapply if possible.
sapply(x, mySquare)


# more examples
x <- list(rnorm(10000, mean = 0, sd = 1), runif(10000, min = 0, max = 1))
lapply(x, mean)
sapply(x, mean)


x <- 1:4
lapply(x, runif)


# sapply will give the same result
sapply(x, runif)




# use apply functions will dramaticly increase the performance
?system.time

x <- 1:10^5
system.time(    
    for (i in x)
    {
        mySquare(x)
    }
)

# user  system elapsed 
# 45.927   0.172  46.101 

system.time(
    sapply(x, mySquare)    
)

# user  system elapsed 
# 0.200   0.002   0.203


# ======================= Generating Random Variables =========================

?rnorm
# rnorm(n = , mean = , sd = )


x <- rnorm(n = 10000, mean = 0, sd = 1)
x
# computes a histogram
?hist
hist(x)
hist(x, nclass = 40)
hist(x, nclass = 40, main = "mu = 0, sigm = 1")

# another sigma
x <- rnorm(n = 10000, mean = 0, sd = 5)
x
hist(x, nclass = 40, main = "mu = 0, sigma = 5")


?set.seed

# Anywho the random numbers R gives you aren't really random. They're pseudo-random. 
# Basically there's a function that outputs numbers that look random. 
# To do this it needs some inputs. The first input it gets will be the 'seed'.


set.seed(1)
rnorm(5)
rnorm(5)

set.seed(1)
rnorm(5)



# other functions

# density function
dnorm(x = 0)    # mean = 0, sd = 1
dnorm(x = 1)    # check table if you want
for (x in seq(0, 1, by = 0.1))
{
    print(dnorm(x))
}



# cumulative distribution function
pnorm(q = 0)
pnorm(q = 5)
for (x in seq(-3, 3, by = 0.1))
{
    print(pnorm(x))
}


# other distributions
x <- rpois(1000, lambda = 2)
hist(x, nclass = 40)

x <- rexp(1000)
hist(x, nclass = 40)

x <- rt(1000, df = 10)
hist(x, nclass = 40)