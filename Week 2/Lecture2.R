# Lecture 2: Functions
# Agenda : 1. Import Data from CSV
#          2. Functions
#          3. R Coding Style




# ======================================= 1 Import CSV =======================================


read.csv(file=, header=, ...)


read.csv("GOOG.csv")
GOOG <- read.csv("GOOG.csv")

# GOOG is a list
mode(GOOG)

head(GOOG)
names(GOOG)


GOOG$Adj.Close





# =================================== Example: Coin Flips ======================
# Imagine that you are flipping a fair coin, up to 1000 times. 
# Print out the proporation of head after every flip.


# Analysis: 
# We could keep generating a logical variable, using 1 for heads and 0 for tails.
# We also need a vector, which has 1000 elements, recording how many heads we got after each iteration.
# Another vector can be used for recording 1000 probabilities.
# Functions will be used: \textit{sample(), plot()}.


# plot()
plot(x=, y=, )

plot(x=1:10, y=1:10)

x <- seq(-2, 2, by = 0.0001)
plot(sin(x), type="l")


# other parameters in plot()

plot(1:1000, result.Vec, type="l", main="Coin Flips")

plot(1:1000, result.Vec, type="l", main="Coin Flips", ylim = c(0, 1))

plot(1:1000, result.Vec, type="l", main="Coin Flips", ylim = c(0, 1), 
     xlab = "Number of flips", ylab = "Probability")

# ...




# sample()
?sample
sample(x=, size=, replace=, prob=)
# sample with replacement and sample without replacement

sample(c(0,1), 1)


sample(x = c(1, 0), size = 1, replace = T, prob = c(0.5, 0.5))

# you can also use other functions, such as runif() + round()


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


# Question: what if the coin is not fair?




# =========================== 2. User Defined functions ========================

foo <- function(# parameters)   
{
    # body
}


# keyword return
foo <- function()
{
    # body
    # a function without return
}

foo <- function()
{
    # body
    return ()
}


# Example 1:
PrintHW <- function()
{
    "Hello World!"
}
PrintHW()


PrintSomething <- function(sth)
{
    print(sth)
}
PrintSomething("HW!")
PrintSomething(1)




PrintSomething <- function(sth)
{
    flag = is.character(sth)
    if (!flag)
    {
        stop("Error: Parameter is not character!")  # a function to generate error message
    }
    print(sth)
}


PrintSomething("HW!")
PrintSomething(100)



# Example 2:
add <- function(a, b)
{
    c <- a + b
    return (c)
}
add(1, 2)


result <- add(1, 2)
result


# Go back to the coin...
# Question: what if the coin is not fair?

No.heads <- 0
result.Vec <- NULL
for (flips in 1:1000)
{
    tmp <- sample(x=c(1, 0), size=1, replace=T, prob=c(0.2, 0.8))
    No.heads <- No.heads + tmp
    result.Vec <- c(result.Vec,No.heads/flips)
}
plot(1:1000, result.Vec, type="l")
plot(1:1000, result.Vec, type="l", ylim = c(0, 1))




# what if we have many unfair coin?

coinFlip <- function(headProb) {
    No.heads <- 0
    result.Vec <- NULL
    for (flips in 1:1000) 
    {
        tmp <- sample(x=c(1, 0), size=1, replace=T, 
                      prob=c(headProb, 1-headProb)) # passing parameter to this vector
        
        No.heads <- No.heads + tmp
        result.Vec <- c(result.Vec, No.heads/flips)
    }
    plot(1:1000, result.Vec, type="l")
}

coinFlip(0.5)
coinFlip(0.7)
coinFlip(0.9)
coinFlip(1)


# =========================== 3 Coding Style ===============================

# Use meaningful names on variables, functions as well as files.
# The maximum line length is 80 characters.
# At least 4 spaces for indentation.
# Place space arount all binary operators, such as '+', '-', '<-'
# Always place a space after a comma.
# Sufficient comments.