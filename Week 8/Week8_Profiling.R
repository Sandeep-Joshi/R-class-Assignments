# first example: system.time()

system.time(
    x <- rnorm(100)
)
# user  system elapsed 
# 0       0       0


system.time(
    x <- rnorm(10000000)    # 10 million
)
# user  system elapsed 
# 0.930   0.030   0.959


# Elapsed time > user time
# Elapsed time may be GREATER than user time if the CPU spends a lot of time waiting around

system.time(readLines("http://finance.yahoo.com")) 
# user  system elapsed 
# 0.020   0.007   1.104





# Timing Longer Expressions

system.time({ 
    n <- 10000
    r <- numeric(n) 
    for(i in 1:n)
    {
        x <- rnorm(n)
        r[i] <- mean(x) 
    }
})
# user  system elapsed 
# 10.056   0.007  10.063 



testapply <- function(i)
{
    n <- 10000
    x <- rnorm(n)
    r[i] <- mean(x)
}

system.time({
    idx <- 1:10000
    r <- c()
    sapply(idx, testapply)    
})

# user  system elapsed 
# 5.159   0.010   5.169 


?sapply
r <- c()
sapply(r, testapply)




# ============================ R Profiler ==========================

# Rprof() and summaryRprof() approach
?Rprof()


# how to use the profiler?


Rprof("output file")
##############################
## some code to be profiled
##############################
Rprof(NULL)
##############################
## some code NOT to be profiled
##############################
Rprof("output file", append=TRUE)
##############################
## some code to be profiled
##############################
Rprof(NULL)

# summarize the results
summaryRprof("path_to_hold_output")




# Example 1
rm(list = ls())
source("SimStock.R")
fileName <- "Rprof_example.txt"
Rprof(fileName)
SimStock(maturity = 100, mu = 0.05, sigma = 0.6, steps = 25200, s_0 = 100)
Rprof(NULL)
summaryRprof(fileName)


# Example 2
install.packages("stockPortfolio")

library(stockPortfolio)
fileName <- "Rprof_example.txt"
Rprof(fileName)
gr <- getReturns(c("GOOG", "MSFT", "IBM"), freq="week")
Rprof(NULL)
summaryRprof(fileName)




