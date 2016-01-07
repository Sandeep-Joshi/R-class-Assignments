# ====================== Appetizer ========================

# source
?source
# read R code from a file

# ====================== The first debugging tool: breakpoints ========================
rm(list = ls())
source("foobar.R")
bar()

# set breakpoints by clicking and or press Shift + F9
debugSource("foobar.R")
bar()



# 3 debugging commands:

# n    Execute Next Line                  F10
# c    Continue to the next break point   Shift + F5
# Q    Stop Debugging                     Shift + F8



debugSource("foobar.R")
foo()
bar()

# An official explanation to this issue from RStudio team:
# https://support.rstudio.com/hc/communities/public/questions/200506688-Breakpoints-not-hitting-in-code-loaded-via-source-function


# Example
# hourly average
rm(list = ls())

source("hourlyAverage.R")
hourlyAverage(1:100)

debugSource("hourlyAverage.R")
hourlyAverage(1:100)

# ====================== The second debugging tool: browser() ========================

rm(list = ls())
source("foobarBrowser.R")
bar()



# conditional break points using browser():

if (something)
{
    browser()
}


fooConditional()





# Example 1 + 2 + ... + 100 = 5050
mySum <- function()
{
    browser()
    result <- 0
    for (i in 1:100)
    {
        result = result + i
        if (result > 10 && result < 20)
        {
            browser()
        }
    }
    
    
    return(result)  
}


mySum()



# ====================== The third debugging tool, debug() ========================
# debug()
# debugonce()
# undebug()
# isdebugged(fun)

?debug

# Calling debug(f) places a call to browser() at the beginning of f()


rm(list = ls())
source("foobar.R")

# will set an debugging flag on this function
debug(foo)

foo()
isdebugged(foo)
# True



undebug(foo)        # will remove the debugging flag
isdebugged(foo)
foo()





debugonce(foo)

foo()

foo()   # only debug once






# there is a fourth tool setBreakpoint
?setBreakpoint


# ====================== Full debug session: find runs ========================
rm(list = ls())

?all()

trueVec <- rep(T, 10)
all(trueVec)
vec1 <- rep(T, 5)
all(vec1)
# TRUE
all( c(vec1, FALSE) )
# FALSE


rm(list = ls())
# here is a buggy function
source("findRuns.R")

xVec <- c(1, 0, 0, 1, 1, 0, 1, 1, 1)

findruns(xVec, 2)
# [1] 3 4 6 7
# the result should be 4, 7, 8
# where is the bug?


findruns(xVec, 3)
# [1] 2 3 5 6
# should be 7
# ...?



debug(findruns)
findruns(xVec, 2)
undebug(findruns)

