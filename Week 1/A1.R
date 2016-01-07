rm(list=ls())   # remove all existing variables

?rnorm  # tells your how to use this function
?hist

xx <- rnorm(10000)  # generate 10000 standard normal random samples
hist(xx)            # draw a histogram for vector xx1 <-list

?basicStats
??basicStats


# Three ways to create vector
way1 <- 1:3  # way 1
print(way1)
v1 <- seq(-10,10, by = 0.1)
print(v1)
v2 <- rep(c('0','1','2'),3)
print(v2)

# Explicit coercion
# Convert v2 to numeric
v2num <- as.numeric(v2)
print(v2num)
# Convert v2 to logical
v2NA <- as.logical(v2)
print(v2NA)
# Convert v2num to logical
v2logical <- as.logical(v2num)
print(v2logical)

# Matrix and List
# 3x3 matrix
m2 <- matrix(v2num, nrow = 3, byrow = T)
print(m2)
# Calculating inverse of m2
solve(m2)
# Doesn't work as [m2] yields 0, so changing few enteries
m2[1, 1] = 4
m2[3, 3] = 7
solve(m2)

# My First List
myFirstList <- list(char = v2, integer = v2num, NAs = v2NA, 
                    bool = v2logical, mat = m2)
# Subseting 4th element by indexing
myFirstList[4]
# Subseting 4th element by element name
myFirstList["bool"]



# Fibonacci 
# Wrong one as nth term <> (n-1)th term + (n-2)th term
a <- 0
b <- 1
MAX = 1000
while (TRUE)
{
    print (a)
    if (a > MAX)
        break
    a <- b
    b <- a + b
}

# Right one as given in the code but goes beyond MAX
a <- 0
b <- 1
MAX = 10000
while(TRUE)
{
    print (a)
    if(a > MAX)
        break
    tmp <- a
    a <- b
    b <- tmp + b
}

# learning repeat
?Repeat
# Fibonacci using repeat. Doesn't go be beyond MAX specified
MAX <- 10000
a <- 0
b <- 1
repeat
{
    if(a > MAX)
        break
    print (a)
    tmp <- a
    a <- b
    b <- tmp + b
}
    