# ============= Example 1: Simple grandient descent with 1 variable ================

rm(list = ls())

# function f(x) = x^2 - 10x + 3
# x.min = -b/2a = 5

fx <- function(x)
{
    y = x^2 - 10 * x + 3
    return (y)
}

# derivative of f(x)
# df = x^2 - 10

df <- function(x)
{
    y = x*2 - 10
    return (y)
}

plot(fx, xlim = c(4, 6))

# pseudo code:

# set initial
# set convergence condition
# loop: 
while(1)
{
    # algorithm
    # ...
    # if (converge)
    # {
    #     break
    # }
}




# initial points (guess)
x0 = 6
points(x0, fx(x0), col = "red")

# step length
alpha = 0.2

# condition to terminate the algorithm
epsilon = 0.0001

# step count
step = 1

while(1)
{
    cat("Calculating, step ", step, '\n', sep = "")
    # update x
    x1 = x0 - alpha * df(x0)
    
    # check convergence
    if (abs(fx(x1) - fx(x0)) < epsilon)
    {
        # converged, output some information
        cat("x = ", x1, '\n', sep='')
        cat("Final step: ", step, '\n', sep='')
        break
    }
    
    points(x1, fx(x1), col = "red")
    
    # update x
    x0 = x1
    step = step + 1
#     Sys.sleep(1.2)
}


# ======================= Example 2: Local extrema ===================

rm(list = ls())

# f(x) = x * sin(x)

fx2 <- function(x)
{
    y = x * sin(x)
    return (y)
}


df2 <- function(x)
{
    y = sin(x) + x * cos(x)
    return (y)
}

plot(fx2, xlim = c(-20, 20), main = "f(x) = x*sin(x)")

plot(fx2, xlim = c(2, 13), main = "f(x) = x*sin(x)")

x0 = 7.5    # try 2, 6, 7, 8
points(x0, fx2(x0), col = "blue")

alpha = 0.1
epsilon = 0.0001
step = 1

while(1)
{
    x1 = x0 - alpha * df2(x0)
    if (abs(fx2(x1) - fx2(x0)) < epsilon)
    {
        cat("x = ", x1, '\n', sep='')
        cat("Final step: ", step, '\n', sep='')
        break;
    }
    points(x1, fx2(x1),  col = "red")
    x0 = x1
    step = step + 1
}



# ========================= Gradient Descent and Linear Regression =======================

rm(list = ls()) 
x0 <- c(1,1,1,1,1) # column of 1's
x1 <- c(1,2,3,4,5) # original x-values

x <- cbind(x0,x1)
y <- as.matrix(c(3,7,5,11,14))

plot(y ~ x1)

# number of elements
m <- nrow(y)

# define the gradient function dJ/dtheata: 1/m * (y^hat-y))*x where y^hat = x*beta
# in matrix form this is as follows:
grad <- function(x, y, beta) {
    gradient <- (1/m) * ( t(x) %*% ((x %*% t(beta)) - y) )
    return(t(gradient))
}

# define gradient descent update algorithm
grad.descent <- function(x, maxit){
    beta <- matrix(c(0, 0), nrow=1) # Initialize the parameters
    
    alpha = .05 # set learning rate
    for (i in 1:maxit) {
        beta <- beta - alpha  * grad(x, y, beta)   
    }
    return(beta)
}

# results 
grad.descent(x,1000)
beta <- grad.descent(x,1000)
abline(beta, col = 'red')

# analytical results with matrix algebra
# so-called normal equation
solve( t(x) %*% x ) %*% t(x) %*% y

# results using canned lm function match results above
m1 <- lm(y ~ x[, 2])
summary(m1)

# compare with theoretical value
coef(m1)







########### FYI ####################
# matrix munipulation

a <- matrix(1:9, 3, 3, byrow = T)
b <- matrix(1:12, 3, 4, byrow = T)
a; b

# transpose
t(a)
t(b)

# matrix multiplication
c <- a %*% b

# inverse
diag(a) <- c(3, 3, 3)
a
solve(a)

###############################
