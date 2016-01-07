# ================== Newton's Method ==============================

# simple example, newton's methodx
# f(x) = x^2 - 2

fx <- function(x)
{
    y = x^2 - 2
    return (y)
}

# f'(x) = 2*x
dfx <- function(x)
{
    y = 2*x
    return(y)
}

plot(dfx)

# initial plot
plot(fx, ylim = c(-3, 60), xlim = c(-8, 8))
abline(h=0, v=0)

x0 = 8          # initial guess
epsilon = 0.01  # convergent condition
points(x0, fx(x0), col = "red")

step = 1        # step count

while(1)
{
    tmp = x0
    Sys.sleep(1)
    
    deltaX = fx(x0) / dfx(x0)
    x0 = x0 - deltaX
    points(x0, fx(x0), col = "red")    
    step = step + 1
    
    if (abs(x0-tmp) < epsilon)
    {
        points(x0, fx(x0), col = "red")
        Sys.sleep(1)
        print("Converged!")
        print(paste("x0 = ", x0, ", step = ", step, sep=""))
        break
    }
}


# ==================== calculate the bond yield ======================
rm(list = ls())

# a bond with coupon 3
# paid every 6 months
# face value 100
# bond price 98.39
# we want to calculate the yield

# 3 * exp(-0.5y) + 
# 3 * exp(-0.5y) + 
# 3 * exp(-0.5y) + 
# 103 * exp(-2y) = 98.39

bond <- function(y)
{
    value <- 3 * exp(-0.5 * y) + 
        3 * exp(-1 * y) + 
        3 * exp(-1.5 * y) + 
        103 * exp(-2 * y) - 98.39
    return(value)
}

dbond <- function(y)
{
    value <- -1.5 * exp(-0.5 * y) + 
        -3 * exp(-1 * y) + 
        -4.5 * exp(-1.5 * y) + 
        -206 * exp(-2 * y)
    return(value)
}

y0 = 0.02
while(1)
{
    y1 <- y0 - bond(y0) / dbond(y0)
    if(abs(y0-y1) < 1e-5)
    {
        print("converged!")
        cat("y = ", y1, sep = "")
        break
    }
    y0 <- y1
}