foo <- function()
{
    browser()
    
    x <- 1
    y <- 2
    z <- 3
    
    return(c(x, y, z))
}

bar <- function()
{
    browser()
    foo()
}






# =====================================================




fooConditional <- function()
{
    x <- rnorm(1)
    if (abs(x) < 10)
        browser()
    
    y <- x * 2
    print(x)
    print(y)
    return(x + y)
}