SimStock <- function(maturity, mu, sigma, steps, s_0)
{
    result <- s_0
    dt <- maturity/steps
    for (i in 2:steps)
    {
        result[i] <- result[i-1] + result[i-1] * mu * dt + result[i-1] * sigma * rnorm(1) * sqrt(dt)
    }
    result
}