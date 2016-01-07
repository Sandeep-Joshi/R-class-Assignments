## this file provides two functions of simulating the GBM

simOne_terminal_only <- function(S0, r, Maturity, steps, sigma)
{
    dt <- Maturity / steps
    epsilon_t_vec <- rnorm(steps)
    epsilon_t_vec <- append(0, epsilon_t_vec)
    cum_wt_vec <- cumsum(epsilon_t_vec) * sqrt(dt)
    ST <- S0 * exp(r*Maturity + cum_wt_vec[steps + 1] * sigma)
    return (ST)
}

simOne_keep_path <- function(S0, r, Maturity, steps, sigma)
{
    dt <- Maturity / steps
    epsilon_t_vec <- rnorm(steps)
    epsilon_t_vec <- append(0, epsilon_t_vec)
    dwt_vec <- epsilon_t_vec * sqrt(dt)
    St_vec <- c()
    St_vec[1] <- S0
    for(i in 1:steps)
    {
        dwt <- dwt_vec[i+1]
        St_vec[i+1] <- St_vec[i] + r * St_vec[i] * dt + sigma * St_vec[i] * dwt
    }
    return(St_vec)
}


## Monte Carlo Simulation on GBM

GBM_MC <- function(S0, r, Maturity, steps, sigma, npaths, keep_path)
{
    if(keep_path)
    {
        ST_mat <- matrix(0, nrow = npaths, ncol = steps + 1)
        for(i in 1:npaths)
        {
            ST_mat[i, ] <- simOne_keep_path(S0, r, Maturity, steps, sigma)
        }
        return(ST_mat)
    } else {
        ST_vec <- c()
        for(i in 1:npaths)
        {
            ST_vec[i] <- simOne_terminal_only(S0, r, Maturity, steps, sigma)
        }
        return(ST_vec)
    }
}