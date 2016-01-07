hourlyAverage <- function(tempVec)
{
    # calculate the average of every four elements
    hourlyVec <- c()
    for(i in 1:length(tempVec))
    {
        if (i %% 4 == 0)
        {
            tmpAve <- mean(tempVec[(i-3):i])
            hourlyVec[floor(i/4)] <- tmpAve
        }
    }
    return(hourlyVec)
}