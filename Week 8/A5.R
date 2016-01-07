#------------------------------------#
#     A S S I G N M E N T   5        #
#------------------------------------#

# Clear console
cat("\014") 
# Set working directory
setwd("D:/Stevens/Sem 3/FE515/Week 8")  # Comment this line

# Read the csv files
data <- read.csv("zye2@stevens.edu-MS_N-N99265651.csv", stringsAsFactors = TRUE)

data[,'Time'] <- paste(data$Date.L., data$Time.L.)
data$Time <- as.POSIXlt(strptime(data$Time, "%Y%m%d %H:%M:%S"))

# Filter out the daily time window we need 11:00 AM and 2:30 PM
data <- subset(data, strptime(Time.L., "%H:%M:%S") >= strptime('11:00:00.000000', "%H:%M:%S") & strptime(Time.L., "%H:%M:%S") <= strptime('14:30:00.000000', "%H:%M:%S"))

# Calculate volatility for these windows
realized_vol <- function(ser){
    
    # realized variance
    realized_var <- sum((diff(log(ser)))^2)/(length(ser) -1)
    #print(length(ser)-1)
    #print(sum((diff((ser)))^2))
    # realized volatility
    return(sqrt(realized_var))
} 

library(quantmod)
#print (unique(data$Date.L.))
#level <- unique(data$Date.L.)
#fdata <- factor(data, levels = level, ordered = TRUE)
res1 <- tapply(data$Last, data$Date.L., realized_vol)
res2 <- setNames(aggregate(data$Last ~ data$Date.L., FUN = realized_vol), c("Day", "Volatility"))

# Plot the graph
library(ggplot2)
qplot(names(res1), res1)

head(res2)
ggplot(data=res2, aes(x=Day, y=Volatility)) + geom_line()
      
# Write to CSV
write.csv(res2, "Output.csv")
