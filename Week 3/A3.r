#------------------------------------#
#     A S S I G N M E N T   3        #
#------------------------------------#

cat("\014")

# Question 1
# Coin simulation
coin_toss <- function(tosses, consecutive_heads)
{
    tmp <- sample(x=c(0,1), size = tosses, replace = T, prob = c(.5,.5))
    count = 0
    pos = 0
    # cat(tmp)
    for (i in tmp)
    {
        pos = pos + 1
        if (i ==1 )
            count <- count + 1
        else
            count = 0
        if (count == consecutive_heads)
        {
            return(pos)
        }
        
    }
    return(0)
}

# plot for actual results as simulation
size = 10000
plot(x <- 1:size, y <- as.numeric(unlist(lapply(x, coin_toss, consecutive_heads =3))), type = "l")
print(paste('Expected tosses for', size, 'samples', sum(y)/size))

# Mathematically
# Let the expectation be x
# For three consecutive heads we need at least three tosses.

# Toss 1 if tail. We need atleast one more toss hence, multiply that with prob(T)
# (x + 1) * (1/2)  ---- (1)

# Toss 2 if tail i.e. head, tail, we need two more tosses, multiply that with prob(H)*prob(T)
# (x + 2) * (1/2) * (1/2)  ----(2)

# Toss 3 if tail i.e head, head, tail, three more tosses, and prob(H)*prob(H)*prob(T)
# (x + 3)*(1/2)*(1/2)*(1/2)  ----(3)

# if all heads i.e. head, head, head. no more tosses and prob prob(H)^3
# 3*(1/8)  ----(4)

# In all these equations the first part is the number of tosses it would take for 3 heads and
# second part is the net probablity of it happening.

# Adding (1), (2), (3) and (4) we would get x,
# x - (x + 1)*(1/2) - (x + 2)*(1/4) - (x + 3)*(1/8) - 3/8 = 0
# x - x*(7/8) - (7/4) = 0
# x = 7*8/4 = 14

# our answer for 10000 samples was 13.9154 which is ~ 14




# Question 2
setwd("D:/Stevens/Sem 3/FE515/Week 3/Assignment 3")  # Comment this line
# Remove NA values
df <- na.omit(read.csv("GOOGwna.csv", stringsAsFactors = F))
# save file
filename = 'out.csv'
write.csv(file = filename, x = df, row.names = FALSE)
# read the saved file 
res <- read.csv(filename)
print(length(colnames(res)))
# Calculate mean using apply
apply(res[2:length(colnames(res))],2, mean)



# Question 3
# Monty hall simulation
cat("\014")
MontyHallSim <- function(tries)
{
    stuck = 0
    for(i in 1:tries)
    {
        door = c(0, 0, 1)
        tmp <- sample(door, size = 3, replace = FALSE)
        #print(tmp)
        # contestant's choice
        choice <- sample(tmp, 1, replace = FALSE)
        print(choice)
        # remove one bad door and the choice contestant made and check last remaining door
        val <- match(choice, tmp)
        tmp <- tmp[-val]
        #print(tmp)
        # remove hosts' choice
        val2 <- match(0, tmp)
        tmp <- tmp[-val2]
        if(sum(tmp) == 0)
        {
            stuck <- stuck + 1
        }
    }
    
    # if user switched 
    print(paste('Win after switch:', (tries - stuck)/tries))
    # if user stuck to his choice
    print(paste('Win without switch:', stuck/tries))
}

MontyHallSim(1000)
