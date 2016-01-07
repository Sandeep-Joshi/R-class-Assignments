#-------------------------------------#
#     G G P L O T 2    D E M O        #
#-------------------------------------#
#               [Sandeep Joshi]       #
#-------------------------------------#

# Clear console
cat("\014") 

library(ggplot2)

fc <- c("Manchester United", "Arsenal", "Manchester City", "Chelsea")
#  box plot and stacked bar chart for your part of the project.

# Create random samples of data for Votes and age of voter
people <- data.frame(Age = sample(c(15:45), 1000, replace = TRUE), Vote = sample(fc, 1000, replace = TRUE, prob = c(5, 3, 3, 1)))

# Add a column for age group as factors
people$bin <- cut(people$Age, seq(10, 50, 10), labels  = paste("Group", 1:4))
print(people)
# Voting summary
print(table(people$Vote))

# Summarize age wise votes across votes
vote_Share <- table(findInterval(people$Age,seq(10, max(people$Age), 10)))
# print(vote_Share)

# H I S T O G R A M S
# Assigning age groups to the data
windows(5, 5, xpos = 100, ypos = 10)
plot1 <- ggplot(people, aes(Age))
plot1 <- plot1 + ggtitle("Number of persons who participated vs Age (Histogram)")
plot1 + geom_histogram(binwidth = .5)

# Showing their choices using Facets
windows(5, 5, xpos = 700, ypos = 10)
plot2 <- ggplot(people, aes(Age))
plot2 <- plot2 + ggtitle("Number of single club fans who participated vs Age (Histogram)")
plot2 <- plot2 + ylab("Number of fans")
plot2 <- plot2 + geom_histogram(colour = "red", fill = "white", binwidth = 0.5) 
plot2 + facet_wrap(~ Vote, nrow = 2, ncol = 2)

# Showing their choice in the age-groups
windows(5, 5, xpos = 100, ypos = 500)
plot3 <- ggplot(people, aes(x = bin, fill = Vote))
plot3 <- plot3 + ggtitle("Number of person in Age groups")
plot3 <- plot3 + xlab("Age groups") + ylab("Number of fans")
plot3 + geom_bar(colour = "black", position = position_dodge()) + scale_fill_manual(values=c("yellow", "blue", "lightblue", "red"))

# Stacked bar charts
# install.packages("mapproj")
windows(5, 5, xpos = 700, ypos = 500)
plot3 <- plot3 + geom_bar(colour = "black", position = "stack") + ggtitle("Number of persons in Age groups (Stacked)")
plot3 <- plot3 + coord_flip()
plot3 <- plot3 + xlab("Age groups") + ylab("Number of fans") 
plot3 + scale_fill_manual(values=c("yellow", "blue", "lightblue", "red")) # Setting legend colours to their famous jersies


# P I E   C H A R T S

# First club
windows(5, 5, xpos = 100, ypos = 10)
pie1 <- ggplot(data = subset(people, Vote==fc[1]), aes(x = factor(1), stat = "bin", fill=factor(bin))) + geom_bar(width = 1) 
pie1 <- pie1 + coord_polar(theta = "y") + ggtitle(paste("Age group distribution for ", fc[1], " fans")) 
pie1 <- pie1 + xlab("") 
pie1 <- pie1 + ylab("O°-> number of fans")
pie1 + scale_fill_discrete(name = "Age Groups")

# Second club
windows(5, 5, xpos = 700, ypos = 10)
pie2 <- ggplot(data = subset(people, Vote==fc[2]), aes(x = factor(1), stat = "bin", fill=factor(bin))) + geom_bar(width = 1) 
pie2 <- pie2 + coord_polar() + labs(title = paste("Age group distribution for ", fc[2], " fans"), x = "", y = "Number of fans") 
pie2 + scale_fill_discrete(name = "Age Groups")
   

# Third club
windows(5, 5, xpos = 100, ypos = 500)
pie3 <- ggplot(data = subset(people, Vote==fc[3]), aes(x = factor(bin))) + geom_bar(width = 1, colour = "lightblue") 
pie3 + coord_polar(theta = "y") + labs(title = paste("Age group distribution for ", fc[3], " fans"), x = "Age", y = "O°-> number of fans")

# Inversion of axis
windows(5, 5, xpos = 100, ypos = 500)
pie3 <- ggplot(data = subset(people, Vote==fc[3]), aes(x = factor(bin))) + geom_bar(width = 1, colour = "lightblue") 
pie3 + coord_polar(theta = "x", direction = 1) + labs(title = paste("Age group distribution for ", fc[3], " fans"), x = "O°-> Age groups", y = "Radius -> number of fans")

# print(table(subset(people, Vote==fc[3])))

# Fourth club
windows(5, 5, xpos = 700, ypos = 500)
pie4 <- ggplot(data = subset(people, Vote==fc[1]), aes(x = Age, fill=factor(bin))) + geom_bar(binwidth = 1) 
pie4 <- pie4 + coord_polar(theta = "y") + ggtitle(paste("Age distribution for ", fc[4], " fans"))
pie4 <- pie4 + ylab("O°-> number of fans")
pie4 + scale_fill_discrete(name = "Age Groups")

# V I O L I N   P L O T
windows(50, 50, xpos = 100, ypos = 10)
box1 <- ggplot(data = people, aes(Vote, Age, fill = Vote)) + geom_violin() + geom_jitter()
box1 + ggtitle(paste("Violin plot... when simple box plot just wouldn't do"))

