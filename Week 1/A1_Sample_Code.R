rm(list=ls())   # remove all existing variables

?rnorm  # tells your how to use this function
?hist

xx <- rnorm(10000)  # generate 10000 standard normal random samples
hist(xx)            # draw a histogram for vector xx