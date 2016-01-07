##GGPLOT PROJECT##
rm(list = ls())
library(ggplot2) 


##LINE GRAPH WITH SMOOTHING##
real_vol <- read.csv("REAL_VOL.csv")


results = data.frame(real_vol$X, real_vol$x)

myPlot <- ggplot(results) + aes(real_vol$X, real_vol$x, group=1) + geom_point(shape=1) +geom_smooth() + geom_line() + xlab("Time") + ylab("Realized Volitility") + ggtitle("Realized Volitility VS. Time")
myPlot

#BASIC BELL CURVE
ggplot() + stat_function(aes(x = -3:3),
                         fun = dnorm, n = 101, args = list(sd=0.5))
#CDF Curve
ggplot() + stat_qq(aes(sample=1:100), distribution = qt,
                   dparams = list(df=5))

help(dev.cur)


# create factors with value labels 

mtcars$hp <- cut(as.numeric(mtcars$hp), 3, labels=c("low","medium","high")) 

mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual")) 
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl")) 

# Kernel density plots for mpg
# grouped by number of gears (indicated by color)
qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5), 
      main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
      ylab="Density")

# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
qplot(hp, mpg, data=mtcars, shape=am, color=am, 
      facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon") 

# Separate regressions of mpg on weight for each number of cylinders
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), 
      method="lm", formula=y~x, color=cyl, 
      main="Regression of MPG on Weight", 
      xlab="Weight", ylab="Miles per Gallon")

# Boxplots of mpg by number of gears 
# observations (points) are overlayed and jittered
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"), 
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")