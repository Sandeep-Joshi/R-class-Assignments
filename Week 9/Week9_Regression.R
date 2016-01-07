# ============================ Linear Regression ================================

data <- read.csv("grape crops.csv", header=T)
yield <- data$yield
cluster <- data$cluster.count
plot(yield ~ cluster)

lm(yield ~ cluster)

lm.r <- lm(yield ~ cluster)
abline(lm.r, col='red')

summary(lm.r)

#///////////////////////////////////////////////////////
Call:
  lm(formula = yield ~ cluster)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.60700 -0.19471 -0.03241  0.23220  0.64874 

# Residuals gives the difference between the experimental value and predicted value.
# The distance from data point to the regression line

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.02790    0.78355  -1.312    0.219    
cluster      0.05138    0.00725   7.087 3.35e-05 ***
  ---
  Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

Residual standard error: 0.3641 on 10 degrees of freedom
Multiple R-squared:  0.834,  Adjusted R-squared:  0.8174 
F-statistic: 50.23 on 1 and 10 DF,  p-value: 3.347e-05

# ////////////////////////////////////////////////////////////

# residuals
lm.r$residuals
sort(lm.r$residuals)
boxplot(lm.r$residuals)


# p-value, no intercept
newlm.r <- lm(yield ~ -1 + cluster)
summary(newlm.r)


plot(yield ~ cluster)
abline(lm.r, col = 'red')
abline(newlm.r, col = 'blue')



# (0, 0)
plot(yield ~ cluster, xlim = c(-10, 120), ylim = c(-5, 20))
abline(lm.r, col = 'red')
abline(newlm.r, col = 'blue')
abline(h = 0)
abline(v = 0)


# other functions:
coef(newlm.r)
resid(newlm.r)  # or
residuals(newlm.r)
fitted(newlm.r)

# ============================ Quadratic Model ================================

lm.q <- lm(yield ~ cluster + I(cluster^2))
lm.q
summary(lm.q)
fitted(lm.q)
data.frame(fitted(lm.q), yield)
plot(yield ~ cluster)
lines(sort(cluster), sort(fitted(lm.q)), type='l', col='red')




# ============================= Stock and ETF ================================

library(quantmod)
getSymbols("CSCO")
getSymbols("DIA")

csco <- data.frame(CSCO)
dia <- data.frame(DIA)

# get last price
csco.price <- csco$CSCO.Adjusted
dia.price <- dia$DIA.Adjusted


# get returns
csco.rtn <- diff(csco.price, lag = 1) / csco.price[-length(csco.price)]
dia.rtn <- diff(dia.price, lag = 1) / dia.price[-length(dia.price)]

plot(csco.rtn ~ dia.rtn)

lm1 <- lm(csco.rtn ~ dia.rtn)
abline(lm1, col = 'red')

summary(lm1)

lm2 <- lm(csco.rtn ~ -1 + dia.rtn)
abline(lm2, col = 'blue')
summary(lm2)
