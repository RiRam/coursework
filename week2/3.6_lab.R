# 3.6.1 Libraries
library (MASS)
library (ISLR)

# 3.6.2 Simple Linear Regression
fix(Boston)
names(Boston)

lm.fit=lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)

lm.fit
# Call:
#   lm(formula = medv ~ lstat)
# 
# Coefficients:
#   (Intercept)        lstat  
# 34.55        -0.95  

summary(lm.fit)
# Call:
#   lm(formula = medv ~ lstat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -15.168  -3.990  -1.318   2.034  24.500 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 34.55384    0.56263   61.41   <2e-16 ***
#   lstat       -0.95005    0.03873  -24.53   <2e-16 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.216 on 504 degrees of freedom
# Multiple R-squared:  0.5441,	Adjusted R-squared:  0.5432 
# F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

names(lm.fit)
coef(lm.fit)  # coefficients
confint(lm.fit) # confidence interval for coefficient estimates
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="confidence")  # used to produce confidence intervals and prediction intervals given lstat
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="prediction")

plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd=3) # line width
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")  # pch = symbol
plot(1:20, 1:20, pch=1:20)  # very cool

par(mfrow=c(2,2)) # tells R to split the display screen into separate panels to view multiple plots
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))  # calculate residuals
plot(predict(lm.fit), rstudent(lm.fit))   # studentized residuals

plot(hatvalues(lm.fit))   # compute leverage statistics
which.max(hatvalues(lm.fit))  # identifies index of largest element in a vector

# 3.6.3 Multiple Linear Regression
lm.fit=lm(medv~lstat+age, data=Boston)  # fit model with multiple predictors
summary(lm.fit)
# Call:
#   lm(formula = medv ~ lstat + age, data = Boston)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -15.981  -3.978  -1.283   1.968  23.158 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 33.22276    0.73085  45.458  < 2e-16 ***
#   lstat       -1.03207    0.04819 -21.416  < 2e-16 ***
#   age          0.03454    0.01223   2.826  0.00491 ** 
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.173 on 503 degrees of freedom
# Multiple R-squared:  0.5513,	Adjusted R-squared:  0.5495 
# F-statistic:   309 on 2 and 503 DF,  p-value: < 2.2e-16

lm.fit=lm(medv~., data=Boston) # regression using all the predictors
summary(lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

library(car)
vif(lm.fit) # part of car package, used to compute variance inflation factors

lm.fit1=lm(medv~.-age, data=Boston) # regression using all predictors except age
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age) # more or less same thing

# 3.6.4 Interaction Terms
summary(lm(medv~lstat:black, data=Boston))  # include an interaction term between lstat and black
summary(lm(medv~lstat*age, data=Boston)) # lstat, age, lstat x age (interaction term)

# 3.6.5 Non-linear Transformations of the Predictors
lm.fit2=lm(medv~lstat+I(lstat^2)) # given a predictor X, can create predictor X^2 using I(X^2) 
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit, lm.fit2)  # compute analysis of variance table, performs hypothesis test comparing 2 models
                        # null H: two models fit the data equally well
                        # alt H: the full model is superior

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat,5))  # poly of value, order
summary(lm.fit5)

summary(lm(medv~log(rm), data=Boston))  # log transformation

# 3.6.6 Qualitative Predictors
fix(Carseats)
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)  # returns coding for dummy variables
                      # R created ShelveLocGood - 1 if good, 0 otherwise
                      # R created ShelveLocMedium - 1 if medium, 0 otherwise
