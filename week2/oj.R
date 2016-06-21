# 6-21-2016
########################################
# 1. Load the orange juice data.
########################################
library(readr)
library(ggplot2)

oj <- read_csv('oj.csv', na='\\N')

########################################
# 2. Visualizing price.
########################################
########################################
# i. Make a plot of the distribution of prices.
########################################

ggplot(oj, aes(x = price)) + geom_histogram()

########################################
# ii. Change the x-axis on this plot to use a logarithmic scale using scale_x_log10().
########################################

ggplot(oj, aes(x = price)) + geom_histogram() + scale_x_log10()

########################################
# iii. Repeat i), faceted by brand.
########################################

ggplot(oj, aes(x = price)) + geom_histogram() + facet_wrap(~ brand)

########################################
# iv. Repeat ii), faceted by brand.
########################################

ggplot(oj, aes(x = price)) + geom_histogram() + scale_x_log10() + facet_wrap(~ brand)

########################################
# v. What do these graphs tell you about the variation in price? Why do the log plots look different? 
# Do you find them more/less informative?
########################################

# Prices tend to stay predominantly stay at one price for dominicks and minute maid, tropicana demonstrates more
# variation. 

########################################
# 3. Visualizing the quantity/price relationship.
########################################
########################################
# i. Plot logmove (the log of quantity sold) vs. log price.
########################################

ggplot(oj, aes(x = price, y = logmove)) + scale_x_log10() + geom_point()

########################################
# ii. Color each point by brand. What do insights can you derive that were not apparent before?
########################################

ggplot(oj, aes(x = price, y = logmove, color = brand)) + scale_x_log10() + geom_point()
# The show that by brand, dominicks is the cheapest and moves the most product, tropicana is most expensive 
# and moves less, and minute maid is in the middle (lol).

########################################
# 4. Estimating the relationship.
########################################
########################################
# i. Do a regression of logmove on log price. How well does the model fit? 
# What is the elasticity (the coefficient on log price), and does it make sense?
########################################
library(dplyr)
oj <- mutate(oj, logprice = log10(price))


lm.fit=lm(logprice~logmove, oj)
summary(lm.fit)
coefficients(lm.fit)
# (Intercept)     logmove 
# 0.85805267 -0.05645038  

par(mfrow=c(2,2))
plot(lm.fit)

########################################
# ii. Now add in an intercept term for each brand (by adding brand to the regression formula). 
# How do the results change? How should we interpret these coefficients?
########################################

lm.fit=lm(logprice~logmove+brand, oj)
summary(lm.fit)
coefficients(lm.fit)
# (Intercept)          logmove brandminute.maid   brandtropicana 
# 0.72776290      -0.05437741       0.11683916       0.21701577 

par(mfrow=c(2,2))
plot(lm.fit)

########################################
# iii. Now add interaction terms to allow the elasticities to differ by brand, 
# by including a brand:log price term in the regression formula. 
# Note the estimate coefficients will "offset" the base estimates. 
# What is the insights we get from this regression? What is the elasticity for each firm? 
# Do the elasticities make sense?
########################################
lm.fit=lm(brand:logprice~logmove, oj)
summary(lm.fit)
coefficients(lm.fit)
# (Intercept)          logmove brandminute.maid   brandtropicana 
# 0.72776290      -0.05437741       0.11683916       0.21701577 

par(mfrow=c(2,2))
plot(lm.fit)


########################################
# 5. Impact of "featuring in store".
########################################
########################################
# i. Which brand is featured the most? Make a plot to show this.
########################################

df <- filter(oj, feat == 1)
ggplot(df, aes(x = brand)) + geom_bar()
# minute maid has the most features

########################################
# ii. How should we incorporate the "featured in store" variable into our regression? 
# Start with an additive formulation (e.g. feature impacts sales, but not through price).
########################################



########################################
# iii. Now run a model where features can impact sales and price sensitivity.
########################################



########################################
# iv. Now run a model where each brand can have a different impact of being featured and a different impact on price sensitivity. Produce a table of elasticties for each brand, one row for "featured" and one row for "not featured" (you need 6 estimates).
########################################


