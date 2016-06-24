# 6-22-16

library(readr)
library(ggplot2)

oj <- read_csv('oj.csv', na='\\N')
oj <- mutate(oj, logprice = log(price))

########################################
# 1. Let’s return to the orange juice assignment and investigate how store demographics are related to demand.
########################################
# i. Let’s start with the following model: logmove ~ log(price)*brand*feat and add in the store demographics 
# as linear features (e.g., + AGE60 + EDUC + ETHNIC + INCOME). Try them individually and then all together.

lm.fit <- lm(logmove~logprice*brand*feat, oj)
summary(lm.fit) # Multiple R-squared:  0.5354,	Adjusted R-squared:  0.5352

lm.fit_age60 <- lm(logmove~logprice*brand*feat + AGE60, oj)
summary(lm.fit_age60) # Multiple R-squared:  0.5488,	Adjusted R-squared:  0.5486  t=29.354 

lm.fit_educ <- lm(logmove~logprice*brand*feat + EDUC, oj)
summary(lm.fit_educ)  # Multiple R-squared:  0.5357,	Adjusted R-squared:  0.5355 t=4.460

lm.fit_ethnic <- lm(logmove~logprice*brand*feat + ETHNIC, oj)
summary(lm.fit_ethnic)  # Multiple R-squared:  0.5417,	Adjusted R-squared:  0.5415 t=19.873

lm.fit_income <- lm(logmove~logprice*brand*feat + INCOME, oj)
summary(lm.fit_income)  # Multiple R-squared:  0.5389,	Adjusted R-squared:  0.5388  t=-14.921

lm.fit_multiple <- lm(logmove~logprice*brand*feat + AGE60 + EDUC + ETHNIC + INCOME, oj)
summary(lm.fit_multiple)  # Multiple R-squared:  0.5681,	Adjusted R-squared:  0.5678 

# ii. What demographics are significantly (t > 2 standard deviations) related to demand?

# iii. How much did the adjusted R-squared improve with the addition of these variables?
# adjusted r-squared increased from 0.5352 to 0.5678 with the addition of the linear features.

########################################
# 2. Let’s focus on two variables HHLARGE ("fraction of households that are large") and EDUC ("fraction of shoppers
# with advanced education").
########################################
# i. What are the means and percentiles of each of these variables?

summary(oj$HHLARGE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.01351 0.09794 0.11120 0.11560 0.13520 0.21640 

summary(oj$EDUC)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.04955 0.14600 0.22940 0.22520 0.28440 0.52840 

# ii. Using your coefficient estimates from the regression in 1b:

# a) If we move from the median value of HHLARGE to the 75th percentile (3rd quartile), how much does logmove 
# change each week on average? You can estimate this visually if you plot the fitted model, or you can compare 
# the predicted values for rows that have the median and 75th percentiles for HHLARGE.

lm.fit_hhlarge <- lm(logmove~logprice*brand*feat + HHLARGE, oj)
summary(lm.fit_hhlarge)
df <- mutate(oj, predicted = fitted(lm.fit_hhlarge))
ggplot(df, aes(x=price, y=exp(1)^logmove, color=brand, shape=as.factor(feat))) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=price, y=exp(1)^predicted, color=brand, linetype=as.factor(feat))) +
  facet_wrap(~ brand) +
  scale_x_log10(breaks=c(1,2,3)) +
  scale_y_log10(breaks=c(1e3,1e4,1e5))

# b) If we move from the median value of EDUC to the 75th percentile (3rd quartile), how much does logmove change 
# each week on average?

lm.fit_educ <- lm(logmove~logprice*brand*feat + EDUC, oj)
summary(lm.fit_educ)
df <- mutate(oj, predicted = fitted(lm.fit_educ))
ggplot(df, aes(x=price, y=exp(1)^logmove, color=brand, shape=as.factor(feat))) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=price, y=exp(1)^predicted, color=brand, linetype=as.factor(feat))) +
  facet_wrap(feat ~ brand) +
  scale_x_log10(breaks=c(1,2,3)) +
  scale_y_log10(breaks=c(1e3,1e4,1e5))

# c) Based on this analysis, which is the more important predictor of demand?
df <- mutate(oj, predicted_educ = fitted(lm.fit_educ), predicted_hhlarge = fitted(lm.fit_hhlarge))
ggplot(df, aes(x=price, y=exp(1)^logmove, shape=as.factor(feat))) +
#  geom_point(alpha=0.1) +
  geom_line(aes(x=price, y=exp(1)^predicted_educ, linetype=as.factor(feat), color="red")) + 
  geom_line(aes(x=price, y=exp(1)^predicted_hhlarge, linetype=as.factor(feat), color="black")) +
  facet_wrap( ~ brand) +
  scale_x_log10(breaks=c(1,2,3)) +
  scale_y_log10(breaks=c(1e3,1e4,1e5))


# iii. Now let’s see if these variables impact price sensitivity. Add two interaction terms (with logprice) to the
# model to test this.

# a) What are the coefficients on the interaction terms?

# b) Recall, positive values indicate lower price sensitivity and negative values indicate greater price sensitivity. 
# Do your estimates make sense based on your intuition?

# c) What are the coefficient estimates on the constants EDUC and HHLARGE? How do they compare to your regression from 1b?

# d) Similar to 2b, if we move from the median value of each variable to the 3rd quartile, how much does elasticity 
# change? Based on this, which is more important to price sensitivity?

# iv. You should notice that the coefficients on EDUC and HHLARGE have flipped sign once we include interaction terms
# with price. HHLARGE now appears to be a positive demand shifter and increases price sensitivity. Explain in words or pictures what is going on.

########################################
# 3. Let’s split our data into a training set and a test set. An easy way to do this is with the sample command. The 
# following will randomly select 20% of the rows in our data frame: indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
########################################
# i. Now let’s use this index to create a training and a test set, try: OJtest=oj[index, ] and Ojtrain=oj[-index, ]. 
# What did this do? How many rows does the test set have? How many rows does the training set have?

indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
OJtest=oj[indexes, ]
OJtrain=oj[-indexes, ]
head(OJtest)
head(OJtrain)
nrow(OJtest)
nrow(OJtrain)

# This generated two dataframes for test and train. OJtest has 5789 rows, and OJtrain has 23158 rows.

########################################
# 4. Now let’s run the very simple model logmove ~ log(price) + brand on the training data.
########################################
# i. Use LM on this model and report the R-squared.
OJtrain <- mutate(logprice = log(price), OJtrain)
model <- lm(logmove~logprice + brand, OJtrain)

# ii. Use predict(model, Ojtest) to predict log sales for the test set.
predicted_sales <- predict(model, OJtest)

# iii. Compute cor(predicted_sales,logmove)^2 on the test set. This is our "honest R-squared". How does it compare 
# to the value in (a)?
cor(predicted_sales, OJtest$logmove)^2
# 0.41008

########################################
# 5. Now let’s run better models.
########################################
# i. Run our "previous favorite" logmove ~ brand*log(price)*feat on the training data. Use LM to get regular R-squared.
# Now, follow the procedure in (3) to compute "honest R-squared". What is it? How do they compare?

model <- lm(logmove~brand*logprice*feat, OJtrain)
summary(model)
# Multiple R-squared:  0.5319,	Adjusted R-squared:  0.5317 

predicted_sales <- predict(model, OJtest)
cor(predicted_sales, OJtest$logmove)^2
# 0.5487649

# The "honest R-squared" is higher.

# ii. Now add in all the demographics. What is the regular R-squared on training data? What is the honest R-squared 
# on the test set?
model <- lm(logmove~brand*logprice*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, OJtrain)
summary(model)
# Multiple R-squared:  0.5837,	Adjusted R-squared:  0.5833 
predicted_sales <- predict(model, OJtest)
cor(predicted_sales, OJtest$logmove)^2
# 0.5898572
