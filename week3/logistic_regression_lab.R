library(dplyr)
library(stargazer)
library(caret)
loan <- read.csv("https://www.dropbox.com/s/89g1yyhwpcqwjn9/lending_club_cleaned.csv?raw=1")

summary(loan)

logit1 <- glm(good ~ fico, data = loan, family = "binomial")
summary(logit1)

exp(coef(logit1))

test <- data.frame(fico=c(700,750))
test$pred <- predict(logit1,test, type="response")
test

logit2 <- glm(good ~ fico + loan_amnt, data = loan, family = "binomial")
summary(logit2)
exp(coef(logit2))

logit3 <- glm(good ~ fico + loan_amnt + purpose, data = loan, family = "binomial")
summary(logit3)
round(exp(coef(logit3)),3)

loan <- loan %>% group_by(purpose) %>% mutate(nobs=n()) 
loan$purpose <-  reorder(loan$purpose, -loan$nobs)
levels(loan$purpose)

logit4 <- glm(good ~ fico + loan_amnt + income + purpose, data = loan, family = "binomial")
summary(logit4)

stargazer(logit1,logit2, logit3, logit4, type="text")

set.seed(364)
sample <- sample(nrow(loan),floor(nrow(loan)*0.8))
train <- loan[sample,]
test <- loan[-sample,]

logit4 <- glm(good ~ fico + dti+ loan_amnt + purpose, data = train, family = "binomial")
test$pred <- predict(logit4, test, type="response")

test$good_pred <- ifelse(test$pred > 0.80, "good", "bad")
confusionMatrix(test$good_pred, test$good)

########################
## 1. Let’s load the Titanic training data. What are the odds of surviving the shipwreck?
install.packages("titanic")
library(titanic)
summary(Titanic)

## 2. Using the logit model, estimate how much lower are the odds of survival for men relative to women?
logit <- glm(Survived ~ Sex -1, data = df, family = "binomial")
summary(logit)
exp(coef(logit))

## 3. Controlling for gender, does age have a statistically significant effect on the odds of survival? If so, what is
## the magnitude of that effect?

## 4. Controlling for gender, does passenger class have a statistically significant effect on the odds of survival? 
## If so, what is the magnitude of that effect?

## 5. Controlling for gender, estimate the effect of being in the second class relative to first class, and the effect 
## of being in the third relative to first.

## 6. Add fare to the regression you estimated above. Is fare a significant determinant of survival controlling for
## gender and passenger class? Do you think that if we regressed survival on just gender and fare, fare would be 
## significant? Explain.

## 7. As we know from the movie, Jack traveled in the third class and paid 5 pounds (I know that Jack actually won the
## ticket in poker, but Swen, from whom Jack won the ticket, paid …). Rose traveled in the first class and paid 500 for
## her ticket (I know that her fiancee, Cal Hockley - Pittsburgh steel tycoon, actually bought the ticket, but …). 
## What is the probability that Jack will survive? What is the probability that Rose will survive?

## 8. Create your own logistic model and make predictions for passengers in the Titanic test data set. Keep in mind 
## that you must make predictions for all passengers in the test data (even those with missing values). Use your own
## probability cut off for predicting survival (0.5 is a natural start). Submit your predictions to kaggle, did you 
## do better with logistic regression than with decision trees? Which algorithm do you like better?