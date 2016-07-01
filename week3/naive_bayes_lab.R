train <- data.frame(class=c("spam","ham","ham","ham"), 
                    viagra=c("yes","no","no","yes"))
train

library(e1071)
classifier <- naiveBayes(class ~ viagra,train)
classifier

test <- data.frame(viagra=c("yes"))
test$viagra <- factor(test$viagra, levels=c("no","yes"))
test

prediction <- predict(classifier, test ,type="raw")
prediction

train <- data.frame(type=c("spam","ham","ham","ham"), 
                    viagra=c("yes","no","no","yes"),
                    meet=c("yes","yes","yes", "no"))
train

classifier <- naiveBayes(type ~ viagra + meet,train)
classifier

test <- data.frame(viagra=c("yes"), meet=c("yes"))
test$viagra <- factor(test$viagra, levels=c("no","yes"))
test$meet <- factor(test$meet, levels=c("no","yes"))
test

prediction <- predict(classifier, test ,type="raw")
prediction

#######
## 1.
df <- data.frame(buy=c("yes","no","no","yes"), 
                    income=c("high","high","medium","low"))
df

## 3. 
classifier <- naiveBayes(buy ~ income, df)
classifier

## 4.

test <- data.frame(income=c("yes"))
test$income <- factor(test$income, levels=c("no","yes"))
test

prediction <- predict(classifier, test ,type="raw")
prediction

## 5. 
train <- data.frame(buy=c("yes","no","no","yes"), 
                    income=c("high","high","medium","low"),
                    gender=c("male","female","female", "male"))

## 7. 
classifier <- naiveBayes(buy ~ income + gender,train)
classifier

test <- data.frame (income=c("yes"), gender=c("yes"))
test$income <- factor(test$income, levels=c("no","yes"))
test$gender <- factor(test$gender, levels=c("no","yes"))
test

prediction <- predict(classifier, test ,type="raw")
prediction
