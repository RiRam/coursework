library(dplyr)
library(lubridate)
library(ggplot2)

################
## prep

load('trips.RData')
trips_with_weather <- mutate(trips_with_weather, day = wday(ymd, T, F))

trips_with_weather$is_weekend <- ifelse(trips_with_weather$day == "Saturday" | trips_with_weather$day == "Sunday", TRUE, FALSE)

holidays <- c("2014-01-01", "2014-01-20", "2014-02-17", "2014-04-20", "2014-05-11", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-24", "2014-12-25", "2014-12-31")
holidays <- as.Date(holidays)
trips_with_weather <- mutate(trips_with_weather, is_holiday = ymd %in% holidays)

trips_with_weather <- mutate(trips_with_weather, is_heavy_rain = prcp > 2)
trips_with_weather <- mutate(trips_with_weather, is_sig_snow = snow > 1)
trips_with_weather <- mutate(trips_with_weather, tavg = (tmax+tmin)/2)
trips_with_weather <- mutate(trips_with_weather, is_extreme_weather = tavg > 8.5 | tavg < 3)

df <- group_by(trips_with_weather, is_weekend, ymd, tmax, tmin, snwd, prcp, snow, is_holiday, is_heavy_rain, is_sig_snow, tavg, is_extreme_weather) %>% summarize(num_trips = n())

###################


# The point of this exercise is to get experience in an open-ended prediction exercise: predicting the total number of 
# Citibike trips taken on a given day. 


indexes <- sample(1:nrow(df), size=0.2*nrow(df))
Ttest = df[indexes,]
Ttrain = df[-indexes,]

###########
## experimenting

model <- lm(num_trips~tmax*tmin*is_extreme_weather + prcp*is_holiday + snwd + snow*is_weekend, Ttrain)
Ttrain$predicted <- predict(model, Ttrain)
Ttest$predicted <- predict(model, Ttest)

Ttrain <- mutate(Ttrain, diff = (num_trips - predicted)^2)
Ttest <- mutate(Ttest, diff = (num_trips - predicted)^2)

rmseTest <- sqrt(sum(Ttest$diff)/nrow(Ttest))
rmseTrain <- sqrt(sum(Ttrain$diff)/nrow(Ttrain))
rmseTest
rmseTrain




##########


train_cor <- c()
test_cor <- c()

for(k in 1:20) {
  model <- lm(num_trips~poly(tmin, k, raw = T), Ttrain)
  Ttrain$predicted <- predict(model, Ttrain)
  Ttest$predicted <- predict(model, Ttest)
  
  train_cor[k] <- cor(Ttrain$predicted, Ttrain$num_trips)^2
  test_cor[k] <- cor(Ttest$predicted, Ttest$num_trips)^2
}

Ttrain <- mutate(Ttrain, diff = (num_trips - predicted)^2)
Ttest <- mutate(Ttest, diff = (num_trips - predicted)^2)

rmseTest <- sqrt(sum(Ttest$diff)/nrow(Ttest))
rmseTrain <- sqrt(sum(Ttrain$diff)/nrow(Ttrain))
rmseTest
rmseTrain

cor_results <- data.frame(train = train_cor, test = test_cor, k = 1:20)
ggplot(cor_results) +
  geom_line(aes(x = k, y = train, color = "red")) + 
  geom_line(aes(x = k, y = test))


model <- lm(num_trips~poly(tavg, 6, raw = T)*is_extreme_weather + prcp*is_heavy_rain*is_weekend + snow*is_sig_snow + is_holiday, Ttrain)
Ttest$predicted <- predict(model, Ttest)

Ttest <- mutate(Ttest, diff = (num_trips - predicted)^2)

rmseTest <- sqrt(sum(Ttest$diff)/nrow(Ttest))
rmseTest

summary(model)
# Multiple R-squared:  0.8957,	Adjusted R-squared:  0.8904

ggplot(Ttest) +
  geom_point(aes(x = ymd, y = num_trips, color="red")) +
  geom_point(aes(x = ymd, y = predicted))

ggplot(Ttest) +
  geom_point(aes(x = predicted, y = num_trips, color = is_weekend, shape = is_holiday)) +
  geom_abline()

############
model <- lm(num_trips~tavg*is_weekend*tmin*prcp*is_holiday, df)
Ttest$predicted <- predict(model, Ttest)
summary(model)
ggplot(Ttest, aes(color = is_weekend)) +
  geom_point(aes(x = ymd, y = num_trips)) +
  geom_point(aes(x = ymd, y = predicted))


ggplot(Ttest) +
  geom_point(aes(x = predicted, y = num_trips, color = is_holiday)) +
  geom_abline()

Ttest <- mutate(Ttest, diff = (num_trips - predicted)^2)

rmseTest <- sqrt(sum(Ttest$diff)/nrow(Ttest))
rmseTest

#######################################
## 5 fold cross validation
df$fold <- sample(1:5, nrow(df), replace=T)
rmse <- c()
for(f in 1:5) {
  train <- filter(df, fold != f)
  test <- filter(df, fold == f)
  
  #model <- lm(num_trips~tmax*tmin*is_extreme_weather + prcp*is_holiday + snwd + snow*is_weekend, train)  # old model
  model <- lm(num_trips~tmax + tmin + prcp + is_holiday + snwd + is_weekend, train) # better??? model
  test$predicted <- predict(model, test)
  test <- mutate(test, diff = (num_trips - predicted)^2)
  
  rmseCV <- sqrt(sum(test$diff)/nrow(test))
  rmseCV
  rmse[f] <- rmseCV
}

mean(rmse)

se <- sd(rmse)/sqrt(nrow(df))
se
#######################################
##


set.seed(42)
num_days <- nrow(df)
ndx <- sample(1:num_days, round(0.8 * num_days))

# use this data frame to fit your model
trips_per_day_train <- df[ndx, ]

# pretend this doesn't exist (for now!)
trips_per_day_test <- df[-ndx, ]

######################################

# fit a linear model
model <- lm(num_trips~tmax + tmin + prcp + is_holiday + snwd + is_weekend, data = trips_per_day_train)

# look at the r^2 on the training and test data
cor_train <- cor(predict(model, trips_per_day_train), trips_per_day_train$num_trips)
cor_train^2
cor_test <- cor(predict(model, trips_per_day_test), trips_per_day_test$num_trips)
cor_test^2
# look at the RMSE on the test data
sqrt(mean((predict(model, trips_per_day_test) - trips_per_day_test$num_trips)^2))

# plot the predicted vs. actual values
plot_data <- df
plot_data$predicted <- predict(model, df)
ggplot(plot_data, aes(color=is_weekend)) +
  geom_point(aes(x = ymd, y=num_trips, shape=is_holiday)) +
  geom_line(aes(x = ymd, y=predicted))

# plot trips over time
ggplot(plot_data, aes(x=predicted, y=num_trips, color=is_extreme_weather, shape=is_holiday, size=prcp)) +
  geom_point() +
  geom_abline(slope=1)


########################################
# repeat this, with cv.glmnet
########################################

# fit a linear model with lasso regularization
form <- as.formula(num_trips~tmax + tmin + prcp + is_holiday + snwd + is_weekend)
y <- trips_per_day_train$num_trips
X <- model.matrix(form, data=trips_per_day_train)
cvfit <- cv.glmnet(X, y)

# plot the generalization curve
plot(cvfit)

# predict on all the data 
plot_data <- df
X_plot <- model.matrix(form, data=df)
plot_data$predicted <- as.numeric(predict(cvfit, newx=X_plot, s="lambda.min"))

# measure performance on the test data
cor(plot_data$predicted[-ndx], plot_data$num_trips[-ndx])
sqrt(mean((plot_data$predicted[-ndx] - plot_data$num_trips[-ndx])^2))
#plot_data %>% select(predicted, num_trips)

# plot the predicted vs. actual values
ggplot(plot_data, aes(x=predicted, y=num_trips, color=is_holiday, size=prcp)) +
  geom_point() +
  geom_abline()

# plot trips over time
ggplot(plot_data, aes(color=is_holiday)) +
  geom_point(aes(x = ymd, y=num_trips, shape=is_weekend)) +
  geom_line(aes(x = ymd, y=predicted))


