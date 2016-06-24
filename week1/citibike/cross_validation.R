# Cross-validation for Citibike trips

# In this assignment we'll predict number of trips per day as a function of the weather on that day.

# 1. Create a dataframe with one row for each day, the number of trips taken on that day, and the minimum temperature on
# that day.

df <- summarize(group_by(trips_with_weather, ymd, tmin), num_trips = n()) %>% select(ymd, num_trips, tmin)

# 2. Split the data into a randomly selected training and test set, as in the above exercise, with 80% of the data for 
# training the model and 20% for testing.

indexes <- sample(1:nrow(df), size=0.2*nrow(df))
Ttest = df[indexes,]
Ttrain = df[-indexes,]

# 3. Fit a model to predict the number of trips as a (linear) function of the minimum temperature, and evaluate the fit 
# on the training and testing data sets. Do this first visually by plotting the predicted and actual values. Then do 
# this with R^2, as above. You'll want to use the predict and cor functions for this.

model <- lm(num_trips~tmin, Ttrain)

summary(model)
# Multiple R-squared:  0.6755,	Adjusted R-squared:  0.6729 
Ttest$predicted <- predict(model, Ttest)

ggplot(Ttest, aes(x = predicted, y = num_trips, color=tmin)) + geom_point() +geom_smooth()

cor(Ttest$predicted, Ttest$num_trips)^2
# 0.7257838

# 4. Repeat this procedure, but add a quadratic term to your model (e.g., + tmin^2, or equivalently + poly(k,2)). How 
# does the model change, and how do the fits between the linear and quadratic models compare?

model <- lm(num_trips~tmin+poly(tmin, 2), Ttrain)

summary(model)
# Multiple R-squared:  0.6897,	Adjusted R-squared:  0.6848  
Ttest$predicted <- predict(model, Ttest)
#!!! got a warning message: prediction from a rank-deficient fit may be misleading - not enough data?

ggplot(Ttest, aes(x = predicted, y = num_trips, color=tmin)) + geom_point() +geom_smooth()

cor(Ttest$predicted, Ttest$num_trips)^2
# 0.7260215

# The model changes very slightly, and the fit increases by a small amount in the r^2.

# 5. Now automate this, extending the model to higher-order polynomials with a for loop over the degree k. For each 
# value of k, fit a model to the training data and save the R^2 on the training data to one vector and test vector to
# another. Then plot the training and test R^2 as a function of k. What value of k has the best performance?

train_cor <- c()
test_cor <- c()

for(k in 1:20) {
  model <- lm(num_trips~tmin+poly(tmin, k), Ttrain)
  Ttrain$predicted <- predict(model, Ttrain)
  Ttest$predicted <- predict(model, Ttest)
  
  train_cor[k] <- cor(Ttrain$predicted, Ttrain$num_trips)^2
  test_cor[k] <- cor(Ttest$predicted, Ttest$num_trips)^2
}

cor_results <- data.frame(train = train_cor, test = test_cor, k = 1:20)
ggplot(cor_results) +
  # geom_line(aes(x = k, y = train, color="red")) + 
  # geom_line(aes(x = k, y = test, color="blue"))
  geom_smooth(aes(x = k, y = train, color = "red")) + 
  geom_smooth(aes(x = k, y = test))

# 6. Finally, fit one model for the value of k with the best performance in 6), and plot the actual and predicted values
# for this model.


