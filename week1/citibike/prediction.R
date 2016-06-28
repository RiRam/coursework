load('trips.RData')
trips_with_weather <- mutate(trips_with_weather, day = wday(ymd, T, F))

trips_with_weather$is_weekend <- ifelse(trips_with_weather$day == "Saturday" | trips_with_weather$day == "Sunday", TRUE, FALSE)

df <- group_by(trips_with_weather, is_weekend, ymd, tmax, tmin, prcp, snow) %>% summarize(num_trips = n())
set.seed(147)
# The point of this exercise is to get experience in an open-ended prediction exercise: predicting the total number of 
# Citibike trips taken on a given day. 


indexes <- sample(1:nrow(df), size=0.2*nrow(df))
Ttest = df[indexes,]
Ttrain = df[-indexes,]

train_cor <- c()
test_cor <- c()

for(k in 1:20) {
  model <- lm(num_trips~is_weekend*poly(tmin, k, raw = T)*snow, Ttrain)
  Ttrain$predicted <- predict(model, Ttrain)
  Ttest$predicted <- predict(model, Ttest)
  
  train_cor[k] <- cor(Ttrain$predicted, Ttrain$num_trips)^2
  test_cor[k] <- cor(Ttest$predicted, Ttest$num_trips)^2
}

Ttrain <- mutate(Ttrain, diff = (num_trips - predicted)^2)
Ttest <- mutate(Ttest, diff = (num_trips - predicted)^2)

rmseTest <- sqrt(sum(Ttest$diff)/nrow(Ttest))
rmseTrain <- sqrt(sum(Ttrain$diff)/nrow(Ttrain))

cor_results <- data.frame(train = train_cor, test = test_cor, k = 1:20)
ggplot(cor_results) +
  geom_line(aes(x = k, y = train, color = "red")) + 
  geom_line(aes(x = k, y = test))





model <- lm(num_trips~is_weekend*poly(tmin, 5, raw = T)*snow, Ttrain)
Ttest$predicted <- predict(model, Ttest)

summary(model)
# Multiple R-squared:   0.76,	Adjusted R-squared:  0.7505 

ggplot(Ttest, aes(color = is_weekend)) +
  geom_point(aes(x = ymd, y = num_trips)) +
  geom_point(aes(x = ymd, y = predicted))

ggplot(Ttest) +
  geom_point(aes(x = predicted, y = num_trips, color = is_weekend)) +
  geom_abline()

############
model <- lm(num_trips~tmax*tmin*is_weekend*prcp, df)
Ttest$predicted <- predict(model, Ttest)
summary(model)
ggplot(Ttest, aes(color = is_weekend)) +
  geom_point(aes(x = ymd, y = num_trips)) +
  geom_point(aes(x = ymd, y = predicted))
