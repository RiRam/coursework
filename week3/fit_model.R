library(dplyr)
library(lubridate)

setwd("~/coursework/week1/citibike")

load('trips.RData')
trips_with_weather <- mutate(trips_with_weather, day = wday(ymd, T, F))

trips_with_weather$is_weekend <- ifelse(trips_with_weather$day == "Saturday" | trips_with_weather$day == "Sunday", TRUE, FALSE)

holidays <- c("2014-01-01", "2014-01-20", "2014-02-17", "2014-04-20", "2014-05-11", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-24", "2014-12-25", "2014-12-31")
holidays <- as.Date(holidays)
trips_with_weather <- mutate(trips_with_weather, is_holiday = ymd %in% holidays)

df <- group_by(trips_with_weather, is_weekend, ymd, tmax, tmin, snwd, prcp, snow, is_holiday) %>% summarize(num_trips = n())

set.seed(42)
num_days <- nrow(df)
ndx <- sample(1:num_days, round(0.8 * num_days))

trips_per_day_train <- df[ndx, ]

trips_per_day_test <- df[-ndx, ]

model <- lm(num_trips~tmax + tmin + prcp + is_holiday + snwd + is_weekend, data = trips_per_day_train)

setwd("~/coursework/week3")

save(model, file="model.RData")
