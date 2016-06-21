########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)
library(tidyr)
library(lubridate)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')
trips <- mutate(trips, gender=factor(gender, levels=c(0,1,2), labels=c("Unknown","Male","Female")))

########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(x = trips$tripduration)) + geom_histogram() + xlim(0, 7500)

# plot the distribution of trip times by rider type
ggplot(trips, aes(x = trips$tripduration, fill = trips$usertype)) + geom_histogram() + xlim(0, 7500)

# plot the number of trips over each day
ggplot(trips, aes(ymd)) + geom_bar()

# plot the number of trips by gender and age
df <- group_by(trips, as.factor(gender), birth_year) 
df <- summarize(df, total=n())
ggplot(df, aes(x = birth_year, y = total, color = as.factor(gender))) + geom_point()

# plot the ratio of male to female trips by age
# hint: use the spread() function to reshape things to make it easier to compute this ratio
df <- group_by(trips, gender, birth_year) 
df <- summarize(df, total=n())
df <- spread(df, gender, total)
ggplot(df, aes(x = birth_year, y=Male/Female)) + geom_line() + geom_smooth()

########################################
# plot weather data
########################################
# plot the minimum temperature over each day
df <- group_by(weather, date, tmin)
df <- summarize(df)
ggplot(df, aes(x = date, y = tmin)) + geom_point()

# plot the minimum temperature and maximum temperature over each day
# hint: try using the gather() function for this to reshape things before plotting
df <- gather(weather, "maxmin", "temp", 5:6)
ddf <- group_by(df, ymd, temp, maxmin)
df <- summarize(df)
ggplot(df, aes(x = ymd, y = temp, color = maxmin)) + geom_point()

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the minimum temperature over each day
ggplot(trips_with_weather, aes(x = ymd, y = tmin)) + geom_point()

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
df <- group_by(trips_with_weather, ymd, tmin)
df <- summarize(df, perday = n())
ggplot(df, aes(x = tmin, y = perday, color = as.factor(ymd))) + geom_point()

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
df <- group_by(trips_with_weather, ymd, tmin, prcp)
df <- summarize(df, perday = n())
df <- mutate(df, subst = prcp > 50)
ggplot(df, aes(x = tmin, y = perday, color = subst)) + geom_point()

# add a smoothed fit on top of the previous plot, using geom_smooth
df <- group_by(trips_with_weather, ymd, tmin, prcp)
df <- summarize(df, perday = n())
df <- mutate(df, subst = prcp > 50)
ggplot(df, aes(x = tmin, y = perday, color = subst)) + geom_point() + geom_smooth()

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

df <- mutate(trips_with_weather, hour = hour(starttime))
df <- group_by(df, hour, ymd)
df <- select(df, hour, ymd)
df <- summarize(df, count = n())
df <- summarize(group_by(df, hour), m = mean(count), s = sd(count), plus = m+s, minus = m-s)

# plot the above
ggplot(df, aes(x = hour, y = m)) + geom_line() + geom_errorbar(aes(x = hour, ymin = minus, ymax = plus))

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
df <- mutate(trips_with_weather, day = wday(ymd, label = TRUE))
df <- group_by(df, day)
df <- select(df, ymd, day)
df <- summarize(df, num_per_day = n())
df <- summarize(group_by(df, day), mean = mean(num_per_day), stdev = sd(num_per_day))


numdays <- length(unique(trips$ymd))
count <- summarize(group_by(trips_with_weather, ymd), days = n())

# using built in mean() function
df1 <- mutate(trips_with_weather, day = wday(starttime, label = T), hour = hour(starttime)) %>% group_by(ymd, day, hour) %>% summarize(num_trips = n()) %>% group_by(day, hour) %>% summarize(mean_trips = mean(num_trips), sd_trips = sd(num_trips))

# using numdays and sum() to calculate mean
df2 <- mutate(trips_with_weather, day = wday(starttime, label = T), hour = hour(starttime)) %>% group_by(ymd, day, hour) %>% summarize(num_trips = n()) %>% group_by(day, hour) %>% summarize(mean_trips = sum(num_trips)/numdays, sd_trips = sd(num_trips))
ggplot(df1, aes(x = hour, y = mean_trips)) + facet_wrap(~ day) + geom_point()

# hint: use the wday() function from the lubridate package
