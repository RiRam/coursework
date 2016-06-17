########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


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
df <- group_by(trips, gender, birth_year) 
df <- summarize(df, total=n())
ggplot(df, aes(x = birth_year, y = total, color = as.factor(gender))) + geom_point()

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
