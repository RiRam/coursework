library(dplyr)
library(readr)


# load model
load("model.RData")

# read in weather
weather <- read.table('weather_2015.csv', header=T, sep=',')

# extract just a few columns, lowercase column names, and parse dates
parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

weather <- select(weather, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather) <- tolower(names(weather))
weather <- mutate(weather,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather <- tbl_df(weather)

# add additional features (is_weekend, is_holiday)
weather <- mutate(weather, day = wday(ymd, T, F))
weather <- mutate(weather, is_weekend = ifelse(weather$day == "Saturday" | weather$day == "Sunday", TRUE, FALSE))

holidays <- c("2014-01-01", "2014-01-20", "2014-02-17", "2014-04-20", "2014-05-11", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-24", "2014-12-25", "2014-12-31")
holidays <- as.Date(holidays)
weather <- mutate(weather, is_holiday = ymd %in% holidays)

# predict
weather$predicted <- predict(model, weather)