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

holidays <- as.Date(c("2015-01-01", "2015-01-19", "2015-02-16", "2015-05-25", "2015-07-03", "2015-09-07", "2015-10-12", "2015-11-11", "2015-11-26", "2015-12-25"))
weather <- mutate(weather, is_holiday = ymd %in% holidays)

# predict
weather$predicted <- predict(model, weather)