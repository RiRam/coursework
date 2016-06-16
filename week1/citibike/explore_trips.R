library(dplyr)

load('trips.RData')

# count the number of trips (= rows in the data frame)
nrow(trips)
[1] 5370361

# find the earliest and latest birth years (see help for max and min to deal with NAs)
min(trips$birth_year, na.rm = TRUE)
[1] 1899

max(trips$birth_year, na.rm = TRUE)
[1] 1998

# use filter and grepl to find all trips that either start or end on broadway
starts_with_broadway <- grepl("Broadway", the_tibble$start_station_name)
ends_with_broadway <- grepl("Broadway", the_tibble$end_station_name)
filter(the_tibble, ends_with_broadway | starts_with_broadway)

# do the same, but find all trips that both start and end on broadway
starts_with_broadway <- grepl("Broadway", the_tibble$start_station_name)
ends_with_broadway <- grepl("Broadway", the_tibble$end_station_name)
filter(the_tibble, ends_with_broadway | starts_with_broadway)

# use filter, select, and distinct to find all unique station names
filter(select(distinct(the_big_tibble, start_station_id), starts_with("start_station_name")), TRUE)

# count trips by gender
the_big_tibble %>% group_by(gender) %>% summarize(total = n())

# find the 10 most frequent station-to-station trips
the_big_tibble %>% group_by(start_station_name, end_station_name) %>% summarize(total = n()) %>% ungroup() %>% arrange(-total)

# count all trips that start and end on broadway
starts_with_broadway <- grepl("Broadway", the_tibble$start_station_name)
ends_with_broadway <- grepl("Broadway", the_tibble$end_station_name)
nrow(filter(the_tibble, ends_with_broadway | starts_with_broadway))
[1] 1060101