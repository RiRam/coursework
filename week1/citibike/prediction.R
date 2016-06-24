trips <- mutate(trips, day = wday(ymd, T, F))

trips$is_weekend <- ifelse(trips$day == "Saturday" | trips$day == "Sunday", TRUE, FALSE)
