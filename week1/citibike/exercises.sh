#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations
cut -d, -f5 201402-citibike-tripdata.csv | tail -n+2 | sort | uniq | wc -l
cut -d, -f5 201402-citibike-tripdata.csv | grep -v "start station name"| sort | uniq | wc -l
cut -d, -f5 201402-citibike-tripdata.csv | awk 'NR > 1'| sort | uniq | wc -l

# count the number of unique bikes
cut -d, -f12 201402-citibike-tripdata.csv | awk 'NR > 1' | sort | uniq | wc -l

# extract all of the trip start times
cut -d, -f2 201402-citibike-tripdata.csv

# count the number of trips per day
cut -d, -f2 201402-citibike-tripdata.csv | awk 'NR > 1' | cut -c2-11 | uniq -c
cut -d, -f2 201402-citibike-tripdata.csv | grep -Eo '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}' | sort | uniq -c


# find the day with the fewest rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -c2-11 | uniq -c | sort -n | head -n2 | tail -n1
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f2 | sort | cut -d' ' -f1 | uniq - c | sort -nr | sed -n '1p' | cut -d'"' -f2

# find the id of the bike with the most rides
cut -d, -f12 201402-citibike-tripdata.csv | sort -n | uniq -c | sort -n | tail -n1

# count the number of riders by gender and birth year
cut -d, -f15 201402-citibike-tripdata.csv | sort -n | uniq -c #gender
cut -d, -f14 201402-citibike-tripdata.csv | sort -n | uniq -c #birth year

awk -F, '{if ($15 ~ 1) men[$14]++; else if ($15 ~ 2) women[$14]++;} END {print "women:"; for(key in women) print key"\t"women[key]; print "men:"; for (key in men) print key"\t"men[key];}' 201402-citibike-tripdata.csv

# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
awk -F, '$5 ~ /.*[0-9].*&.*[0-9].*/' 201402-citibike-tripdata.csv | wc -l

# compute the average trip duration
awk -F, '/[0-9]/ {gsub(/"/, "", $1); sum+=$1; count++} END {print sum/count}' 201402-citibike-tripdata.csv










# !!! extra !!! compute the average trip duration of trips starting on a station on Broadway
awk -F, '/[0-9]/ {gsub(/"/, "", $1); if($5 ~ /.*Broadway.*/) {sum+=$1; count++}} END {print sum/count}' 201402-citibike-tripdata.csv
