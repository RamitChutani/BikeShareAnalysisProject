# =========================
# STEP 0: PREPARING SESSION
# =========================

#installing packages
install.packages("tidyverse")

#loading packages into session
library(tidyverse) # tidyverse is a collection of R packages designed for data science
library(conflicted) # this package helps manage conflicts by asking which functions (with the same name) take priority

# following packages are loaded as part of tidyverse
# library(lubridate) # this package provides intuitive functions to simplify working with date-time data
# library(stringr) # package for easy and consistent string manipulation. specifically str_trim function
# library(purrr) # provides tools for simplifying tasks like applying functions to each element of a list/vector
# library(ggplot2) # this package provides a powerful and flexible framework for creating complex and customizable graphics

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")





# =======================
# STEP 1: COLLECTING DATA
# =======================

# setting working directory to make it easier to load files
setwd("C:/Users/Ramit/Documents/CAPSTONE PROJECT/CAPSTONE FINAL/data/csv/")

# load csv into work space
tripdata_2306 <- read.csv("202306-divvy-tripdata.csv")
tripdata_2307 <- read.csv("202307-divvy-tripdata.csv")
tripdata_2308 <- read.csv("202308-divvy-tripdata.csv")
tripdata_2309 <- read.csv("202309-divvy-tripdata.csv")
tripdata_2310 <- read.csv("202310-divvy-tripdata.csv")
tripdata_2311 <- read.csv("202311-divvy-tripdata.csv")
tripdata_2312 <- read.csv("202312-divvy-tripdata.csv")
tripdata_2401 <- read.csv("202401-divvy-tripdata.csv")
tripdata_2402 <- read.csv("202402-divvy-tripdata.csv")
tripdata_2403 <- read.csv("202403-divvy-tripdata.csv")
tripdata_2404 <- read.csv("202404-divvy-tripdata.csv")
tripdata_2405 <- read.csv("202405-divvy-tripdata.csv")
tripdata_2406 <- read.csv("202406-divvy-tripdata.csv")





# ========================================
# STEP 2: COMBINE DATA INTO A SINGLE TABLE
# ========================================

# create a list of all the data frames
df_list <- list(tripdata_2306, tripdata_2307, tripdata_2308, tripdata_2309, 
                tripdata_2310, tripdata_2311, tripdata_2312, tripdata_2401, 
                tripdata_2402, tripdata_2403, tripdata_2404, tripdata_2405, 
                tripdata_2406)

# code to check if columns match names and type before joining data using a function
check_column_consistency <- function(df_list) {
  reference_cols <- data.frame(
    name = names(df_list[[1]]),
    type = sapply(df_list[[1]], class),
    stringsAsFactors = FALSE
  ) # Get column names and types of the first data frame
  
  for (i in 2:length(df_list)) {
    current_cols <- data.frame(
      name = names(df_list[[i]]),
      type = sapply(df_list[[i]], class),
      stringsAsFactors = FALSE
    )  # Check each data frame against the reference
    
    if (!identical(reference_cols, current_cols)) {
      print(paste("Dataframe", i, "has differences:"))
      differences <- anti_join(reference_cols, current_cols, by = "name")
      print(differences)
    } else {
      print(paste("Dataframe", i, "matches the reference."))
    }
  }
}

# running consistency check
check_column_consistency(df_list) 

# [1] "Dataframe 2 matches the reference."
# [1] "Dataframe 3 matches the reference."
# [1] "Dataframe 4 matches the reference."
# [1] "Dataframe 5 matches the reference."
# [1] "Dataframe 6 matches the reference."
# [1] "Dataframe 7 matches the reference."
# [1] "Dataframe 8 matches the reference."
# [1] "Dataframe 9 matches the reference."
# [1] "Dataframe 10 matches the reference."
# [1] "Dataframe 11 matches the reference."
# [1] "Dataframe 12 matches the reference."
# [1] "Dataframe 13 matches the reference."
# all data frames match the reference

# Combine all data frames into a single data frame
all_trips <- bind_rows(df_list) 

# view combined data frame
View(all_trips)

# remove individual month data to free up memory
rm(list = c("tripdata_2306", "tripdata_2307", "tripdata_2308", "tripdata_2309", 
            "tripdata_2310", "tripdata_2311", "tripdata_2312", "tripdata_2401", 
            "tripdata_2402", "tripdata_2403", "tripdata_2404", "tripdata_2405", 
            "tripdata_2406"))






# =============================================
# STEP 3: CLEAN UP, FIXING DATA, ADDING COLUMNS
# =============================================

## Inspect the combined data frame for list of problems

# list of column names
colnames(all_trips) 

# [1] "ride_id"            "rideable_type"      "started_at"         "ended_at"           "start_station_name" "start_station_id"   "end_station_name"  
# [8] "end_station_id"     "start_lat"          "start_lng"          "end_lat"            "end_lng"            "member_casual" 

# list of column types
str(all_trips) 

# 'data.frame':	6453999 obs. of  13 variables:
# $ ride_id           : chr  "6F1682AC40EB6F71" "622A1686D64948EB" "3C88859D926253B4" "EAD8A5E0259DEC88" ...
# $ rideable_type     : chr  "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
# $ started_at        : chr  "2023-06-05 13:34:12" "2023-06-05 01:30:22" "2023-06-20 18:15:49" "2023-06-19 14:56:00" ...
# $ ended_at          : chr  "2023-06-05 14:31:56" "2023-06-05 01:33:06" "2023-06-20 18:32:05" "2023-06-19 15:00:35" ...
# $ start_station_name: chr  "" "" "" "" ...
# $ start_station_id  : chr  "" "" "" "" ...
# $ end_station_name  : chr  "" "" "" "" ...
# $ end_station_id    : chr  "" "" "" "" ...
# $ start_lat         : num  41.9 41.9 42 42 42 ...
# $ start_lng         : num  -87.7 -87.7 -87.7 -87.7 -87.7 ...
# $ end_lat           : num  41.9 41.9 41.9 42 42 ...
# $ end_lng           : num  -87.7 -87.7 -87.6 -87.7 -87.7 ...
# $ member_casual     : chr  "member" "member" "member" "member" ...

## Problem 1: INCORRECT TYPES ("started_at" and "ended_at" columns are in chr type. needs converting to date-time data before further manipulations)
# =============================================

# Converting using lubridate
all_trips <- all_trips %>%
  mutate(started_at = ymd_hms(started_at), ended_at = ymd_hms(ended_at))

# checking conversion
class(all_trips$started_at) #[1] "POSIXct" "POSIXt" 
class(all_trips$ended_at) #[1] "POSIXct" "POSIXt" 

#statistical summary for numerical columns
summary(all_trips) 

# ride_id           rideable_type     started_at                       ended_at                         start_station_name start_station_id   end_station_name  
# Length:6453999    Length:6453999    Min.   :2023-06-01 00:00:44.00   Min.   :2023-06-01 00:02:56.00   Length:6453999     Length:6453999     Length:6453999    
# Class :character  Class :character  1st Qu.:2023-08-05 15:30:47.00   1st Qu.:2023-08-05 15:53:51.00   Class :character   Class :character   Class :character  
# Mode  :character  Mode  :character  Median :2023-10-17 07:39:52.00   Median :2023-10-17 07:50:36.00   Mode  :character   Mode  :character   Mode  :character  
#                                     Mean   :2023-11-26 07:45:49.20   Mean   :2023-11-26 08:04:19.18                                                           
#                                     3rd Qu.:2024-04-12 14:29:54.00   3rd Qu.:2024-04-12 14:45:59.50                                                           
#                                     Max.   :2024-06-30 23:55:17.06   Max.   :2024-06-30 23:59:57.93                                                           
# 
# end_station_id    start_lat       start_lng        end_lat           end_lng         member_casual     
# Length:6453999    Min.   :41.63   Min.   :-87.94   Min.   : 0.00   Min.   :-88.16   Length:6453999    
# Class :character  1st Qu.:41.88   1st Qu.:-87.66   1st Qu.:41.88   1st Qu.:-87.66   Class :character  
# Mode  :character  Median :41.90   Median :-87.64   Median :41.90   Median :-87.64   Mode  :character  
#                   Mean   :41.90   Mean   :-87.65   Mean   :41.90   Mean   :-87.65                     
#                   3rd Qu.:41.93   3rd Qu.:-87.63   3rd Qu.:41.93   3rd Qu.:-87.63                     
#                   Max.   :42.07   Max.   :-87.46   Max.   :42.19   Max.   :  0.00                     
#                                                    NA's   :8808    NA's   :8808  

## ================ ##
## NEW COLUMNS HERE ##
## ================ ##

# adding new column: TRIP LENGTH in minutes
all_trips <- all_trips %>%
  mutate(trip_length = as.numeric(difftime(ended_at, started_at, units = "mins")))

# add columns for weekday/month/year/day of month for another way to group data in analysis
all_trips <- all_trips %>%
  mutate(
    day_of_week_started = wday(started_at, label = TRUE, abbr = TRUE),
    day_of_week_ended = wday(ended_at, label = TRUE, abbr = TRUE),
    month_started = month(started_at, label = TRUE, abbr = TRUE),
    month_ended = month(ended_at, label = TRUE, abbr = TRUE),
    year_started = year(started_at),
    year_ended = year(ended_at),
    date_started = day(started_at),
    date_ended = day(ended_at)
  )

## ================ ##

## Problem 2: EMPTY STRINGS (trim leading and trailing white space from all character columns)
# =============================================

# trim using stringr package, str_trim function
all_trips <- all_trips %>%
  mutate(across(where(is.character), str_trim)) 

## Problem 3: NULL DATA (convert empty strings to null where applicable, find null values and decide how to fix null data)
# =============================================

# convert all empty strings to NA using na_if()
all_trips <- all_trips %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# creating table for column NA values
na_count <- tibble(
  column = names(all_trips),
  NA_count = map_int(all_trips, ~ sum(is.na(.)))
)

View(na_count) # start_name/id have 1049262 NA values, end_name/id have 1104606 NA values, end_lat/long have 8808 NA values each

# later analysis found that lat long data and station name data did not produce same number of unique stations by an order of magnitude 
# 1.6 million unique lat long combinations vs ~1600 unique station names
# decision to remove all null station_name rows, though it is a sizable chunk of data, could not think of a solution in allotted time

# maybe first check if this missing data rows are spread evenly, otherwise it will cause sampling error by removing it
na__bias <- all_trips %>%
  group_by(year_started, month_started) %>%
  summarise(
    total_rows = n(),
    na_count_start = sum(is.na(start_station_name)),
    na_count_end = sum(is.na(end_station_name)),
    na_percentage_value1 = (na_count_start / total_rows) * 100,
    na_percentage_value2 = (na_count_end / total_rows) * 100
  )

## NA values fairly uniformly distributed across each month of analysis, sampling bias is unlikely to be introduced due to removal of null data

# creating new df with removed null data (=rows with start_station_name and end_station_name missing)
all_trips_v2 <- all_trips %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name)) 

# remove lat/long (lets leave id) since its useless?
all_trips_v2 <- all_trips_v2 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

# updated na_count table
na_count_v2 <- tibble(
  column = names(all_trips_v2),
  NA_count = map_int(all_trips_v2, ~ sum(is.na(.)))
)

View(na_count_v2) # all NA values removed

## Problem 4: Duplicate Data (ride_id columns should all be distinct trips)
# =============================================

# checking for duplicates assuming IDs cannot be same for two trips
n_distinct(all_trips_v2$ride_id) 

# 4809101 distinct ids in 4809222rows => 121 duplicates

# creating new df to identify all duplicate ride_id's
duplicate_ids <- all_trips_v2 %>%
  filter(ride_id %in% ride_id[duplicated(ride_id)]) 

View(all_duplicate_ids) # viewing new df suggests that entire rows have been duplicated, so we can remove these rows from all_trips

# creating new df for unique trips, where duplicated ride_id rows are removed (keeping original row for duplicates) and all other distinct rows
unique_trips <- all_trips_v2 %>%
  distinct(ride_id, .keep_all = TRUE)

## Problem 5: DATA ENTRY ERRORS (checking for typos in rideable_type column and member_casual column)
# =============================================

# checking unique data entries in the following columns
unique(unique_trips$rideable_type) #[1] "electric_bike" "classic_bike"  "docked_bike"
unique(unique_trips$member_casual) #[1] "member" "casual"

# no data entry erros detected

## Problem 6: INVALID DATA (check if time data is valid, if location latitude/longitude are feasible, and checking for real trips)
# =============================================

View(na_count)
# checking na_count df for date-time data after converting using ymd_hms() shows that these columns do not have any nulls
# it can be safely conclude that all data is valid and as it would be converted to null if it was not valid

# check for "HQ QR" in station name
HQ_trips <- unique_trips %>%
  filter(str_detect(start_station_name, regex("hq", ignore_case = TRUE)) | str_detect(end_station_name, regex("hq", ignore_case = TRUE)))

# check for "repair" in name
repair_trips <- unique_trips %>% 
  filter(str_detect(start_station_name, regex("repair", ignore_case = TRUE)) | str_detect(end_station_name, regex("repair", ignore_case = TRUE)))

# check for "divvy" in station id
divvy_trips <- unique_trips %>% 
  filter(str_detect(start_station_id, regex("divvy", ignore_case = TRUE)) | str_detect(end_station_id, regex("divvy", ignore_case = TRUE)))

# check for "test" in station name
test_trips <- unique_trips %>% 
  filter(str_detect(start_station_name, regex("test", ignore_case = TRUE)) | str_detect(end_station_name, regex("test", ignore_case = TRUE)))

# 0 trips with "hq", 1 trip with "repair", 1 trip with "divvy" (same as repair), 15 trips with "test"

# creating new df with removed 16 "fake" trips
real_trips <- unique_trips %>%
  filter(
    !str_detect(start_station_name, regex("repair", ignore_case = TRUE)) &
    !str_detect(end_station_name, regex("repair", ignore_case = TRUE)) &
    !str_detect(start_station_name, regex("test", ignore_case = TRUE)) &
    !str_detect(end_station_name, regex("test", ignore_case = TRUE)) &
    !str_detect(start_station_id, regex("divvy", ignore_case = TRUE)) &
    !str_detect(end_station_id, regex("divvy", ignore_case = TRUE))
  )

# checking for outliers in trip length
summary(real_trips$trip_length) 

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -54.567     5.833    10.167    16.620    18.183   11152.267 = 7-8 days 

# trip lengths up-to 7-8 days can be valid/explained as week long trips or tourists in town renting the bike, so only negative trips need to be removed

# checking how many trips have -ve or zero trip length
impossible_trips <- real_trips %>%
  filter(trip_length <= 0) 
# 666 trips with negative time, majority of which are 0 min round trips

# checking how many trips are false starts (Choosing round trips with less than 3 min length as false starts)
false_starts <- real_trips %>% 
  filter((start_station_name == end_station_name) & trip_length < 3)
# 112343 trips = 0.0233% of total trips

# creating new df, removing impossible trips and false starts. also removing any trip less than 30 secs
real_trips_v2 <- real_trips %>%
  filter(trip_length > 0.5 & !(start_station_name == end_station_name & trip_length < 3))

# rechecking trip length 
summary(real_trips_v2$trip_length) 

#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.504     6.100    10.417    16.998    18.483   11152.267


# remove older data frames to free up memory
rm("all_trips", "all_trips_v2","unique_trips", "real_trips")








# ==========================================
# STEP 4: DATA ANALYSIS AND VISUALIZATIONS
# ==========================================

## TRIP LENGTH
## ===========

# checking for extreme outlier trips (more than 1 day)
outlier_trips <- real_trips_v2 %>% 
  filter(trip_length > 1440)
# 180 trips longer than 1 day, and 8 outlier trips more than 1500 minutes

# rechecking summary of trip length
trip_stats <- real_trips_v2 %>%
  summarise(
    count = n(),
    mean = mean(trip_length, na.rm = TRUE),
    median = median(trip_length, na.rm = TRUE),
    min = min(trip_length, na.rm = TRUE),
    max = max(trip_length, na.rm = TRUE),
    sd = sd(trip_length, na.rm = TRUE)
  )

print(as.data.frame(trip_stats))
#     count     mean   median       min      max       sd
# 1 4696424 16.99843 10.41667 0.5042667 11152.27 37.24257

# Histogram of trip lengths 
ggplot(data = real_trips_v2, aes(x = trip_length)) + 
  geom_histogram(binwidth = 1, fill = "blue", alpha = 1) + 
  labs(title = "Distribution of Trip Lengths", x = "Trip Length (mins)", y = "Frequency") +
  theme_light()

# Histogram of trip lengths shorter than a day
ggplot(data = real_trips_v2 %>% filter(trip_length < 1440), aes(x = trip_length)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 1) +
  labs(title = "Distribution of Trips shorter than a day", x = "Trip Length (mins)", y = "Frequency") +
  theme_light()

# Histogram of trip lengths longer than a day
ggplot(data = real_trips_v2 %>% filter(trip_length > 1440), aes(x = trip_length)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 1) +
  labs(title = "Distribution of Trips more than a Day", x = "Trip Length (mins)", y = "Frequency") +
  theme_light()

# Histogram of trip lengths shorter than 2 hrs
ggplot(data = real_trips_v2 %>% filter(trip_length < 120), aes(x = trip_length)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 1) +
  labs(title = "Distribution of Trips shorter than 2 hours", x = "Trip Length (mins)", y = "Frequency") +
  theme_light()

# Histogram of trip lengths from 2 hours to 24 hours
ggplot(data = real_trips_v2 %>% filter(trip_length > 120 & trip_length < 1440), aes(x = trip_length)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 1) +
  labs(title = "Distribution of Trips from 2 hours to 24 hours", x = "Trip Length (mins)", y = "Frequency") +
  theme_light()



## DAY OF WEEK / MONTH
## ===================

# trip length statistics grouped by day of week (can be altered to group by other stuff)
trip_stats_by_dayofweek <- real_trips_v2 %>%
  group_by(day_of_week_started) %>%
  summarize(
    count = n(),
    mean = mean(trip_length, na.rm = TRUE),
    median = median(trip_length, na.rm = TRUE),
    min = min(trip_length, na.rm = TRUE),
    max = max(trip_length, na.rm = TRUE),
    sd = sd(trip_length, na.rm = TRUE)
  )

print(as.data.frame(trip_stats_by_dayofweek))
# day_of_week_started  count     mean    median       min       max       sd
# 1                 Sun 627198 20.89963 12.483333 0.5166667  3818.400 42.27194
# 2                 Mon 621455 15.93327  9.716667 0.5166667  1497.383 34.91141
# 3                 Tue 655049 14.92410  9.583333 0.5042667  1494.467 31.59625
# 4                 Wed 682552 14.94865  9.600000 0.5121167  5379.000 34.99805
# 5                 Thu 695969 14.78116  9.566667 0.5166667  6891.217 34.03754
# 6                 Fri 678705 16.61780 10.183333 0.5166667  3300.833 37.24026
# 7                 Sat 735496 20.77067 12.583333 0.5166667 11152.267 43.13255

# frequency of trips started by day of week
real_trips_v2 %>%
  count(day_of_week_started) %>%
  ggplot(aes(x = day_of_week_started, y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) + # Add labels above bars
  labs(title = "Frequency of Trips Started by Day of the Week",
       x = "Day of the Week", y = "Number of Trips Started") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# Saturday is most popular day for starting a trip

# add new column: TRIP DURATION CATEGORY
real_trips_v2 <- real_trips_v2 %>%
  mutate(
    trip_duration_category = case_when(
      trip_length < 60 ~ "Less than 1 hour",
      trip_length < 120 ~ "Less than 2 hours",
      trip_length < 360 ~ "Less than 6 hours",
      trip_length < 1440 ~ "Less than 1 day",
      TRUE ~ "More than 1 day"
    )
)

# Plot for trips less than 1 hour
real_trips_v2 %>%
  filter(trip_duration_category == "Less than 1 hour") %>%
  count(day_of_week_started) %>%
  ggplot(aes(x = day_of_week_started, y = n, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Trips Less Than 1 Hour Over Days of the Week",
    x = "Day of the Week",
    y = "Number of Trips"
  ) +
  theme_light()

# Plot for remaining categories
real_trips_v2 %>%
  filter(trip_duration_category != "Less than 1 hour") %>%
  count(day_of_week_started, trip_duration_category) %>%
  ggplot(aes(x = day_of_week_started, y = n, color = trip_duration_category, group = trip_duration_category)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  geom_text(aes(label = n), vjust = 1, hjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Trips (Remaining Categories) Over Days of the Week",
    x = "Day of the Week",
    y = "Number of Trips",
    color = "Trip Duration"
  ) +
  theme_light()

# trip length statistics grouped by months (can be altered to group by other stuff)
trip_stats_by_month <- real_trips_v2 %>%
  group_by(month_started) %>%
  summarize(
    count = n(),
    mean = mean(trip_length, na.rm = TRUE),
    median = median(trip_length, na.rm = TRUE),
    min = min(trip_length, na.rm = TRUE),
    max = max(trip_length, na.rm = TRUE),
    sd = sd(trip_length, na.rm = TRUE)
  )

print(as.data.frame(trip_stats_by_month))
#   month_started   count     mean    median       min       max       sd
# 1            Jan  110201 12.54852  7.600000 0.5166667  1497.317 45.53949
# 2            Feb  181533 13.63680  8.533333 0.5166667  1497.650 37.10126
# 3            Mar  225503 14.58703  9.100000 0.5166667  1509.367 32.66843
# 4            Apr  291036 16.58149  9.983333 0.5166667  1493.550 35.78102
# 5            May  431577 18.86642 11.433333 0.5166667  1496.417 37.63464
# 6            Jun 1007772 18.60017 11.557025 0.5042667 11152.267 39.35960
# 7            Jul  557606 18.94022 11.666667 0.5166667  2457.850 37.79090
# 8            Aug  569771 18.17979 11.266667 0.5166667  6891.217 40.80298
# 9            Sep  494237 17.29722 10.716667 0.5333333  1497.900 34.58892
# 10           Oct  394599 15.11423  9.333333 0.5166667  1495.567 34.67958
# 11           Nov  268968 13.10732  8.483333 0.5500000  1497.500 31.04240
# 12           Dec  163621 12.30409  8.066667 0.5166667  1494.467 31.08465


## START/END TIMES
## ===============

# plotting start/end times (extract hour and minute of the day from started_at and ended_at)
real_trips_v2 %>%
  mutate(
    start_time_minute = hour(started_at) * 60 + minute(started_at) + second(started_at) / 60,
    end_time_minute = hour(ended_at) * 60 + minute(ended_at) + second(ended_at) / 60
  ) %>%
  ggplot() + 
  geom_density(aes(x = start_time_minute, color = "Start Time"), fill = "blue", alpha = 0.3) +
  geom_density(aes(x = end_time_minute, color = "End Time"), fill = "red", alpha = 0.3) +
  scale_x_continuous(name = "Time of Day (Minutes)", breaks = seq(0, 1440, by = 120), labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")) +
  labs(title = "Density of Trip Start and End Times", color = "Event") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# member trips
real_trips_v2 %>%
  filter(member_casual == "member") %>% 
  mutate(
    start_time_minute = hour(started_at) * 60 + minute(started_at) + second(started_at) / 60,
    end_time_minute = hour(ended_at) * 60 + minute(ended_at) + second(ended_at) / 60
  ) %>%
  ggplot() + 
  geom_density(aes(x = start_time_minute, color = "Start Time"), fill = "blue", alpha = 0.3) +
  geom_density(aes(x = end_time_minute, color = "End Time"), fill = "red", alpha = 0.3) +
  scale_x_continuous(name = "Time of Day (Minutes)", breaks = seq(0, 1440, by = 120), labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")) +
  labs(title = "Density of Member Trip Times", color = "Event") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# casual trips
real_trips_v2 %>%
  filter(member_casual == "casual") %>% 
  mutate(
    start_time_minute = hour(started_at) * 60 + minute(started_at) + second(started_at) / 60,
    end_time_minute = hour(ended_at) * 60 + minute(ended_at) + second(ended_at) / 60
  ) %>%
  ggplot() + 
  geom_density(aes(x = start_time_minute, color = "Start Time"), fill = "blue", alpha = 0.3) +
  geom_density(aes(x = end_time_minute, color = "End Time"), fill = "red", alpha = 0.3) +
  scale_x_continuous(name = "Time of Day (Minutes)", breaks = seq(0, 1440, by = 120), labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")) +
  labs(title = "Density of Casual Trip Times", color = "Event") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# trips that start and end on different days (that are 1 hour or longer)
overnight_trips <- real_trips_v2 %>% 
  filter(date_started != date_ended & trip_duration_category != "Less than 1 hour")


# plotting start/end times again with frequency plot
real_trips_v2 %>%
  mutate(
    start_time_minute = hour(started_at) * 60 + minute(started_at) + second(started_at) / 60,
    end_time_minute = hour(ended_at) * 60 + minute(ended_at) + second(ended_at) / 60
  ) %>%
  ggplot() + 
  geom_freqpoly(aes(x = start_time_minute, color = "Start Time"), binwidth = 1, alpha = 0.6, size= 1) +
  geom_freqpoly(aes(x = end_time_minute, color = "End Time"), binwidth = 1, alpha = 0.3, size = 0.5) +
  scale_x_continuous(name = "Time of Day", breaks = seq(0, 1440, by = 120), labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")) +
  scale_color_manual(values = c("Start Time" = "blue", "End Time" = "red")) +  # Specify colors for the legend
  labs(title = "Number of Trips Starting and Ending by Time of Day", color = "Time Type", y = "Number of Trips") +  # Add title to the legend
  theme_light()

# same as above for members
real_trips_v2 %>%
  filter(member_casual == "member") %>% 
  mutate(
    start_time_minute = hour(started_at) * 60 + minute(started_at) + second(started_at) / 60,
    end_time_minute = hour(ended_at) * 60 + minute(ended_at) + second(ended_at) / 60
  ) %>%
  ggplot() + 
  geom_freqpoly(aes(x = start_time_minute, color = "Start Time"), binwidth = 1, alpha = 0.6, size= 1) +
  geom_freqpoly(aes(x = end_time_minute, color = "End Time"), binwidth = 1, alpha = 0.3, size = 0.5) +
  scale_x_continuous(name = "Time of Day", breaks = seq(0, 1440, by = 120), labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")) +
  scale_color_manual(values = c("Start Time" = "blue", "End Time" = "red")) + 
  labs(title = "Number of Member Trips by Start and End Times", color = " ", y = "Number of Trips") +
  theme_light()


# same as above for casuals
real_trips_v2 %>%
  filter(member_casual == "casual") %>% 
  mutate(
    start_time_minute = hour(started_at) * 60 + minute(started_at) + second(started_at) / 60,
    end_time_minute = hour(ended_at) * 60 + minute(ended_at) + second(ended_at) / 60
  ) %>%
  ggplot() + 
  geom_freqpoly(aes(x = start_time_minute, color = "Start Time"), binwidth = 1, alpha = 0.6, size= 1) +
  geom_freqpoly(aes(x = end_time_minute, color = "End Time"), binwidth = 1, alpha = 0.3, size = 0.5) +
  scale_x_continuous(name = "Time of Day", breaks = seq(0, 1440, by = 120), labels = function(x) format(as.POSIXct(x * 60, origin = "1970-01-01", tz = "UTC"), "%H:%M")) +
  scale_color_manual(values = c("Start Time" = "blue", "End Time" = "red")) + 
  labs(title = "Number of Casual Trips by Start and End Times", color = " ", y = "Number of Trips") +
  theme_light()

## TRIP DURATION CATEGORY
## ======================




## RIDEABLE TYPE 
## =============

# trip length statistics grouped by rideable type (can be altered to group by other stuff)
trip_stats_by_rideable <- real_trips_v2 %>%
  group_by(rideable_type) %>%
  summarize(
    count = n(),
    mean = mean(trip_length, na.rm = TRUE),
    median = median(trip_length, na.rm = TRUE),
    min = min(trip_length, na.rm = TRUE),
    max = max(trip_length, na.rm = TRUE),
    sd = sd(trip_length, na.rm = TRUE)
  )

print(as.data.frame(trip_stats_by_rideable))
#   rideable_type   count     mean   median       min        max        sd
# 1  classic_bike 3096668 18.48751 11.01667 0.5166667  1509.3667  41.42212
# 2   docked_bike   47148 57.28008 29.33333 1.3833333 11152.2667 130.51355
# 3 electric_bike 1552608 12.80523  9.25000 0.5042667   479.9833  13.59558

# bar chart showing distribution of trips along rideable type and member status
ggplot(real_trips_v2, aes(x = factor(rideable_type, levels = c("classic_bike", "electric_bike", "docked_bike")), fill = member_casual)) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Breakdown of Rideable Types by Member Status",
       x = "Rideable Type",
       y = "Trip Count",
       fill = "Column B") +
  theme_light() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1))





## STATION NAMES
## =============

# trip length statistics grouped by start stations(can be altered to group by other stuff)
trip_stats_by_starts <- real_trips_v2 %>%
  group_by(start_station_name) %>%
  summarize(
    count = n(),
    mean = mean(trip_length, na.rm = TRUE),
    median = median(trip_length, na.rm = TRUE),
    min = min(trip_length, na.rm = TRUE),
    max = max(trip_length, na.rm = TRUE),
    sd = sd(trip_length, na.rm = TRUE)
  )

# combine start and end stations into one vector and get unique stations
n_distinct(c(real_trips_v2$start_station_name, real_trips_v2$end_station_name)) #[1] 1685 unique stations

# counting trips starting from each station
start_station_counts <- real_trips_v2 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(trips_started = n())

# counting trips ending at each station
end_station_counts <- real_trips_v2 %>%
  group_by(end_station_name, member_casual) %>%
  summarise(trips_ended = n())

# Identify round trips = 185779 out of 4696424 = 3.956% are round trips (= 2.975m out of 4.808m = 61.8% are round trips)
round_trips <- real_trips_v2 %>%
  filter(start_station_name == end_station_name)

# count round trips for member and casual
round_trips_by_status <- round_trips %>%
  group_by(member_casual) %>%
  summarise(round_trips = n())

# making routes by user type table
routes_by_trip_length <-real_trips_v2 %>%
  group_by(member_casual, start_station_name, end_station_name) %>%
  summarise(
    route = paste(start_station_name, "to", end_station_name)[1],
    num_trips = n(),
    duration = mean(trip_length),
    .groups = "drop"
  ) %>%
  arrange(desc(num_trips))

# top 10 route for casual
casual_routes <- routes_by_trip_length %>%
  filter(member_casual == "casual") %>%
  slice_head(n = 10)

# top 10 route for member
member_routes <- routes_by_trip_length %>%
  filter(member_casual == "member") %>%
  slice_head(n = 10)


## MEMBER STATUS
## =============

# percentage of casual and member trips
trip_percentages <- real_trips_v2 %>%
  count(member_casual) %>%
  mutate(percentage = n / sum(n) * 100)

print(as.data.frame(trip_percentages))
#   member_casual       n percentage
# 1        casual 1675390   35.67374
# 2        member 3021034   64.32626


# trip statistics grouped by user type (can be altered to group by other stuff)
trip_stats_by_membership <- real_trips_v2 %>%
  group_by(member_casual) %>%
  summarize(
    count = n(),
    mean = mean(trip_length, na.rm = TRUE),
    median = median(trip_length, na.rm = TRUE),
    min = min(trip_length, na.rm = TRUE),
    max = max(trip_length, na.rm = TRUE),
    sd = sd(trip_length, na.rm = TRUE)
  )

print(as.data.frame(trip_stats_by_membership))
#   member_casual   count     mean    median       min      max       sd
# 1        casual 1675390 24.57508 13.750000 0.5166667 11152.27 52.75348
# 2        member 3021034 12.79660  9.066667 0.5042667  1497.65 23.73554

# histogram of trip count by user type (use filter to change trip lengths, use position to be dodge or stack)
ggplot(real_trips_v2 %>% filter(trip_length < 100), aes(x = trip_length, fill = member_casual)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Trips by User Type",
       x = "Trip Length (mins)",
       y = "Trip Count") +
  theme_light() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1))

# histogram of trip count by day of week and user type
real_trips_v2 %>% 
  group_by(member_casual, day_of_week_started) %>%
  summarise(trip_count = n()) %>% 
  ggplot(aes(x = day_of_week_started, y = trip_count, fill = member_casual)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Trips by weekday and membership",
       x = "Day of the Week",
       y = "Trip Count") +
  theme_light() + 
  theme(axis.text.y = element_text(angle = 45, hjust = 1))

# histogram of trip length by day of week and user type
real_trips_v2 %>% 
  group_by(member_casual, day_of_week_started) %>%
  summarise(avg_trip_length = mean(trip_length)) %>% 
  ggplot(aes(x = day_of_week_started, y = avg_trip_length, fill = member_casual)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Trip Length by weekday and membership",
       x = "Day of the Week",
       y = "Trip Length") +
  theme_light() + 
  theme(axis.text.y = element_text(angle = 45, hjust = 1))