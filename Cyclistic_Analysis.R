# Load necessary library to project

library(rstudioapi)
library(tidyverse)
library(lubridate)
library(ggplot2)


# Define working directory 
script_dir = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_dir))
getwd()
setwd("./Divvy_Data")

# Combine all csv files in the folder
all_trips <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))


colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)
table(all_trips$member_casual)
table(is.na(all_trips))
rowSums(is.na(all_trips))
sum(complete.cases(all_trips))
sum(!complete.cases(all_trips))


# Add and format more variables (columns)
all_trips$date <- as.Date(all_trips$started_at)
all_trips$hour <- format(all_trips$started_at, "%H")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$month_year <- format(as.Date(all_trips$date), "%b-%y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

str(all_trips_v2)
View(all_trips)
View(all_trips_v2)


# Cleaning up Head Quarter Station and negative ride_length then drop all NA observation ()
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]
all_trips_v2 <- all_trips_v2 %>%  drop_na()

all_trips_v2$ride_legth_HMS <- seconds_to_period(all_trips_v2$ride_length)
# Descriptive Analysis
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, 
          FUN = function(x) c(mean = mean(x), median = median(x),
                              max = max(x), min = min(x)))

# Re-order day of week
all_trips_v2$day_of_week <-ordered(all_trips_v2$day_of_week, levels = c("Sunday","Monday",
                                                                        "Tuesday", "Wednesday",
                                                                        "Thursday", "Friday",
                                                                        "Saturday"))

# Pivot table to see member/casual average ride length (duration) in each day of the week
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# Analyse ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday)

pivot_trips <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month, year, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  arrange(member_casual, weekday)

View(pivot_trips)
# Visualise the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualise average duration by rider type on multiple month

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month, year, weekday, hour) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = avg_duration_min, fill = member_casual)) +
  geom_col(position = "dodge") + facet_wrap(~ month ~year) 

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, hour, month) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  ggplot(aes(x = hour, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + facet_wrap(~month) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title="Number of rides for each rider types in hour", 
       subtitle="Historical trip data during Apr 2022 to Mar 2023", 
       caption = "Data collected by Motivate International Inc")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, hour, month, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  ggplot(aes(x = weekday, y = avg_duration_min, fill = member_casual)) +
  geom_boxplot(outlier.shape = 1) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title="Average duration (minutes) for each rider types in weekday", 
       subtitle="Historical trip data during Apr 2022 to Mar 2023", 
       caption = "Data collected by Motivate International Inc")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, hour, month, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  ggplot(aes(x = month, y = avg_duration_min, fill = member_casual)) +
  geom_boxplot(outlier.shape = 1) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title="Average duration (minutes) for each rider types in monhth", 
       subtitle="Historical trip data during Apr 2022 to Mar 2023", 
       caption = "Data collected by Motivate International Inc")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, hour, month, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_boxplot(outlier.shape = 1) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title="Number of rides for each rider types in monhth", 
       subtitle="Historical trip data during Apr 2022 to Mar 2023", 
       caption = "Data collected by Motivate International Inc")
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, start_station_name) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>%
  arrange(desc(number_of_rides)) %>%  slice (1:10) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  ggplot(aes(x= number_of_rides, y = start_station_name, fill=member_casual)) +
  geom_bar(stat = "identity") +facet_wrap(~member_casual) +
  labs(title="Top10 Stations (number of rides) for each rider types to start riding", 
       subtitle="Historical trip data during Apr 2022 to Mar 2023", 
       caption = "Data collected by Motivate International Inc")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month, year, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 
  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = hour(seconds_to_period(total_duration))) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = avg_duration_min, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            total_duration = sum(ride_length)) %>% 

  mutate(avg_duration_min = minute(seconds_to_period(average_duration)),
         total_duration_hrs = round(total_duration/3600)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = avg_duration_min, fill = member_casual)) +
  geom_col(position = "dodge") + facet_wrap(~month)

View(all_trips_v2)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, rideable_type, month, month_year, weekday, start_station_name) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length), total_duration = sum(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  mutate(avg_duration_hms = seconds_to_period(average_duration),total_duration_hms = seconds_to_period(total_duration)) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "stack") + facet_wrap(~ month) 


# Export to csv for further analysis
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week , FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
View(all_trips_v2)
View(counts)
