## Google-Data-Analytics-Capstone-Project
This case study is the Capstone Project of Google Data Analytics Professional Certificate. The 6 steps of Data Analysis (APPASA) is used to present this analysis.
1. **A**sk
2. **P**repare
3. **P**rocess
4. **A**nalyse
5. **S**hare
6. **A**ct

### Dataset
Cyclistic's historical trip data has been used for analyse and identify trends. Noted that the datasets have a different name becuase Cyclistic is a fictional company.
The data has been made available by Motivate International Inc. under this [license](https://ride.divvybikes.com/data-license-agreement)

### Background
Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet about 6,000 bicycles that are geotracked and locked into a network of 600+ stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

#### Phase1: Ask
Three questions will guide the future marketing program:
1. How do annual members and casual riders use Cyclsitic bike differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

Moreno has assigned you the first question to answer: **How do annual members and casual riders use Cyclistic bikes differently?**

#### Business Task

Stakeholders would like to convert casual riders into annual mmembers as conclusion that annual members are much more profitable than casual riders.

**Primary stakeholders:** The director of marketing and Cyclistic executive team


**Secondary stakeholders:** Cyclistic marketing analytics team

**Problem:** Maximizing annual memberships.

**Insights:** Understanding how annual members and casual ridres use the bikes differenyly, identifying factors that motivate casual ridersto become members.

**Business Decisions:** Developing targeted marketing strategies to encoruage casual riders to become members, offering incentives or diccounts to encourage casual riders to upgrade to annual memberships, adjusting pricing plans to make annual memberships more attractive.

The analysis would entail identifying the differences and similarities in usage patterns, such as the durations of rides, the frequency of rides, the distance traveled and the time of day when the bikes are used.


#### Phase2: Prepare

Cyclistic's historical trip data to analyse and identify trends have been collected from April 2022 to March 2023 from [here](https://divvy-tripdata.s3.amazonaws.com/index.html)

The dataset has been orginised on monthly basis (previous year they were organised on quarterly basis)

The dataset has been identified based on **ROCCC**

**R**eliable - Yes, these dataset is widely used for Capstone project for Google course under [license](https://ride.divvybikes.com/data-license-agreement)

**O**riginal - Yes, these historical dataset has been downloaded from [here](https://divvy-tripdata.s3.amazonaws.com/index.html)

**C**Comprehensive - Yes, as per a case study, the data collected for previous 12 months from the time this case study begins (April 2022 to March 2023)

**C**urrent - Yes, the data is up-to last month as per a case study begins

**C**ited - Yes, this dataset has been made available by Motivate International Inc. under this [license](https://ride.divvybikes.com/data-license-agreement)

The dataset consists of 12 CSV files (each for a month) with 13 identical columns and more than 5 million rows.

#### Phase3: Process

For this process, I chose excel to take a first look on each file to check if columns and names were identical then used RStudio (R Programming) to combine and clean the data

##### Findings and Cleaning
1. All twelve (12) files have identical variables (columns)
2. Six(6) variables (columns) have been added
3. There are ninety-nine (99) observations (rows) having negative ride_length values to be filtered out
4. There are over one million observations (rows) having "NA" values to be filtered out

```r
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

# Add and format more variables (columns)
all_trips$date <- as.Date(all_trips$started_at)
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

# Cleaning up Head Quarter Station and negative ride_length then drop all NA observation (rows)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]
all_trips_v2 <- all_trips_v2 %>%  drop_na()
# Cleaning up Head Quarter Station and negative ride_length then drop all NA observation (rows)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]
all_trips_v2 <- all_trips_v2 %>%  drop_na()

```
#### Phase4: Analyse

Perform Descriptive Analysis to see general statistics
```r
# Descriptive Analysis

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
                                                                     
```

```r
# Analyse ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday)


```

```r
# Export to csv for further analysis
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week , FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
```
#### Phase5: Share
```r
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
  group_by(member_casual, rideable_type, month, weekday, start_station_name) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "stack") + facet_wrap(~ month)
```
#### Phase6: Act
