###Cyclistic_Bike_Share_Project###

#The purpose of this project is to combine downloaded data into a single dataframe and then conduct simple analysis to help answer the question " How do annual members and casual riders use Cyclistic bikes differently?"#

#INSTALL REQUIRED PACKAGES#
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(ggplot2)

#UPLOAD DATASETS#
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")


#REMOVE ERRORS IN DATA AND COMBINE INTO A SINGLE FILE#
#Compare column names
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)



#Rename columns to show consistency with q1_2020 columns
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))



#Inspect the dataframes
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)



#Convert ride_id and rideable_type to characters
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 



#Stack individual quarter's data frames into on data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)



#Remove lat, long, birthyear, and gender fields as this data was dropped at the start of 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))


#CLEAN AND ADD DATA TO PREPARE FOR ANALYSIS#
#Inspect the new table
colnames(all_trips) 
nrow(all_trips) 
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)


#In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual" to make the dataframe consistent
#Start by seeing how many observations fall under each usertype
table(all_trips$member_casual)


#Reassign the values
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))


#Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

#Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Add a "ride_length" calculation to all_trips(in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#Inspect the structure of the columns
str(all_trips)


#Convert "ride_length" from Factor to numeric to give us the ability to run calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by ride_length was negative
#Create a new version of dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


#CONDUCT DESCRIPTIVE ANALYSIS
#Descriptive analysis on ride_length(all figures in seconds)
mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length)

#Condense the findings to one line
summary(all_trips_v2$ride_length)


#Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


#View the average ride time by each day for members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#Fix the order of the days of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Rerun the average ride time by each day for members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()						 
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)	


#Visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


#Create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'C:\\Users\\Shanique\\Documents\\avg_ride_length1.csv')
































