#install packages and import libraries
install.packages("tidyverse")
library(tidyverse)
library(lubridate)

#load csv files
jun_2020 <- read_csv("~/Downloads/trip_data/202005-divvy-tripdata.csv")
jul_2020 <- read_csv("~/Downloads/trip_data/202006-divvy-tripdata.csv")
aug_2020 <- read_csv("~/Downloads/trip_data/202007-divvy-tripdata.csv")
sep_2020 <- read_csv("~/Downloads/trip_data/202008-divvy-tripdata.csv")
oct_2020 <- read_csv("~/Downloads/trip_data/202009-divvy-tripdata.csv")
nov_2020 <- read_csv("~/Downloads/trip_data/202010-divvy-tripdata.csv")
dec_2020 <- read_csv("~/Downloads/trip_data/202011-divvy-tripdata.csv")
jan_2021 <- read_csv("~/Downloads/trip_data/202012-divvy-tripdata.csv")
feb_2021 <- read_csv("~/Downloads/trip_data/202101-divvy-tripdata.csv")
mar_2021 <- read_csv("~/Downloads/trip_data/202102-divvy-tripdata.csv")
apr_2021 <- read_csv("~/Downloads/trip_data/202103-divvy-tripdata.csv")
may_2021 <- read_csv("~/Downloads/trip_data/202104-divvy-tripdata.csv")

#format all start_station_id and end_station_id 
jun_2020 <- jun_2020 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

jul_2020 <- jul_2020 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

aug_2020 <- aug_2020 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

sep_2020 <- sep_2020 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

oct_2020 <- oct_2020 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

nov_2020 <- nov_2020 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

dec_2020 <- dec_2020 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

jan_2021 <- jan_2021 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

feb_2021 <- feb_2021 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

mar_2021 <- mar_2021 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

apr_2021 <- apr_2021 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))

may_2021 <- may_2021 %>% 
  mutate(start_station_id = as.numeric(as.character(start_station_id)),end_station_id = as.numeric(as.character(end_station_id)))


#create 1 table
all_trip_day <- bind_rows(jun_2020,jul_2020,aug_2020,sep_2020,oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,apr_2021,may_2021)

#calculate ride length
all_trip_day <- all_trip_day %>% 
  mutate(ride_length = difftime(ended_at,started_at,units = "mins"))
#all_trip_day$ride_length <- as.numeric(as.character(all_trip_day$ride_length))


#format date
all_trip_day$date <- as.Date(all_trip_day$started_at)
#all_trip_day["weekdays"] <- format(as.Date(all_trip_day$date),"%A")
all_trip_day$weekdays <- wday(all_trip_day$date,label = TRUE,abbr = TRUE)
all_trip_day["months"] <- format(as.Date(all_trip_day$date),"%y %m")

#filter out test data 
data_sample <- filter(all_trip_day,all_trip_day$start_station_name != "TEST"
                      |all_trip_day$start_station_name != "Test"
                      |all_trip_day$start_station_name != "test"
                      ) #not sure if '>' or '<'
data_sample <- filter(data_sample,data_sample$ride_length > 0)

data_sample <- drop_na(data_sample)

x <- data_sample %>% 
  filter(data_sample$ride_length < 0)

#Analyze
#max ride length for members 
data_sample %>% 
  group_by(member_casual,weekdays) %>% 
  summarise(num_of_rides = n()) %>% 
  ggplot(aes(x=weekdays,y=num_of_rides,fill=member_casual)) + geom_col(color = "Black", position = "dodge") +
  scale_fill_manual(values = c("#552583","#FDB927"))+
  scale_y_continuous(breaks = c(0,100000,200000,300000,400000),labels = c("0","100k","200k","300k","400k"))+
  labs(title = "Number of rides per week", y = "Number of Rides(thousands)") 


#prefer biketype
data_sample %>% 
  group_by(member_casual,rideable_type) %>% 
  summarise(num_of_rides = n()) %>% 
  ggplot(aes(x=rideable_type, y=num_of_rides,fill=member_casual)) +
  scale_fill_manual(values = c("#552583","#FDB927"))+
  geom_col(color="Black",position = "dodge")
  #y axisd

#average ride length on each day 
data_sample %>% 
  group_by(member_casual,weekdays) %>% 
  summarise(avg_ride_lenght = mean(ride_length)) %>% 
  ggplot(aes(x=weekdays, y = avg_ride_lenght, fill = member_casual)) + 
  geom_col(color = "Black", position = "dodge")+
  scale_fill_manual(values = c("#552583","#FDB927"))+
  labs(title = "Average ride lenght per Weekday", y ="Average Ride Lenght")

#Number of rides each month
data_sample %>% 
  group_by(member_casual,months) %>%
  arrange(months) %>% 
  summarise(num_of_rides = n()) %>% 
  ggplot(aes(x=months,y=num_of_rides, fill=member_casual))+
  geom_col(color="Black", position = "dodge")+
  scale_y_continuous(breaks = c(0,100000,200000,300000,400000),labels = c("0","100k","200k","300k","400k"))+
  scale_x_discrete(labels = c("May-20", "Jun-20", "Jul-20", "Aug-20","Sep-20","Oct-20","Nov-20","Dec-20","Jan-21","Feb-21","Mar-21","Apr-21"))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#552583","#FDB927"))+
  labs(title = "Number of rides per month", y = "Number of Rides" )
  




