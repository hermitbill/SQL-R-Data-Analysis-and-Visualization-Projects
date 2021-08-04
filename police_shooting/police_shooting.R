#import libraries
library(tidyverse)
library(lubridate)
library(ggthemes)

#upload dataset
shooting_dataset <- read_csv("Police Fatalities.csv")

#a small view of the dataset
head(shooting_dataset)

#check for missing values 
colSums(is.na(shooting_dataset))

#replace NA value with unharmed in the armed col
shooting_dataset$Armed <- ifelse(is.na(shooting_dataset$Armed),"Unarmed", shooting_dataset$Armed)

#average age in col
shooting_dataset$Age <- ifelse(is.na(shooting_dataset$Age),
                               ave(shooting_dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                               shooting_dataset$Age)
#convert to integer
shooting_dataset$Age <- as.integer(shooting_dataset$Age)
colSums(is.na(shooting_dataset))

#removing gender and race missing values
shooting_dataset <- na.omit(shooting_dataset)

#-----------
rate <- state_by_race$n / sum(state_by_race$n) * 100
state_by_race$rate <- rate
boxplot(rate~Race, data=state_by_race)

state_by_race$State[which.max(state_by_race$rate)]
state_by_race$State[which.min(state_by_race$rate)]

ca <- state_by_race %>% 
  filter(State == "CA")

ak <- state_by_race %>% 
  filter(State == "AK")

barplot(rate~Race, data = ca)
hist(state_by_race$rate)
#----------

#quick analytically 
summary(shooting_dataset)

#format date col
shooting_dataset$Date <-format(mdy(shooting_dataset$Date), "%Y/%m/%d")
shooting_dataset$Date <- as.Date(shooting_dataset$Date)
shooting_dataset$Year <- format(shooting_dataset$Date, "%Y")
shooting_dataset$Weekday <- wday(shooting_dataset$Date, label = TRUE, abbr = TRUE )

#quick visuals 
shooting_dataset %>% 
  ggplot(mapping = aes(Gender)) + geom_bar()
#we don't have an equal balance of genders, it might be that males get shoot more.

#race
shooting_dataset %>% 
  ggplot(mapping = aes(Race)) + geom_bar()

# states with highest crime 
shooting_dataset %>% 
  ggplot(mapping = aes(State)) + geom_bar() + 
  theme(axis.text.x = element_text(angle=45) )
  
#the matter of death 
shooting_dataset %>% 
  ggplot(mapping = aes(Manner_of_death)) + geom_bar()

#weekday 
shooting_dataset %>% 
  ggplot(mapping = aes(Weekday)) + geom_bar()
#no relationship

#2015 had the most deaths Time series 
shooting_dataset %>%
  group_by(Year) %>% 
  summarise(Death.count = n()) %>% 
  ggplot(aes(Year, Death.count)) + geom_point()+
  theme(axis.text.x = element_text(angle = 45))

shooting_dataset %>% 
  group_by(City) %>% 
  summarise(City.count = n()) %>% 
  arrange(desc(City.count)) %>% top_n(10)
  

#encoding & distribution 
hist(shooting_dataset$Age)

#Over all age distribution ( not a good representation)
shooting_dataset %>% 
  ggplot(mapping = aes(Age)) + geom_histogram()
#multiple shoulders 

#rate 
#male
x <- filter(shooting_dataset, Gender == "Male")
x %>% ggplot(data = x,mapping = aes(Age)) + geom_histogram(binwidth = 1, fill="lightblue",col="black")+
  theme_clean() + labs(y="Number of Men")


#female
x <- filter(shooting_dataset, Gender == "Female")
x %>% ggplot(data = x,mapping = aes(Age)) + geom_histogram()
#-----
params <- shooting_dataset %>% 
  filter(Gender == "Male") %>%
  summarize(mean=mean(Age), sd = sd(Age)) 

table(params)

shooting_dataset %>%
  ggplot(aes(sample = Age)) + 
  geom_qq(dparams = params)+
  geom_abline()
#-----

#scale function
#if it follows a normal distribution 
shooting_dataset %>% 
  filter(Gender == 'Female') %>% 
  ggplot(aes(sample = scale(Age)))+
  geom_qq()+
  geom_abline()
#-----

shooting_dataset %>% 
  group_by(Gender) %>% 
  summarise(mean = mean(Age), sd = sd(Age))

shooting_dataset %>% 
  filter(Gender == "Male") %>% 
  group_by(Race) %>% 
  summarise(mean = mean(Age), min = min(Age), max = max(Age), median = median(Age))

library(ggrepel)
  
#distribution of age within genders
index <- shooting_dataset$Gender == "Male"
x <- shooting_dataset$Age[index]
hist(x)
x

#distribution of age within genders
index <- shooting_dataset$Gender == "Female"
x <- shooting_dataset$Age[index]
hist(x)

# the different race in deaths btw genders 
ggplot(data=shooting_dataset, mapping = aes(Race))+
  geom_bar(aes(fill=Gender), position = "dodge")

#comparing the age that died to years 
diff <- filter(shooting_dataset,Year%in%c(2000,2016)) %>%
  group_by(Race,Age,Year) %>% 
  summarise(Race.count = n())

diff %>% 
  ggplot(aes(Race,Race.count, fill = Race))+
  geom_boxplot()+
  theme(legend.position = 'none')

shooting_dataset %>% 
  group_by(Race, Year) %>% 
  summarise(Race.count = n()) %>% 
  ggplot(aes(Race, Race.count, fill= Race))+
  geom_boxplot()+
  theme_clean()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#-------
#returns max vaule
diff$Age[which.max(diff$Race.count)]
diff$Race[which.max(diff$Race.count)]
diff$Year[which.max(diff$Race.count)]
max(diff$Race.count)

diff %>% 
  select(Race,Age,Year,Race.count) %>% 
  filter(Race.count == max(diff$Race.count))
#-------
diff %>% ggplot(aes(Age,Race.count, col = Race))+
  geom_point()+
  facet_grid(.~Year)

years <- c(2000,2005,2010,2011,2012,2013,2014,2015,2016)
race <- c("Black","White")

shooting_dataset %>% 
  filter(Year %in% years & Race %in% race) %>% 
  group_by(Race,Age,Year) %>% 
  summarise(Race.count = n()) %>% 
  ggplot(aes(Age,Race.count, col = Race))+
  geom_point()+
  facet_wrap(~Year)

#white v black 
shooting_dataset %>% 
  filter(Race %in% race) %>% 
  group_by(Year,Race) %>%   
  summarise(Year.count = n()) %>% 
  ggplot(aes(Year,Year.count, group=Race, col=Race)) + 
  geom_line()


#box plot 
shooting_dataset %>% 
  group_by(Race) %>% 
  
  
  



