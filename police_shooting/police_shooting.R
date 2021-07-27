#import libraries
library(tidyverse)
library(lubridate)

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

#2015 had the most deaths 
shooting_dataset %>% 
  ggplot(mapping = aes(Year)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45))

#encoding & distribution 

