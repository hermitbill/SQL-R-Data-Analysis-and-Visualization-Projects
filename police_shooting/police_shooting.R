#import libraries
library(tidyverse)
library(lubridate)
library(ggthemes)
library(tidyverse) # metapackage of all tidyverse packages

# ### Load the dataset.
shooting_dataset <- read_csv("Police Fatalities.csv")
head(shooting_dataset)

# ### A quick look at the first 6 values.

str(shooting_dataset)

# ### Checking columns format.

colSums(is.na(shooting_dataset))

# ### There are several missing entries in a couple columns.

shooting_dataset$Armed <- ifelse(is.na(shooting_dataset$Armed),"Unharmed",shooting_dataset$Armed )

# ### Replacing 'NA' values with 'Unaramerd' in the Armed column.

shooting_dataset$Age <- ifelse(is.na(shooting_dataset$Age), ave(shooting_dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),shooting_dataset$Age)

# ### Fill missing ages values with the average.

shooting_dataset$Age <- as.integer(shooting_dataset$Age)

#change date format
shooting_dataset$Date <- format(mdy(shooting_dataset$Date), "%Y/%m/%d")

# ### Format Age column to integer.

shooting_dataset$Date <- as.Date(shooting_dataset$Date)

# ### Format Date column.

#creating Year column 
shooting_dataset$Year <- format(shooting_dataset$Date, "%Y")

# ### Create Year column from Date column.

shooting_dataset$Weekday <-wday(shooting_dataset$Date, label = TRUE, abbr = TRUE)

# ### Create a Weekday column.

head(shooting_dataset)

summary(shooting_dataset)
#top 10 city 
# ### The top 10 cities with the highest deaths.

#gender
shooting_dataset %>%
  filter(!is.na(Gender))%>%
  ggplot(aes(Gender, fill=Gender))+
  geom_bar()+
  theme_clean()+
  theme(legend.position ="none")+ 
  labs(title="Gender vs Death Count", y = "Death Count ")

# ### We have more men than female deaths: 
# * **it can be that more men get killed than women.** 
# * **its possible that women weren't fully represented in the dataset.**
# * **I also ommited several missing values.**

#Race
shooting_dataset %>%
  filter(!is.na(Race))%>%
  ggplot(aes(Race, fill=Race))+
  geom_bar()+
  theme_clean()+
  theme(legend.position="none")+
  labs(title="Race vs Death Count", y = "Death Count")

# ### Whites have the highest death. 
# ### Remember I did omit over a thousand missing race values.

#state
shooting_dataset %>%
  ggplot(aes(State))+
  geom_bar(fill="blue")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="State vs Death Count", y = "Death Count")

# ### California has the highest death count.

#matter of death
shooting_dataset%>%
  ggplot(aes(Manner_of_death)) +
  theme_clean()+
  geom_bar(fill="blue")+
  labs(title="Manner of Death vs Death Count", y = "Death Count", x = "Manner of Death" )

#Weekday vs death count
shooting_dataset%>%
  ggplot(aes(Weekday))+
  theme_clean()+
  geom_bar(fill = 'blue')+
  labs(title="Weekday vs Death Count", y = "Death Count")

# ### Weekdays show no relationship to death count.

#the percentage of those who have a mental illness
x <- shooting_dataset%>%
  filter(Gender == "Male")%>%
  pull(Mental_illness)

prop.table(table(x))

# ### Majority of the people in the dataset have no mental illness.

#Death count vs Years 
shooting_dataset%>%
  group_by(Year)%>%
  summarise(Death.count=n())%>%
  ggplot(aes(Year,Death.count,group=1,col="red"))+
  geom_line()+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1 ), legend.position = "none")+
  labs(title="Year vs Death Count", y = "Death Count")

# * **2015 has the highest death count**
# * **2013 and 2014 have the same death count**
# * **a plunge in death from 2015 to 2016**

#taking a closer look at the death count vs Races during highlighted years
years = c(2013,2014,2015,2016)
shooting_dataset %>%
  filter(Year %in% years & !is.na(Race))%>%
  group_by(Race, Year)%>%
  ggplot(aes(Race, fill=Race))+
  geom_bar()+
  #theme_clean()+
  facet_wrap(.~Year)+ 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())+
  labs(title="Race per Year vs Death Count", y = "Death Count", x="")

#distribution in age
shooting_dataset %>%
  ggplot(aes(Age))+ 
  theme_clean()+
  geom_histogram(binwidth = 1, fill="blue", color="black")+
  labs(title="Age Distribution", y = "Number of people")

# There are 2 local modes

#age stats
shooting_dataset %>%
  group_by(Gender)%>%
  summarise(mean=mean(Age),sd= sd(Age))

#Male age distribution
params <- shooting_dataset %>%
  filter(Gender == "Male") %>%
  summarize(mean = mean(Age), sd = sd(Age))

shooting_dataset%>%
  ggplot(aes(sample = Age))+
  geom_qq(dparams = params)+
  geom_abline()+
  theme_clean()+
  labs(title='Male Age distribution')

params <- shooting_dataset %>%
  filter(Gender == "Female") %>%
  summarize(mean = mean(Age), sd = sd(Age))

#women
shooting_dataset%>%
  ggplot(aes(sample = Age))+
  geom_qq(dparams = params)+
  geom_abline()+
  theme_clean()+
  labs(title='Female Age distribution')


# Men and Women follow a some-what normal distribution

# age distribution between Genders
shooting_dataset %>%
  filter(!is.na(Gender))%>%
  ggplot(aes(Age))+ 
  geom_histogram(binwidth = 1, fill="blue")+
  facet_grid(.~Gender)+
  labs(title="Age Distribution between Genders", y = "Number of people")

# the distribution of Age btw Race 
shooting_dataset %>%
  filter(!is.na(Race))%>%
  group_by(Race, Age)%>%
  summarise(Age.count = n())%>%
  ggplot(aes(Race, Age, fill=Race))+
  geom_boxplot()+
  theme_clean()+
  theme(legend.position = "none")+
  labs(title="Age vs Race")


# Native and Other have the youngest death age.

# the distribution of Age btw Race & Gender
shooting_dataset %>%
  filter(!is.na(Race) & !is.na(Gender))%>%
  group_by(Race,Age,Gender)%>%
  summarise(Age.count = n())%>%
  ggplot(aes(Race,Age, fill=Race))+ 
  geom_boxplot()+
  facet_grid(.~Gender)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(title="Age vs Race between Genders")

# Women have a lower age average between races

#mental health 
shooting_dataset%>%
  filter(!is.na(Gender))%>%
  ggplot(aes(Mental_illness, fill= Mental_illness))+
  geom_bar()+
  facet_wrap(Year~Gender)+
  theme(axis.text.x = element_blank(), axis.ticks.x= element_blank()) +
  labs(title="Mental illness per Year vs Genders", y = "Death count", x="")

#Mental illness vs Race
shooting_dataset%>%
  #boxplot vs age vs mental_illness
  filter(!is.na(Race))%>%
  group_by(Age,Mental_illness,Race)%>%
  summarise(Mental.count = n())%>%
  ggplot(aes(Race,Age, fill=Race))+
  geom_boxplot()+
  facet_wrap(.~Mental_illness)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(title="Age vs Race vs Mental illness")
