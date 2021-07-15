#load libraries
library(tidyverse)
library(lubridate)
library(modeest)

#load csv 
pet_food_customer <- read.csv("~/Downloads/pet_food_customer_orders.csv")

head(pet_food_customer)

#format date and create weekday,month and year col
pet_food_customer$date <- as.Date(pet_food_customer$order_payment_date)
pet_food_customer$weekday <- wday(pet_food_customer$date,label = TRUE, abbr = TRUE)
pet_food_customer$month <- format(as.Date(pet_food_customer$date), "%y %m")

#filter out 
customer_wet_food <- filter(pet_food_customer,pet_food_customer$wet_food_order_number > 1 )
#omit na rows
customer_wet_food <- drop_na(customer_wet_food)


#analysis
summary(customer_wet_food)

customer_wet_food %>%
  group_by(month) %>% 
  summarise(num_of_order = n())

#visual
customer_wet_food %>%
  group_by(month, customer_id) %>% 
  summarise(number_of_order = n()) %>% 
  ggplot(aes(x=month, y = number_of_order,fill=number_of_order)) + geom_bar(stat = "identity")
#weekday
customer_wet_food %>%
  group_by(weekday, customer_id) %>% 
  summarise(number_of_order = n()) %>% 
  ggplot(aes(x=weekday, y = number_of_order,fill=number_of_order)) + geom_bar(stat = "identity")

#how many reoccurring orders
# orders were mostly for small dogs
customer_wet_food %>% 
  group_by(pet_breed_size) %>% 
  summarise(num_of_order = n()) %>% 
  ggplot(aes(x=pet_breed_size,y=num_of_order),fill=num_of_order) + geom_bar(stat="identity")


customer_wet_food %>% 
  group_by(pet_life_stage_at_order) %>% 
  summarise(num_of_order = n()) %>% 
  ggplot(aes(x=pet_life_stage_at_order,y=num_of_order),fill=num_of_order) + geom_bar(stat="identity")
  
  
customer_wet_food %>% 
  group_by(gender) %>% 
  summarise(num_of_order = n()) %>% 
  ggplot(aes(x="",y=num_of_order,fill=gender))+geom_bar(stat = "identity", width = 1) + 
  coord_polar("y",start = 0)+
  theme_void()

#food type
#superpremium
customer_wet_food %>% 
  group_by(pet_food_tier) %>% 
  summarise(num_of_order = n())

customer_wet_food %>% 
  group_by(signup_promo) %>% 
  summarise(num = n())

#compare customer with wet_food_order
customer_wet_food %>% 
  group_by(signup_promo,wet_food_order_number) %>% 
  summarise(num=n()) %>% 
  ggplot(aes(x=wet_food_order_number,y=num,fill=signup_promo))+
  geom_col(position = "dodge")+ 
  scale_x_continuous(breaks = c(1:20))
#Null n Default 
#Search generic 

#pet food flavor
customer_wet_food %>% 
  group_by(pet_fav_flavour_list) %>% 
  summarise(num = n(),) %>% 
  filter(pet_fav_flavour_list != "") %>% 
  arrange(desc(num))

#health issues
customer_wet_food %>% 
  group_by(pet_health_issue_list) %>% 
  filter(pet_health_issue_list != "") %>% 
  summarise(num = n()) %>% 
  arrange(desc(num))

#pet alleric
customer_wet_food %>% 
  group_by(pet_allergen_list) %>% 
  filter(pet_allergen_list != "") %>% 
  summarise(num=n()) %>% 
  arrange(desc(num))
  
