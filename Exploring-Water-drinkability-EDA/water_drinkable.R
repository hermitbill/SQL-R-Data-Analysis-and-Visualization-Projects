#import libraries
library(tidyverse)
library(gridExtra)
library(corrplot)

#load data set
water_potability <- read.csv("~/Downloads/water_potability.csv")

#checking data set
head(water_potability)

#count rows n col & data types 
str(water_potability)

#count missing data
colSums(is.na(water_potability))

#data set with NA removes 
new_water_potability <- na.omit(water_potability)

#count unique values to makes sure no incorrect entries 
unique(new_water_potability$Potability)

#summary of the data set
summary(new_water_potability)

#encoding the potability 
summary(new_water_potability$Potability)
new_water_potability$Potability <- factor(new_water_potability$Potability)

#check for bias
new_water_potability %>% 
  group_by(Potability) %>% 
  ggplot(mapping = aes(x=Potability,fill=Potability))+
  geom_bar()

#plot to see any initial relationships
plot(new_water_potability)

#distribution of potability on independent variable 
#continuous variable
one <-new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=ph))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Potability" )

two <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Hardness))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Hardnesss" )

three <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Chloramines))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Chloramines" )

four <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Sulfate))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Sulfate" )

five <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Conductivity))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Conductivity" )

six <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Organic_carbon))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Organic carbon" )

seven <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Trihalomethanes))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Trihalomethanes" )

eight <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Turbidity))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Turbidity" )

nine <- new_water_potability %>% 
  ggplot(data=new_water_potability, mapping = aes(x=Solids))+
  geom_histogram(mapping = aes(fill=Potability))+
  labs(title = "distribution on Solids" )

grid.arrange(one,two,three,four,five,six,seven,eight,nine)


#variations & covariation 
ph_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=ph,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

hardness_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=Hardness,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

chloramines_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=Chloramines,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

sulfate_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=Sulfate,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

conductivity_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=Conductivity,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

organic_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=Organic_carbon,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

trihalomethanes_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=Trihalomethanes,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

turbidity_boxplot<- new_water_potability %>% 
  ggplot(mapping = aes(x=Potability,y=Turbidity,group=Potability))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 1)

all_boxplot <- grid.arrange(ph_boxplot,hardness_boxplot,solids_boxplot,chloramines_boxplot,sulfate_boxplot,conductivity_boxplot,organic_boxplot,trihalomethanes_boxplot,turbidity_boxplot)

#correlation 
new_water_potability_without <- new_water_potability[,c(-10)]
c_water<-cor(new_water_potability_without)

#visual correlation
cor_water <- corrplot(c_water,method = "circle", type = "lower", addCoef.col = "black",
         number.cex = 0.7)



