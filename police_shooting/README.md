## Data Summary 
This dataset aims to provide insight into individuals who were killed during alteractions with police. It includes information on their age, race, mental health status, weapons they were armed with, and if they were fleeing.

## Libraries 
*tidyverse
*lubridate
*ggthemes

## Data source 
This dataset comes from https://data.world/awram/us-police-involved-fatalities.

## insights 
### We have more men than female deaths: 
* **it can be that more men get killed than women.** 
* **its possible that women weren't fully represented in the dataset.**
* **I also omitted several missing values.**

![](img/gender_v_death.png)

### Race 
**Whites have the highest death. 
**Remember I did omit over a thousand missing race values.**

![](img/race_v_death.png)

### States
![](img/state_v_death.png)
**California has the highest death count.**

### Weekdays 
**Weekdays show no relationship to death count.**
![](weeday.png)

### Year
![](img/year_v_death.png)
* **2015 has the highest death count**
* **2013 and 2014 have the same death count**
* **a plunge in death from 2015 to 2016**

### Manner of Death

### Age Distribution 
![](img/age_distribution.png)

**There are 2 local modes**

![](img/female_distr.png)
![](img/male_age_dis.png)
![](img/men&women_age.png)
![](img/avg_v_gender_race.png)
![](img/race_per_year.png)

*Native and Other have the youngest death age.
*Women have a lower age average between races.

## Suggestions 
* **There are several missing values for a couple columns, I can't say for sure my conclusion are valid especially during this period of BLM movement. But I do believe my analysis with the present data holds water.**
* **The state population would be a nice addition to the dataset a so calculating the death rate would be a better metric to measure than death count.**