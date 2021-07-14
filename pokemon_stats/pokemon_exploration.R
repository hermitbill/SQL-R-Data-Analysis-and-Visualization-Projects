library(sqldf)
library(tidyverse)

#cleaning

#import database
pokemon_dataset <- read_csv("pokemon.csv")

#check for missing values 
sqldf("select count(*) from pokemon_dataset where null")

#Pokemon different forms
mega_pokmon <- sqldf("SELECT * FROM pokemon_dataset WHERE Name LIKE '%Mega%' GROUP BY Name ")

#all Pokemon no forms 
all_non_stage_pokemon <- sqldf("SELECT * FROM pokemon_dataset WHERE Name NOT IN 
                               (SELECT Name FROM pokemon_dataset WHERE Name LIKE '%Mega%')")

#max and min ppokemon 
sqldf("SELECT Name, MAX(Total) as max FROM all_non_stage_pokemon")
sqldf("SELECT Name, MIN(Total) as min FROM all_non_stage_pokemon")

sqldf("SELECT Name, Total FROM all_non_stage_pokemon ORDER BY Total DESC LIMIT 50")

#List of Legendary,mystic,ultrabeast use create table 
legendary_pokemon <- data.frame(Name = c("Articuno", "Zapdos","Moltres","Mewtwo","Suicune",
                        "Entei","Raikou","Ho-oh","Lugia","Latias","Latios","Regirock",
                       "Regice","Registeel","Groudon","Kyogre","Rayquaza","Uxie","Mesprit",
                       "Azelf","Heatran","Regigigas","Cresselia","Dialga","Palkia","Giratina",
                       "Tornadus","Thundurus","Landorus","Cobalion","Terrakion","Virizion",
                       "Reshiram","Zekrom","Kyurem","Xerneas","Yveltal","Zygarde","Tapu Bulu",
                       "Tapu Koko","Tapu Lele","Tapu Fini","Type: Null","Silvally","Cosmog","Cosmoem",
                       "Solgaleo","Lunala","Necrozma","Zacian","Zamazenta","Regieleki","Regidrago","Kubfu","Calyrex","Glastrier ","Spectrier","Urshifu","Eternatus"))

mystical_pokemon <- data.frame(Name = c("Mew","Celebi","Jirachi","Deoxys","Manaphy","Phione","Darkrai","Shaymin","Arceus","Victini","Keldeo",
                                        "Meloetta","Genesect","Diancie","Hoopa","Volcanion","Megearna","Marshadow","Zeraora","Meltan","Melmetal","Zarude"))

ultra_beast <- data.frame(Name = c("Nihilego","Buzzwole","Pheromosa","Xurkitree","Celesteela","Kartana",
                                   "Guzzlord","Poipole","Naganadel","Stakataka","Blacephalon"))

#create datasets 
legendary_pokemon_dataset <- sqldf("SELECT * FROM all_non_stage_pokemon WHERE Name IN legendary_pokemon ")
ultra_beast_dataset <- sqldf("SELECT * FROM all_non_stage_pokemon WHERE Name IN ultra_beast")
mystical_pokemon_dataset <- sqldf("SELECT * FROM all_non_stage_pokemon WHERE Name IN mystical_pokemon")

basic_non_legendary<- sqldf("SELECT * FROM all_non_stage_pokemon WHERE Name NOT IN (SELECT Name FROM legendary_pokemon )")
basic_ultra <- sqldf("SELECT * FROM basic_non_legendary WHERE Name NOT IN (SELECT Name FROM mystical_pokemon)")
common_set <- sqldf("SELECT * FROM basic_ultra WHERE Name NOT IN (SELECT Name FROM ultra_beast)")


#top 10
top_attacker <- sqldf("SELECT * FROM common_set ORDER BY Attack DESC LIMIT 10")
top_def <- sqldf("SELECT * FROM common_set ORDER BY Defence DESC LIMIT 10")
top_spe_att <- sqldf("SELECT * FROM common_set ORDER BY SP_Attack DESC LIMIT 10")
top_sp_def <- sqldf("SELECT * FROM common_set ORDER BY Sp_defence DESC LIMIT 10")
top_speed <-sqldf("SELECT * FROM common_set ORDER BY Speed DESC LIMIT 10")
top_hp <- sqldf("SELECT * FROM common_set ORDER BY Hp DESC LIMIT 10")

#distribution 
summary(all_non_stage_pokemon)
#set parameters for plot space
par(mfrow = c(2,3))

boxplot(all_non_stage_pokemon$Attack, main = "Distribution of Attack in Pokemon", xlab = "Pokemon", ylab = "Attack", col="khaki" )
boxplot(all_non_stage_pokemon$HP,main = "Distribution of Hp in Pokemon", xlab = "pokemon", ylab = "Hp", col="red1")
boxplot(all_non_stage_pokemon$Defence,main = "Distribution of Defence in Pokemon", xlab = "Pokemon", ylab = "Defence")
boxplot(all_non_stage_pokemon$Sp_attack,main = "Distribution of Special Attack in Pokemon", xlab = "Pokemon", ylab = "Special Attack", col="azure2")
boxplot(all_non_stage_pokemon$Sp_defence,main = "Distribution of Special Defence in Pokemon", xlab = "pokemon", ylab = "Special Defence", col="burlywood")
boxplot(all_non_stage_pokemon$Total,main = "Distribution of Total Attributes in Pokemon", xlab = "pokemon", ylab = "Sum of Attributes", col="dimgrey")


#correlation
#remove name which will not be used
new_dataset <- all_non_stage_pokemon[2:8]

library(caTools)
set.seed(123)
split <- sample.split(new_dataset$Total, SplitRatio = 2/3)
training_set <- subset(new_dataset, split == TRUE)
test_set <- subset(new_dataset, split == FALSE)

#Fitting multiple linear regression to Training set
regresssor <- lm(formula = Total ~.,
                 data = training_set)

y_pred <- predict(regresssor, newdata = test_set)







