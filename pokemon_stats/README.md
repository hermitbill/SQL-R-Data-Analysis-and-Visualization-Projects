# pokemon game stats

Had lots of fun with this dataset. I grew up playing, trading and collecting Pokemon. In the past few years I haven't had the time to really get back in to it. This dataset was fun to play around with. It help me see how much Pokemon had grown. 

## Table of contents 
* Importing dataset
* Cleaning and processing the dataset
* Building new subsets (legendary Pokemon, mystical Pokemon, Ultra beast)
* Ranking Pokemon on different metrics 
* Data visualizations

## Code and Resourse Used 
**Languages**: Rstudio and SQL\
**Packages**: tidyverse, sqldf, caTools\
**Data source**: [kaggle](https://www.kaggle.com/shubhamchambhare/pokemons-and-there-stats)
**Pokemon resources**: Bulbagarden, Serebii  

## Data

pokemon_dataset: Contains all Pokemon including their different forms eg. Mega Evolution, Gigantamax. Below are the different Pokemon metrics used in the dataset.
1. Total
2. Hp
3. Attack
4. Defense
5. Special Attack
6. Special Defense
7. Speed

### pokemon subsets
legendary pokemon - a group of incredibly rare and often very powerful Pokémon, generally featured prominently in the legends and myths of the Pokemon world.

**Mythical Pokemon** - a related but separate group of Pokemon, which are usually event-exclusive like Pokemon center events, Gamestop events etc.

**Ultra Beast** - are a group of extra dimensional Pokémon originating from Ultra Space. 

**Mega Evolution** -  is a temporary transformation introduced in Generation VI that affects certain Pokemon. Mega Evolution requires the player to hold a Key Stone.  

**Gigantamax** - is a temporary transformation that increases the size of the Pokemon as well as Hp points.

## Insights
* Very few Pokemon have Hp bars over 150 points 
* All Pokemon metrics are statically significant to the Total 
* Top 10 Pokemon in each metric 
!(boxplots.png)
!(multiple_linear_regression.png)
