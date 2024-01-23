
# Week 2: Tuesday

## talk about .Rproj and computer organization

## tidyverse 
#install.packages("tidyverse")
library(tidyverse)
#install.packages("Lahman")
library(Lahman)
#install.packages("lubridate")
library(lubridate)
#install.packages("palmerpenguins")
library(palmerpenguins)

## tibble
as_tibble(iris)
as_tibble(penguins)
as_tibble(Batting)


## dplyr 

### select

### filter 

### arrange 

### mutate 

### group_by/summarize 

### left_join 

### pipe operator



## readr
era_adjusted_HR = read_csv(file = "stat528sp24/notes/1-coding/era_adjusted_HR.csv")
era_adjusted_HR

## examples 

### palmer penguins
penguins %>% 
  group_by(species) %>% 
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

### career home runs (arranged from high to low; add final game)

### career batting average (since 1947; minimum 3000 AB)


## ggplot2 

### plot points; colored by species
ggplot(penguins) + 
  aes(x = body_mass_g, y = flipper_length_mm, col = species) + 
  geom_point()

### career home runs by final game (join in People)


### career batting average since 1947 by final game 


### career home runs vs era-adjusted home runs by final game





#### Notes: 
career_HR = Batting %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR), final_year = max(yearID), AB = sum(AB)) %>% 
  filter(HR > 10, AB > 1000)

career_era_adjusted_HR = era_adjusted_HR %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR), final_year = max(year), AB = sum(AB)) %>% 
  filter(HR > 10, AB > 1000)

playerIDs = intersect(career_HR$playerID, 
                      career_era_adjusted_HR$playerID)

ggplot(career_HR %>% filter(playerID %in% playerIDs)) + 
  aes(x = final_year, y = HR) + 
  geom_point() + 
  geom_smooth()

ggplot(career_era_adjusted_HR %>% filter(playerID %in% playerIDs)) + 
  aes(x = final_year, y = HR) + 
  geom_point() + 
  geom_smooth()

?bind_rows
dat = bind_rows(career_HR %>% filter(playerID %in% playerIDs), 
                career_era_adjusted_HR %>% filter(playerID %in% playerIDs), 
                .id = "id") %>% 
  mutate(id = ifelse(id == 1, "original", "era-adjusted"))

ggplot(dat) +
  aes(x = final_year, y = HR, col = id) + 
  geom_point(col = "black") + 
  geom_smooth()
  
ggplot(dat) +
  aes(x = final_year, y = HR, col = id) + 
  facet_wrap(facets = ~ id) + 
  geom_point(col = "black") + 
  geom_smooth()


## https://daviddalpiaz.org/posts/moneyball-in-r/

