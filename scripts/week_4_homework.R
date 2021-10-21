library(tidyverse)
surveys <- read.csv("data/portal_data_joined.csv")
summary(surveys)

head(survey <- filter(surveys, weight < 60 & weight >30 ))

biggest_critters <- surveys %>% 
  filter(.,!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  summarise(max_weight <- max(weight))

##arrange species and sex by weight descending
biggest_critters %>% arrange(desc( max_weight))

##where NA is by taxa

surveys %>% 
  filter(.,is.na(weight)) %>% 
  (taxa) %>% 
  tally() %>% 
  arrange(desc(n))

###5. remove NA row in weight and add column contain avg weight of each species + sex
surveys_avg_weight <- surveys %>% 
  filter(.,!is.na(weight)) %>% 
  group_by(species, sex) %>% 
  mutate(avg_weight <- mean(weight)) %>% 
  select(species, sex, weight, avg_weight)

####6. Above avg

surveys_avg_weight %>% 
  mutate(above_avg <- weight > avg_weight)
