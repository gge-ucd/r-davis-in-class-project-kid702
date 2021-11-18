library(tidyverse)
library(ggplot2)

gapminder <- read_csv("https://gge-ucd.github.io/R-DAVIS/data/gapminder.csv")


new_data <- gapminder %>% filter(year %in% c(2002,2007)) %>%
  pivot_wider(id_cols = c(country,continent),names_from = year,values_from = pop) %>%
  mutate(popDiff = `2007`-`2002`) %>% ## `` is to call columns value
  filter(continent != 'Oceania')


ggplot(data =new_data) + facet_wrap(~continent,scales = 'free') +
  geom_bar(aes(x = reorder(country,popDiff),y = popDiff, color = continent),stat = 'identity') +
  labs(x = 'Country',y = 'change in pop. 2002 to 2007')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))
  coord_flip() ##switch coordinate

