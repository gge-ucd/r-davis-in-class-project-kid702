library(tidyverse)

gapminder <- read_csv("https://gge-ucd.github.io/R-DAVIS/data/gapminder.csv")
# calculate mean life exp Then create a plot that shows how life expectancy has changed over time in each continent

gapminder %>%
  group_by(continent, year) %>% 
  summarize(mean_lifeExp = mean(lifeExp)) %>%
ggplot()+
  geom_point(aes(x = year, y = mean_lifeExp, color = continent))+
  geom_line(aes(x = year, y = mean_lifeExp, color = continent)) +
  theme_classic()
###What do you think the scale_x_log10() line of code is achieving? 

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), size = .25) + 
  scale_x_log10() +
  geom_smooth(method =  color = 'black', linetype = 'dashed') +
  theme_bw()

##Create a boxplot that shows the life expectency for Brazil, China, El Salvador, Niger, and the United States, with the data points in the background using geom_jitter. 

countries <- c("Brazil", "China", "El Salvador", "Niger", "United States") 

gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(x = country, y = lifeExp))+
  geom_boxplot(alpha = 0.3, aes (x = country, y = lifeExp, color = country)) +
  geom_jitter(alpha = 0.3, color = "blue")+
  ggtitle("Life Expectancy of Five Countries")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Country") + ylab("Life Expectancy")+
  theme_classic()
  
 
