library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)


flights <- read.csv('data/Final project/nyc_13_flights_small.csv')
planes <- read.csv('data/Final project/nyc_13_planes.csv')
weather <- read.csv('data/Final project/nyc_13_weather.csv')


colnames(flights)
colnames(planes)
colnames(weather)
###group data

flights_planes <- left_join(flights, planes)

###Joining, by = c("year", "tailnum")

colnames(flights_planes)


final <- left_join(flights_planes, weather)

## Joining, by = c("year", "month", "day", "origin", "hour", "time_hour")

#### 1---- Plot the departure delay of flights against the precipitation, 
#### and include a simple regression line as part of the plot. 
#### Hint: there is a geom_ that will plot a simple y ~ x regression line for you,
#### but you might have to use an argument to make sure it’s a regular linear model.
#### Use ggsave to save your ggplot objects into a new folder you create called “plots”.


colnames(final)

final %>% 
  filter(.,!is.na(precip)) %>% 
  filter(.,!is.na(dep_delay)) %>% 
  ggplot(aes(x = dep_delay, y = precip)) +
  geom_point()+
  geom_smooth(method = 'lm', formula= y~x)+
  theme_classic()


#### 2----Create a figure that has date on the x axis and each day’s mean departure delay on the y axis. 
#### Plot only months September through December.
#### Somehow distinguish between airline carriers (the method is up to you). 
#### Again, save your final product into the “plot” folder.

colnames(final)

Final_1 <- final %>% 
  filter(.,!is.na(dep_delay)) %>% 
  filter(., month > 8) %>% 
  mutate(date=ymd(paste(year, month, day, sep = '-'))) %>% 
  group_by(date, dep_delay) %>% 
  mutate(mean_delay= mean(dep_delay)) 
  
Final_1 %>% ggplot(aes(x= date, y= mean_delay)) + 
  geom_point(aes(col= carrier)) +
  scale_color_viridis_d()+
  theme_classic()



#### 3----Create a dataframe with these columns: date (year, month and day), 
### mean_temp, where each row represents the airport, based on airport code. 
#### Save this is a new csv into you data folder called mean_temp_by_origin.csv


colnames(final)

mean_temp <- final %>% 
  filter(.,!is.na(temp)) %>% 
  mutate(date=ymd(paste(year, month, day, sep = '-'))) %>%
  group_by(origin, date) %>% 
  summarize(mean_temp = mean(temp)) %>% 
  setNames(.,c('airport', 'date', 'mean_temp'))

write_csv(mean_temp, 'mean_temp_by_origin.csv')


#### 4----Make a function that can: (1) convert hours to minutes;
#### and (2) convert minutes to hours 
#### (i.e., it’s going to require some sort of conditional setting in the function 
#### that determines which direction the conversion is going). 
#### Use this function to convert departure delay (currently in minutes) 
#### to hours and then generate a boxplot of departure delay times by carrier. 
#### Save this function into a script called “customFunctions.R” in your scripts/code folder.


time_convert <- function(min, hour, formula) {
  if(formula == 'hrs_to_min') {time <- hour*60 + min}
  else if(formula == 'min_to_hrs') {time<- min/60 + hour}
  print(time)
}

delay_hour <- final %>% 
  filter(.,!is.na(dep_delay)) %>% 
  group_by(carrier,dep_delay) %>% 
  summarize(delay_hr <- time_convert(min = dep_delay, hour = 0, formula = 'min_to_hrs')) %>% 
  setNames(.,c('carrier', 'dep_delay', 'delay_hrs'))

summary(delay_hour)

graph_delay <- delay_hour %>% 
  ggplot(aes(x=carrier , delay_hrs, fill = carrier))+
  geom_boxplot()+
  coord_flip()

#### 5. Do (at least) 5 things to improve this plot by changing, adding, or subtracting to this plot. 
#### The sky’s the limit here, remember we often reduce data to more succinctly communicate things.

delay_hour %>% 
  ggplot(aes(x=carrier , delay_hrs, fill = carrier))+
  geom_violin()+
  coord_flip() +
  theme_bw()+  
  labs(title = "Carrier vs planes delay", x = 'Airlines',y = 'Delay (hrs)', fill = 'Airlines')+ 
  theme(
    plot.title = element_text(color = "red",size = 20, hjust = 0.5, face = "bold"),   
    axis.title.x = element_text(size=15, face="bold"),                              
    axis.title.y = element_text(size=15, face="bold")
  )
  

