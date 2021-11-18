 mloa<- read_csv("https://raw.githubusercontent.com/gge-ucd/R-DAVIS/master/data/mauna_loa_met_2001_minute.csv")
library(lubridate)
library(tidyverse)
 
###remove observations with missing values in rel_humid, temp_C_2m, and windSpeed_m_s.
mloa2 <- mloa %>%  filter(rel_humid!= -99, temp_C_2m!= -999.9, windSpeed_m_s!=-99.9)
#generate a column called “datetime” using the year, month, day, hour24, and min columns
mloa3 <- mloa2 %>% mutate(datetime=ymd_hm(paste(year,month,day,hour24,min, sep = "-"))) 
##%>%   select(datetime) # to see if it working

#paste:  combine character string in R

#####create a column called “datetimeLocal” that converts the datetime column to Pacific/Honolulu time
mloa3$datetimeLocal = with_tz(time=mloa3$datetime,tzone = "Pacific/Honolulu")


##calculate the mean hourly temperature each month using the temp_C_2m column and the datetimeLocal columns.
###(HINT: Look at the lubridate functions called month() and hour()).
####Finally, make a ggplot scatterplot of the mean monthly temperature, with points colored by local hour

mloa3 %>% mutate(localmonth=month(datetimeLocal),
                 localhour = hour(datetimeLocal)) %>% 
  group_by(localmonth,localhour) %>% 
  summarize(meantemp = mean(temp_C_2m)) %>% 
  ggplot(aes(x=localmonth, y= meantemp))+
  geom_point(aes(col= localhour))+ 
  xlab("Month")+
  ylab("Mean temperature (Degree C)")+
  scale_color_viridis_c()+ ####add color to gradient
  theme_classic()









