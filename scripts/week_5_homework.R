library(tidyverse)
surveys <- read_csv('data/portal_data_joined.csv')
colnames(surveys)

#1.Make wide tibble with genus and each plot_type with value of mean hind_foot_length

surveys_wide<- surveys %>% 
  filter(.,!is.na(hindfoot_length)) %>% 
  group_by(genus, plot_type) %>% 
  summarise(hindfoot_mean=mean(hindfoot_length)) %>% 
  pivot_wider(names_from = "plot_type", values_from = "hindfoot_mean")
                                                                                                                                                               
survey_wide

##categorize weight (not yet success)

    #by ifelse
surveys %>% 
  filter(.,!is.na(weight)) %>% mutate(weight_cat = ifelse(weight <= 20, "small",
                                                           ifelse(weight >= 48, "large",
                                                          "medium"))) %>% tail()

    ## using case_when
Survey_catalized <- surveys %>% 
  filter(.,!is.na(weight)) %>% 
  mutate (weight_cat = case_when(weight <= 20 ~ "small",
                                 weight > 20 & weight < 48 ~ "medium",
                                 weight >= 48 ~ "large"))
  tail(Survey_catalized)
  
  ### Challenge soft code quartiles
surveys_no_NA_weight <- surveys %>% filter(.,!is.na(weight))
  
Survey_catalized_challenge <- surveys_no_NA_weight %>% 
  mutate (weight_cat = case_when(weight <= quantile(surveys_no_NA_weight$weight,0.25) ~ "small",
                                   weight > quantile(surveys_no_NA_weight$weight,0.25) & weight < quantile(surveys_no_NA_weight$weight,0.75) ~ "medium",
                                   weight >= quantile(surveys_no_NA_weight$weight,0.75) ~ "large"))
  tail(Survey_catalized_challenge)
  

