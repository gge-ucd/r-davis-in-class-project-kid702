surveys <- read.csv("data/portal_data_joined.csv")
surveys
colnames(surveys)


##data-frame information
str(surveys)
summary(surveys)


### look at all the species
length(unique(surveys$species))
##or
sum(!duplicated(surveys$species))


#call out the first 60 value at column 6,9,13 (species_id, weight, plot type)
surveys_base<- head(surveys[c(6,9,13)],n=60)
surveys_base

####factor
surveys_base$species_id <- as.factor(surveys_base$species_id)
surveys_base$plot_type <- as.factor (surveys_base$plot_type)

#check data type
class(surveys_base$species_id)
class (surveys_base$plot_type)

#Remove row containing NA from dataframe
na.omit(surveys_base)
#?na.omit
##or
survey_no_NA <- surveys_base[complete.cases(surveys_base),]
#?complete.cases


###CHALLENGE, filter by weight >150
surveys_base<- head(surveys[c(6,9,13)],n=60)
surveys_base
challenge_base <-surveys_base[(surveys_base[,2]>150),]
challenge_base
challenge_base <-na.omit(challenge_base)
challenge_base
