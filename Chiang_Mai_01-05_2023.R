library(lubridate)
library(dplyr)
library(tidyverse)

######2015 PCD dataset from January to May#####
dataset_CM23 <- select(`(36t)CM_2023`, !ora)

# pivot the table to read it
dataset_long_CM23 <- dataset_CM23 |> pivot_longer(cols=!anno_mese_giorno,
                                                     cols_vary = "fastest", 
                                                     names_to = "variable", 
                                                     values_to = "value")
# change the point into commas
dataset_long_CM23$value <- gsub(",", ".", dataset_long_CM23$value)

dataset_long_CM23$CO = as.numeric(dataset_long_CM23$CO) 
# to change the data set values from character to numeric
dataset_long_CM23$value = as.numeric(dataset_long_CM23$value) 
dataset_long_CM23$anno_mese_giorno = as.numeric(dataset_long_CM23$anno_mese_giorno) 

# to organize our data set in year/month/day
dataset_long_CM23$anno_mese_giorno <- ymd(dataset_long_CM23$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_23 <- group_by(dataset_long_CM23, anno_mese_giorno, variable)
mean_day_23 <- summarise(by_day_23, mean=mean(value, na.rm=T))
mean_day_23 <- mean_day_23 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean)) #THIS DATA IS NOT GOOD!

# Creating a monthly ggplot for CO 
library(ggplot2)

#now we divide them in different plot, one for each gas
mean_day_23 %>% filter(variable %in% c("CO", "PM10", "PM2.5")) %>%
  ggplot(aes(x=anno_mese_giorno, y=mean)) + 
  geom_smooth() +
  facet_wrap(~variable, nrow=2, scales="free") + 
  xlab("") + ylab("Daily mean gas concentration at 3 m (ppb), 2016")
####CO should be removed because the mean goes to 0 and many data are not present. Keep focusing of Pm2.5 and PM10(?)
