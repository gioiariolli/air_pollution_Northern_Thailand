# For each dataset we calculte the daily mean of PM2.5
library(tidyr)
library(lubridate)
library(dplyr)

######CHIANG MAI PROVINCE############ these data are from the Pollution Controll Department of Thailand
######Data 2016###############
dataset_CM16 <- select(`(35)CM_16`, !ora)
dataset_CM16_05 <-slice_tail(dataset_CM16, n = 4175)

# pivot the table to read it
dataset_long_CM16 <- dataset_CM16_05 |> pivot_longer(cols=!anno_mese_giorno,
                                                     cols_vary = "fastest", 
                                                     names_to = "variable", 
                                                     values_to = "value"
)
# change the point into commas
dataset_long_CM16$value <- gsub(",", ".", dataset_long_CM16$value)

# to change the data set values from character to numeric
dataset_long_CM16$value = as.numeric(dataset_long_CM16$value) 

# to organize our data set in month/day
dataset_long_CM16$anno_mese_giorno <- ymd(dataset_long_CM16$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_16 <- group_by(dataset_long_CM16, anno_mese_giorno, variable)
mean_day_16 <- summarise(by_day_16, mean=mean(value, na.rm=T))
mean_day_16 <- mean_day_16 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

#######2017
dataset_CM17 <- select(`(35t)CM_17`, !ora)

# pivot the table to read it
dataset_long_CM17 <- dataset_CM17 |> pivot_longer(cols=!anno_mese_giorno,
                                                     cols_vary = "fastest", 
                                                     names_to = "variable", 
                                                     values_to = "value"
)
# change the point into commas
dataset_long_CM17$value <- gsub(",", ".", dataset_long_CM17$value)

# to change the data set values from character to numeric
dataset_long_CM17$value = as.numeric(dataset_long_CM17$value) 

# to organize our data set in month/day
dataset_long_CM17$anno_mese_giorno <- ymd(dataset_long_CM17$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_17 <- group_by(dataset_long_CM17, anno_mese_giorno, variable)
mean_day_17 <- summarise(by_day_17, mean=mean(value, na.rm=T))
#######2018 
dataset_CM18_sept <- select(`(35t)CM_18_sept`, !ora)

# pivot the table to read it
dataset_long_CM18_sept <- dataset_CM18_sept |> pivot_longer(cols=!anno_mese_giorno,
                                                     cols_vary = "fastest", 
                                                     names_to = "variable", 
                                                     values_to = "value")
# change the point into commas
dataset_long_CM18_sept$value <- gsub(",", ".", dataset_long_CM18_sept$value)

# to change the data set values from character to numeric
dataset_long_CM18_sept$value = as.numeric(dataset_long_CM18_sept$value) 

# to organize our data set in month/day
dataset_long_CM18_sept$anno_mese_giorno <- ymd(dataset_long_CM18_sept$anno_mese_giorno)
dataset_long_CM18_septclean <- dataset_long_CM18_sept %>%drop_na()
# creating a new data set where the data are organized per days
by_day_18_sept <- group_by(dataset_long_CM18_septclean, anno_mese_giorno, variable)
mean_day_18_sept <- summarise(by_day_18_sept, mean=mean(value, na.rm=T))
mean_day_18_sept <- mean_day_18_sept %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######Data until 30th of April 2015###############
dataset_CM15 <- select(`(35)CM_15_may`, !ora)

# pivot the table to read it
dataset_long_CM15 <- dataset_CM15 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value"
)
# change the point into commas
dataset_long_CM15$value <- gsub(",", ".", dataset_long_CM15$value)

# to change the data set values from character to numeric
dataset_long_CM15$value = as.numeric(dataset_long_CM15$value) 

# to organize our data set in month/day
dataset_long_CM15$anno_mese_giorno <- ymd(dataset_long_CM15$anno_mese_giorno)
dataset_long_CM15clean <- dataset_long_CM15 %>%drop_na()
# creating a new data set where the data are organized per days
by_day_15 <- group_by(dataset_long_CM15clean, anno_mese_giorno, variable)
mean_day_15 <- summarise(by_day_15, mean=mean(value, na.rm=T))
mean_day_15 <- mean_day_15 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))


######2016 PCD dataset###############
dataset_CM16 <- select(`(35)CM_16`, !ora)

# pivot the table to read it
dataset_long_CM16 <- dataset_CM16 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")
# change the point into commas
dataset_long_CM16$value <- gsub(",", ".", dataset_long_CM16$value)

# to change the data set values from character to numeric
dataset_long_CM16$value = as.numeric(dataset_long_CM16$value) 

# to organize our data set in month/day
dataset_long_CM16$anno_mese_giorno <- ymd(dataset_long_CM16$anno_mese_giorno)
dataset_long_CM16clean <- dataset_long_CM16 %>%drop_na()
# creating a new data set where the data are organized per days
by_day_16 <- group_by(dataset_long_CM16clean, anno_mese_giorno, variable)
mean_day_16 <- summarise(by_day_16, mean=mean(value, na.rm=T))

######2017 PCD dataset###############
dataset_CM17 <- select(`(35t)CM_17`, !ora)

# pivot the table to read it
dataset_long_CM17 <- dataset_CM17 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")
# change the point into commas
dataset_long_CM17$value <- gsub(",", ".", dataset_long_CM17$value)

# to change the data set values from character to numeric
dataset_long_CM17$value = as.numeric(dataset_long_CM17$value) 

# to organize our data set in month/day
dataset_long_CM17$anno_mese_giorno <- ymd(dataset_long_CM17$anno_mese_giorno)
dataset_long_CM17clean <- dataset_long_CM17 %>%drop_na()
# creating a new data set where the data are organized per days
by_day_17 <- group_by(dataset_long_CM17clean, anno_mese_giorno, variable)
mean_day_17 <- summarise(by_day_17, mean=mean(value, na.rm=T))

######2018 PCD dataset###############
dataset_CM18 <- select(`(35t)CM_18`, !ora)

# pivot the table to read it
dataset_long_CM18 <- dataset_CM18 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")
# change the point into commas
dataset_long_CM18$value <- gsub(",", ".", dataset_long_CM18$value)

# to change the data set values from character to numeric
dataset_long_CM18$value = as.numeric(dataset_long_CM18$value) 

# to organize our data set in month/day
dataset_long_CM18$anno_mese_giorno <- ymd(dataset_long_CM18$anno_mese_giorno)
dataset_long_CM18clean <- dataset_long_CM18 %>%drop_na()
# creating a new data set where the data are organized per days
by_day_18 <- group_by(dataset_long_CM18clean, anno_mese_giorno, variable)
mean_day_18 <- summarise(by_day_18, mean=mean(value, na.rm=T))


######2019 PCD dataset###############
dataset_CM19 <- select(`(35t)CM_19`, !ora)

# pivot the table to read it
dataset_long_CM19 <- dataset_CM19 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")
# change the point into commas
dataset_long_CM19$value <- gsub(",", ".", dataset_long_CM19$value)

# to change the data set values from character to numeric
dataset_long_CM19$value = as.numeric(dataset_long_CM19$value) 

# to organize our data set in month/day
dataset_long_CM19$anno_mese_giorno <- ymd(dataset_long_CM19$anno_mese_giorno)
dataset_long_CM19clean <- dataset_long_CM19 %>%drop_na()
# creating a new data set where the data are organized per days
by_day_19 <- group_by(dataset_long_CM19clean, anno_mese_giorno, variable)
mean_day_19 <- summarise(by_day_19, mean=mean(value, na.rm=T))


######2020 PCD dataset###############
dataset_CM20 <- select(`(35t)CM_20`, !ora)

# pivot the table to read it
dataset_long_CM20 <- dataset_CM20 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")

# change the point into commas
dataset_long_CM20$value <- gsub(",", ".", dataset_long_CM20$value)

# to change the data set values from character to numeric
dataset_long_CM20$value = as.numeric(dataset_long_CM20$value) 

# to organize our data set in month/day
dataset_long_CM20$anno_mese_giorno <- ymd(dataset_long_CM20$anno_mese_giorno)
dataset_long_CM20clean <- dataset_long_CM20 %>%drop_na()
# creating a new data set where the data are organized per days
by_day_20 <- group_by(dataset_long_CM20clean, anno_mese_giorno, variable)
mean_day_20 <- summarise(by_day_20, mean=mean(value, na.rm=T))


######agosto 2021 PCD dataset###############
dataset_CM21_ago <- select(`(35t)CM_21_ago`, !ora)

# pivot the table to read it
dataset_long_CM21 <- dataset_CM21_ago |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")

# change the point into commas
dataset_long_CM21$value <- gsub(",", ".", dataset_long_CM21$value)

# to change the data set values from character to numeric
dataset_long_CM21$value = as.numeric(dataset_long_CM21$value) 

# to organize our data set in month/day
dataset_long_CM21$anno_mese_giorno <- ymd(dataset_long_CM21$anno_mese_giorno)
dataset_long_CM21clean <- dataset_long_CM21 %>%drop_na(). ###we took off the NAs because they could affect our mean values, sooner we will do the same with 0
# creating a new data set where the data are organized per days
by_day_21 <- group_by(dataset_long_CM21clean, anno_mese_giorno, variable)
mean_day_21 <- summarise(by_day_21, mean=mean(value, na.rm=T))

##########################################################
##### NOW WE FILTER THE PM2.5 FOR EACH YEAR AND WE PUT THEM TOGETHER ON A LARGE DATASET
#We don't consider the mean= 0.00 because these are missing dates
pm25_CM16 <- mean_day_16 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM17 <- mean_day_17 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM18_sept <- mean_day_18_sept %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM18 <- mean_day_18 %>%
  filter(variable == "PM2.5" & mean != 0.00)
pm25_CM19 <- mean_day_19 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM20 <- mean_day_20 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM21 <- mean_day_21 %>%
  filter(variable == "PM2.5"& mean != 0.00)

#

library(ggplot2)
library(lubridate)
# add the year 
pm25_CM16$year <-2016
pm25_CM17$year <-2017
pm25_CM18_sept$year <-2018
pm25_CM18$year <- 2018
pm25_CM19$year <- 2019
pm25_CM20$year <- 2020
pm25_CM21$year <- 2021

# we bind all the graphics together
pm25_combined <- bind_rows(pm25_CM16, pm25_CM17, pm25_CM18_sept, pm25_CM18, pm25_CM19, pm25_CM20, pm25_CM21)

# we plot them
ggplot(pm25_combined, aes(x = anno_mese_giorno, y = mean, color = as.factor(year))) +
  geom_line(alpha = 0.6) +
  geom_smooth() +# Scatter plot with transparency
  labs(title = "Daily PM2.5 Mean Concentrations Over the Years", 
       x = "Date", 
       y = "Daily PM2.5 Mean Concentration (µg/m³)",
       color = "Year") +  # Label for color legend
  theme_minimal() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
