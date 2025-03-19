library(lubridate)
library(dplyr)
library(tidyverse)

######2015 PCD dataset from January to May#####
dataset_CM16 <- select(`(36)chiang_Maicity_2016`, !ora)
dataset_CM16_05 <-slice_head(dataset_CM16, n = 2904)

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
dataset_long_CM16$anno_mese_giorno = as.numeric(dataset_long_CM16$anno_mese_giorno) 

# to organize our data set in year/month/day
dataset_long_CM16$anno_mese_giorno <- ymd(dataset_long_CM16$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day <- group_by(dataset_long_CM16, anno_mese_giorno, variable)
mean_day <- summarise(by_day, mean=mean(value, na.rm=T))
mean_day <- mean_day %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######### WE WANT TO CREATE A GGPLOT FOR THE MEAN VALUES OF CO #########
# Filtering the data set per the mean day of the variable CO
dataset_CO_CM16 <- filter(mean_day, variable== "CO")

# Creating a monthly ggplot for CO 
library(ggplot2)
ggplot(dataset_CO_CM16, aes(x=anno_mese_giorno, y=mean)) + 
  geom_line()+ ggtitle("2016 Daily CO mean") + xlab("Month") + ylab("Mean CO ppm at 3m")

#now we divide them in different plot, one for each gas
mean_day %>% filter(variable %in% c("CO", "NO", "NO2", "NOX", "SO2", "PM10", "PM2.5")) %>%
  ggplot(aes(x=anno_mese_giorno, y=mean)) + 
  geom_smooth() +
  facet_wrap(~variable, nrow=2, scales="free") + 
  xlab("") + ylab("Daily mean gas concentration at 3 m (ppb), 2016")

####we do the same with 4 meteorological parameter
mean_day %>% filter(variable %in% c("Rel.hum", "Wind.dir", "Wind.speed", "Temp")) %>%
  ggplot(aes(x=anno_mese_giorno, y=mean)) + 
  geom_smooth() + 
  facet_wrap(~variable, scales="free",
             strip.position="top", 
             labeller = as_labeller(c(Rel.hum = "Rel. hum at 2 m (%RH)", Temp = "Temp at 2m", 
                                      Wind.dir = "Wind. dir at 30 m (Deg.M)", 
                                      Wind.speed = "Wind. speed at 30 m (m/s)" ))) + 
  xlab("") + ylab("Meteorological parameters")
