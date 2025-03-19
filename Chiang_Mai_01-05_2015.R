install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tidyverse)

######2015 PCD dataset from January to May#####
dataset <- select(`(35)CM.(city.hall)`, !ora)

# pivot the table to read it
dataset_long <- dataset |> pivot_longer(cols=!anno_mese_giorno,
                                         cols_vary = "fastest", 
                                         names_to = "variable", 
                                         values_to = "value"
                                        )
# change the point into commas
dataset_long$value <- gsub(",", ".", dataset_long$value)

# to change the data set values from character to numeric
dataset_long$value = as.numeric(dataset_long$value) 
dataset_long$anno_mese_giorno = as.numeric(dataset_long$anno_mese_giorno) 

# to organize our data set in year/month/day
dataset_long$anno_mese_giorno <- ymd(dataset_long$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day <- group_by(dataset_long, anno_mese_giorno, variable)
mean_day <- summarise(by_day, mean=mean(value, na.rm=T))
mean_day <- mean_day %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))


######### WE WANT TO CREATE A GGPLOT FOR THE MEAN VALUES OF CO #########
# Filtering the data set per the mean day of the variable CO
dataset_CO <- filter(mean_day, variable== "CO")

# Creating a monthly ggplot for CO 
library(ggplot2)
ggplot(dataset_CO, aes(x=anno_mese_giorno, y=mean)) + 
  geom_line()+ ggtitle("2015 Daily CO mean") + xlab("Month") + ylab("Mean CO ppm at 3m")

#now we divide them in different plot, one for each gas
mean_day %>% filter(variable %in% c("CO", "NO", "NO2", "O3", "SO2", "PM10")) %>%
ggplot(aes(x=anno_mese_giorno, y=mean)) + 
  geom_smooth() +
  facet_wrap(~variable, nrow=2, scales="free") + 
  xlab("") + ylab("Daily mean gas concentration at 3 m (ppb)")

####we do the same with meteorological parameter
mean_day %>% filter(variable %in% c("Rain", "Rel.hum", "Wind.dir", "Wind.speed", "Temp", "Net.rad", "Glob.rad")) %>%
ggplot(aes(x=anno_mese_giorno, y=mean)) + 
  geom_smooth() + 
  facet_wrap(~variable, scales="free",
             strip.position="top", 
             labeller = as_labeller(c(Glob.rad = "Glob.rad (w/m2)", Net.rad = "Net.rad (w/m2)", Rain = "Rain at 3 m (mm)", 
                                      Rel.hum = "Rel. hum at 2 m (%RH)", Temp = "Temp at 2m", 
                                      Wind.dir = "Wind. dir at 30 m (Deg.M)", Wind.speed = "Wind. speed at 30 m (m/s)" ))) +
               xlab("") + ylab("Meteorological parameters")
