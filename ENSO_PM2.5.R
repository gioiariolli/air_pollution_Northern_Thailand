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
dataset_CM20$NO = as.numeric(dataset_CM20$NO) 
dataset_CM20$PM2.5 = as.numeric(dataset_CM20$PM2.5) 
dataset_CM20$NO2 = as.numeric(dataset_CM20$NO2) 
dataset_CM20$SO2 = as.numeric(dataset_CM20$SO2) 
dataset_CM20$O3 = as.numeric(dataset_CM20$O3) 
dataset_CM20$PM10 = as.numeric(dataset_CM20$PM10) 
dataset_CM20$Rain = as.numeric(dataset_CM20$Rain) 
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
dataset_long_CM21clean <- dataset_long_CM21 %>% drop_na() ###we took off the NAs because they could affect our mean values, sooner we will do the same with 0
# creating a new data set where the data are organized per days
by_day_21 <- group_by(dataset_long_CM21clean, anno_mese_giorno, variable)
mean_day_21 <- summarise(by_day_21, mean=mean(value, na.rm=T))


#######2022
dataset_CM22 <- select(`(35t)CM_22`, !ora)
dataset_CM22$CO = as.numeric(dataset_CM22$CO) 
dataset_CM22$SO2 = as.numeric(dataset_CM22$SO2)
dataset_CM22$PM2.5 = as.numeric(dataset_CM22$PM2.5) 
dataset_CM22$PM10 = as.numeric(dataset_CM22$PM10) 
# pivot the table to read it
dataset_long_CM22 <- dataset_CM22 |> pivot_longer(cols=!anno_mese_giorno,
                                                      cols_vary = "fastest", 
                                                      names_to = "variable", 
                                                      values_to = "value")

# change the point into commas
dataset_long_CM22$value <- gsub(",", ".", dataset_long_CM22$value)

# to change the data set values from character to numeric
dataset_long_CM22$value = as.numeric(dataset_long_CM22$value) 

# to organize our data set in month/day
dataset_long_CM22$anno_mese_giorno <- ymd(dataset_long_CM22$anno_mese_giorno)
dataset_long_CM22clean <- dataset_long_CM22 %>% drop_na() ###we took off the NAs because they could affect our mean values, sooner we will do the same with 0
# creating a new data set where the data are organized per days
by_day_22 <- group_by(dataset_long_CM22clean, anno_mese_giorno, variable)
mean_day_22 <- summarise(by_day_22, mean=mean(value, na.rm=T))

dataset_CM22_may <- select(`(35t)CM_22_may`, !ora)

# pivot the table to read it
dataset_long_CM22_may <- dataset_CM22_may |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")

# change the point into commas
dataset_long_CM22_may$value <- gsub(",", ".", dataset_long_CM22_may$value)

# to change the data set values from character to numeric
dataset_long_CM22_may$value = as.numeric(dataset_long_CM22_may$value) 

# to organize our data set in month/day
dataset_long_CM22_may$anno_mese_giorno <- ymd(dataset_long_CM22_may$anno_mese_giorno)
dataset_long_CM22mayclean <- dataset_long_CM22_may %>% drop_na() ###we took off the NAs because they could affect our mean values, sooner we will do the same with 0
# creating a new data set where the data are organized per days
by_day_22_may <- group_by(dataset_long_CM22mayclean, anno_mese_giorno, variable)
mean_day_22_may <- summarise(by_day_22_may, mean=mean(value, na.rm=T))

####################2023
dataset_CM23 <- select(`(35t)CM_23`, !ora)
dataset_CM23 <- dataset_CM23[1:2880, ]
dataset_CM23$CO = as.numeric(dataset_CM23$CO)
dataset_CM23$SO2 = as.numeric(dataset_CM23$SO2)
dataset_CM23$PM2.5 = as.numeric(dataset_CM23$PM2.5)
dataset_CM23$PM10 = as.numeric(dataset_CM23$PM10)
dataset_CM23$anno_mese_giorno = as.character(dataset_CM23$anno_mese_giorno)
# pivot the table to read it
dataset_long_CM23 <- dataset_CM23 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")

# change the point into commas
dataset_long_CM23$value <- gsub(",", ".", dataset_long_CM23$value)

# to change the data set values from character to numeric
dataset_long_CM23$value = as.numeric(dataset_long_CM23$value) 

# to organize our data set in month/day
dataset_long_CM23$anno_mese_giorno <- ymd(dataset_long_CM23$anno_mese_giorno)
dataset_long_CM23clean <- dataset_long_CM23 %>%drop_na()

head(dataset_CM23$anno_mese_giorno)
# creating a new data set where the data are organized per days
by_day_23 <- group_by(dataset_long_CM23clean, anno_mese_giorno, variable)
mean_day_23 <- summarise(by_day_23, mean=mean(value, na.rm=T))

dataset_CM23july <- select(`(35t)CM_23(july)`, !ora)
dataset_CM23july <- dataset_CM23july[1:4280, ]
# pivot the table to read it
dataset_long_CM23july <- dataset_CM23july |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")

# change the point into commas
dataset_long_CM23july$value <- gsub(",", ".", dataset_long_CM23july$value)
dataset_CM23july$anno_mese_giorno = as.character(dataset_CM23july$anno_mese_giorno)
# to change the data set values from character to numeric
dataset_long_CM23july$value = as.numeric(dataset_long_CM23july$value) 

# to organize our data set in month/day
dataset_long_CM23july$anno_mese_giorno <- ymd(dataset_long_CM23july$anno_mese_giorno)
dataset_long_CM23cleanjuly <- dataset_long_CM23july %>%drop_na()

head(dataset_CM23$anno_mese_giorno)
# creating a new data set where the data are organized per days
by_day_23july <- group_by(dataset_long_CM23cleanjuly, anno_mese_giorno, variable)
mean_day_23july <- summarise(by_day_23july, mean=mean(value, na.rm=T))
#########2024
dataset_CM24 <- select(`(35t)CM_24`, !ora)

# pivot the table to read it
dataset_long_CM24 <- dataset_CM24 |> pivot_longer(cols=!anno_mese_giorno,
                                                          cols_vary = "fastest", 
                                                          names_to = "variable", 
                                                          values_to = "value")

# change the point into commas
dataset_long_CM24$value <- gsub(",", ".", dataset_long_CM24$value)

# to change the data set values from character to numeric
dataset_long_CM24$value = as.numeric(dataset_long_CM24$value) 

# to organize our data set in month/day
dataset_long_CM24$anno_mese_giorno <- ymd(dataset_long_CM24$anno_mese_giorno)
dataset_long_CM24clean <- dataset_long_CM24 %>% drop_na() ###we took off the NAs because they could affect our mean values, sooner we will do the same with 0
# creating a new data set where the data are organized per days
by_day_24 <- group_by(dataset_long_CM24clean, anno_mese_giorno, variable)
mean_day_24 <- summarise(by_day_24, mean=mean(value, na.rm=T))
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
pm25_CM22 <- mean_day_22 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM23 <- mean_day_23 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM23july <- mean_day_23july %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM22may <- mean_day_22_may %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CM24 <- mean_day_24 %>%
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
pm25_CM22$year <-2022
pm25_CM22may$year <-2024
pm25_CM23$year <-2023
pm25_CM23july$year <-2023
pm25_CM24$year <-2024
# we bind all the graphics together
pm25_combined <- bind_rows(pm25_CM16, pm25_CM17, pm25_CM18_sept, pm25_CM18, pm25_CM19, 
                           pm25_CM20, pm25_CM21, pm25_CM22, pm25_CM22may, pm25_CM23, 
                           pm25_CM23july, pm25_CM24)

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

library(dplyr)

# Classify years into El Niño and La Niña categories
pm25_all_years <- bind_rows(
  mutate(pm25_CM16, year = 2016, ENSO = "El Niño"),
  mutate(pm25_CM17, year = 2017, ENSO = "La Niña"),
  mutate(pm25_CM18_sept, year = 2018, ENSO = "La Niña"),
  mutate(pm25_CM18, year = 2018, ENSO = "La Niña"),
  mutate(pm25_CM19, year = 2019, ENSO = "El Niño"),
  mutate(pm25_CM20, year = 2020, ENSO = "El Niño"),
  mutate(pm25_CM21, year = 2021, ENSO = "La Niña"),
  mutate(pm25_CM22, year = 2022, ENSO = "La Niña"),
  mutate(pm25_CM22may, year = 2022, ENSO = "La Niña"),
  mutate(pm25_CM23, year = 2023, ENSO = "El Niño"),
  mutate(pm25_CM23july, year = 2023, ENSO = "El Niño"),
  mutate(pm25_CM24, year = 2024, ENSO = "El Niño"),
)

# Verify combined data
head(pm25_all_years)

library(ggplot2)

# Create the plot
ggplot(pm25_all_years, aes(x = anno_mese_giorno, y = mean, color = ENSO)) +
  geom_line(alpha = 0.8) + 
  labs(title = "Daily PM2.5 Mean Values by Year and ENSO Classification",
       x = "",
       y = "PM2.5 mean values (ug/mL)",
       color = "ENSO Classification") +
  scale_color_manual(values = c("El Niño" = "red", "La Niña" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top")
