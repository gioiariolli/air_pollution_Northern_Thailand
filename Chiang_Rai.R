library(ggplot2)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggpubr)

######CHIANG RAI PROVINCE############ these data are from the Pollution Controll Department of Thailand
######Data 2019###############
dataset_CR19 <- select(`(73t)CR_19`, !ora)

# pivot the table to read it
dataset_long_CR19 <- dataset_CR19 |> pivot_longer(cols=!anno_mese_giorno,
                                                     cols_vary = "fastest", 
                                                     names_to = "variable", 
                                                     values_to = "value"
)
# change the point into commas
dataset_long_CR19$value <- gsub(",", ".", dataset_long_CR19$value)

# to change the data set values from character to numeric
dataset_long_CR19$value = as.numeric(dataset_long_CR19$value) 

# to organize our data set in month/day
dataset_long_CR19$anno_mese_giorno <- ymd(dataset_long_CR19$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_19 <- group_by(dataset_long_CR19, anno_mese_giorno, variable)
mean_day_19 <- summarise(by_day_19, mean=mean(value, na.rm=T))
mean_day_19 <- mean_day_19 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######Data 2020a###############
dataset_CR20 <- select(`(73t)CR_20`, !ora)

# pivot the table to read it
dataset_long_CR20 <- dataset_CR20 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value"
)
# change the point into commas
dataset_long_CR20$value <- gsub(",", ".", dataset_long_CR20$value)

# to change the data set values from character to numeric
dataset_long_CR20$value = as.numeric(dataset_long_CR20$value) 

# to organize our data set in month/day
dataset_long_CR20$anno_mese_giorno <- ymd(dataset_long_CR20$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_20 <- group_by(dataset_long_CR20, anno_mese_giorno, variable)
mean_day_20 <- summarise(by_day_20, mean=mean(value, na.rm=T))
mean_day_20 <- mean_day_20 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######Data 2020b###############
dataset_CR21 <- select(`(73t)CR_21`, !ora)

# pivot the table to read it
dataset_long_CR21 <- dataset_CR21 |> pivot_longer(cols=!anno_mese_giorno,
                                                          cols_vary = "fastest", 
                                                          names_to = "variable", 
                                                          values_to = "value"
)
# change the point into commas
dataset_long_CR21$value <- gsub(",", ".", dataset_long_CR21$value)

# to change the data set values from character to numeric
dataset_long_CR21$value = as.numeric(dataset_long_CR21$value) 

# to organize our data set in month/day
dataset_long_CR21$anno_mese_giorno <- ymd(dataset_long_CR21$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_21 <- group_by(dataset_long_CR21, anno_mese_giorno, variable)
mean_day_21 <- summarise(by_day_21, mean=mean(value, na.rm=T))
mean_day_21 <- mean_day_21 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######Data 2022###############
dataset_CR22 <- select(`(73t)CR_22`, !ora)

# pivot the table to read it
dataset_long_CR22 <- dataset_CR22 |> pivot_longer(cols=!anno_mese_giorno,
                                                          cols_vary = "fastest", 
                                                          names_to = "variable", 
                                                          values_to = "value"
)
# change the point into commas
dataset_long_CR22$value <- gsub(",", ".", dataset_long_CR22$value)

# to change the data set values from character to numeric
dataset_long_CR22$value = as.numeric(dataset_long_CR22$value) 

# to organize our data set in month/day
dataset_long_CR22$anno_mese_giorno <- ymd(dataset_long_CR22$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_22 <- group_by(dataset_long_CR22, anno_mese_giorno, variable)
mean_day_22 <- summarise(by_day_22, mean=mean(value, na.rm=T))
mean_day_22 <- mean_day_22 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######Data 2023###############
dataset_CR23 <- select(`(73t)CR_23_augdic`, !ora)

# pivot the table to read it
dataset_CR23$PM10 = as.numeric(dataset_CR23$PM10) 
dataset_CR23$CO = as.numeric(dataset_CR23$CO) 
dataset_CR23$O3 = as.numeric(dataset_CR23$O3) 
dataset_CR23$PM2.5 = as.numeric(dataset_CR23$PM2.5) 
dataset_CR23$NO = as.numeric(dataset_CR23$NO) 
dataset_CR23$NO2 = as.numeric(dataset_CR23$NO2) 
dataset_CR23$NOX = as.numeric(dataset_CR23$NOX) 
dataset_CR23$SO2 = as.numeric(dataset_CR23$SO2) 
dataset_long_CR23 <- dataset_CR23 |> pivot_longer(cols=!anno_mese_giorno,
                                                          cols_vary = "fastest", 
                                                          names_to = "variable", 
                                                          values_to = "value"
)

# change the point into commas
dataset_long_CR23$value <- gsub(",", ".", dataset_long_CR23$value)

# to change the data set values from character to numeric
dataset_long_CR23$value = as.numeric(dataset_long_CR23$value) 

# to organize our data set in month/day
dataset_long_CR23$anno_mese_giorno <- ymd(dataset_long_CR23$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_23 <- group_by(dataset_long_CR23, anno_mese_giorno, variable)
mean_day_23 <- summarise(by_day_23, mean=mean(value, na.rm=T))
mean_day_23 <- mean_day_23 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######Data 2023b###############
dataset_CR23b <- select(`(73t)CR23_genapr`, !ora)
# pivot the table to read it
dataset_CR23b$PM10 = as.numeric(dataset_CR23b$PM10) 
dataset_CR23b$CO = as.numeric(dataset_CR23b$CO) 
dataset_CR23b$O3 = as.numeric(dataset_CR23b$O3) 
dataset_CR23b$PM2.5 = as.numeric(dataset_CR23b$PM2.5) 
dataset_CR23b$NO = as.numeric(dataset_CR23b$NO) 
dataset_CR23b$NO2 = as.numeric(dataset_CR23b$NO2) 
dataset_CR23b$NOX = as.numeric(dataset_CR23b$NOX) 
dataset_CR23b$SO2 = as.numeric(dataset_CR23b$SO2) 
dataset_CR23b$Wind.speed = as.numeric(dataset_CR23b$Wind.speed) 
dataset_CR23b$Rain= as.numeric(dataset_CR23b$Rain) 
dataset_long_CR23b <- dataset_CR23b |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value"
)

# change the point into commas
dataset_long_CR23b$value <- gsub(",", ".", dataset_long_CR23b$value)

# to change the data set values from character to numeric
dataset_long_CR23b$value = as.numeric(dataset_long_CR23b$value) 

# to organize our data set in month/day
dataset_long_CR23b$anno_mese_giorno <- ymd(dataset_long_CR23b$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_23b <- group_by(dataset_long_CR23b, anno_mese_giorno, variable)
mean_day_23b <- summarise(by_day_23b, mean=mean(value, na.rm=T))
mean_day_23b <- mean_day_23b %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))

######Data 2024###############
dataset_CR24 <- select(`(73t)CR_24`, !ora)

# pivot the table to read it
dataset_CR24$PM10 = as.numeric(dataset_CR24$PM10) 
dataset_CR24$Rel.hum = as.numeric(dataset_CR24$Rel.hum) 
dataset_CR24$CO = as.numeric(dataset_CR24$CO) 
dataset_CR24$O3 = as.numeric(dataset_CR24$O3) 
dataset_CR24$PM2.5 = as.numeric(dataset_CR24$PM2.5) 
dataset_CR24$NO = as.numeric(dataset_CR24$NO) 
dataset_CR24$NO2 = as.numeric(dataset_CR24$NO2) 
dataset_CR24$NOX = as.numeric(dataset_CR24$NOX) 
dataset_CR24$SO2 = as.numeric(dataset_CR24$SO2)
dataset_long_CR24 <- dataset_CR24 |> pivot_longer(cols=!anno_mese_giorno,
                                                  cols_vary = "fastest", 
                                                  names_to = "variable", 
                                                  values_to = "value")
# change the point into commas
dataset_long_CR24$value <- gsub(",", ".", dataset_long_CR24$value)

# to change the data set values from character to numeric
dataset_long_CR24$value = as.numeric(dataset_long_CR24$value) 

# to organize our data set in month/day
dataset_long_CR24$anno_mese_giorno <- ymd(dataset_long_CR24$anno_mese_giorno)

# creating a new data set where the data are organized per days
by_day_24 <- group_by(dataset_long_CR24, anno_mese_giorno, variable)
mean_day_24 <- summarise(by_day_24, mean=mean(value, na.rm=T))
mean_day_24 <- mean_day_24 %>% mutate(mean = ifelse(variable == "CO", mean * 1000, mean))


##########################################################
##### NOW WE FILTER THE PM2.5 FOR EACH YEAR AND WE PUT THEM TOGETHER ON A LARGE DATASET
#We don't consider the mean= 0.00 because these are missing dates
pm25_CR19 <- mean_day_19 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CR20 <- mean_day_20 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CR21 <- mean_day_21 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CR22 <- mean_day_22 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CR23 <- mean_day_23 %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CR23b <- mean_day_23b %>%
  filter(variable == "PM2.5"& mean != 0.00)
pm25_CR24 <- mean_day_24 %>%
  filter(variable == "PM2.5"& mean != 0.00)
#

x <- (pm25_CR21$mean, c(32:124))


library(ggplot2)
library(lubridate)
# add the year 
pm25_CR19$year <- 2019
pm25_CR20$year <- 2020
pm25_CR21$year <- 2021
pm25_CR22$year <-2022
pm25_CR23$year <-2023
pm25_CR23b$year <-2023
pm25_CR24$year <-2024
# we bind all the graphics together
pm25CR_combined <- bind_rows(pm25_CR19, pm25_CR20, pm25_CR21, pm25_CR22, 
                             pm25_CR23, pm25_CR23b, pm25_CM24)


# Classify years into El Niño and La Niña categories
pm25CR_all_years <- bind_rows(
  mutate(pm25_CR19, year = 2019, ENSO = "El Niño"),
  mutate(pm25_CR20, year = 2020, ENSO = "El Niño"),
  mutate(pm25_CR21, year = 2021, ENSO = "La Niña"),
  mutate(pm25_CR22, year = 2022, ENSO = "La Niña"),
  mutate(pm25_CR23, year = 2023, ENSO = "El Niño"),
  mutate(pm25_CR23b, year = 2023, ENSO = "El Niño"),
  mutate(pm25_CR24, year = 2024, ENSO = "El Niño"),
)

# Verify combined data
head(pm25CR_all_years)

library(ggplot2)

# Create the plot
ggplot(pm25CR_all_years, aes(x = anno_mese_giorno, y = mean, color = ENSO, group = interaction(year, ENSO))) +
  geom_line(alpha = 0.8, na.rm = TRUE) + 
  labs(title = "CHIANG RAI - Daily PM2.5 Mean Values by Year and ENSO Classification",
       x = "",
       y = "PM2.5 mean values (ug/mL)",
       color = "ENSO Classification") +
  scale_color_manual(values = c("El Niño" = "red", "La Niña" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top")


##### NOW WE FILTER THE PM10 FOR EACH YEAR AND WE PUT THEM TOGETHER ON A LARGE DATASET
#We don't consider the mean= 0.00 because these are missing dates
pm10_CR19 <- mean_day_19 %>%
  filter(variable == "PM10"& mean != 0.00)
pm10_CR20 <- mean_day_20 %>%
  filter(variable == "PM10"& mean != 0.00)
pm10_CR21 <- mean_day_21 %>%
  filter(variable == "PM10"& mean != 0.00)
pm10_CR22 <- mean_day_22 %>%
  filter(variable == "PM10"& mean != 0.00)
pm10_CR23 <- mean_day_23 %>%
  filter(variable == "PM10"& mean != 0.00)
pm10_CR23b <- mean_day_23b %>%
  filter(variable == "PM10"& mean != 0.00)
pm10_CR24 <- mean_day_24 %>%
  filter(variable == "PM10"& mean != 0.00)
#

library(ggplot2)
library(lubridate)
# add the year 
pm10_CR19$year <- 2019
pm10_CR20$year <- 2020
pm10_CR21$year <- 2021
pm10_CR22$year <-2022
pm10_CR23$year <-2023
pm10_CR23b$year <-2023
pm10_CR24$year <-2024
# we bind all the graphics together
pm10CR_combined <- bind_rows(pm10_CR19, pm10_CR20, pm10_CR21, pm10_CR22, 
                             pm10_CR23, pm10_CR23b, pm10_CR24)


# Classify years into El Niño and La Niña categories
pm10CR_all_years <- bind_rows(
  mutate(pm10_CR19, year = 2019, ENSO = "El Niño"),
  mutate(pm10_CR20, year = 2020, ENSO = "El Niño"),
  mutate(pm10_CR21, year = 2021, ENSO = "La Niña"),
  mutate(pm10_CR22, year = 2022, ENSO = "La Niña"),
  mutate(pm10_CR23, year = 2023, ENSO = "El Niño"),
  mutate(pm10_CR23b, year = 2023, ENSO = "El Niño"),
  mutate(pm10_CR24, year = 2024, ENSO = "El Niño"),
)

# Verify combined data
head(pm10CR_all_years)

library(ggplot2)

# Create the plot
ggplot(pm10CR_all_years, aes(x = anno_mese_giorno, y = mean, color = ENSO, group = interaction(year, ENSO))) +
  geom_line(alpha = 0.8, na.rm = TRUE) + 
  labs(title = "CHIANG RAI - Daily PM10 Mean Values by Year and ENSO Classification",
       x = "",
       y = "PM10 mean values (ug/mL)",
       color = "ENSO Classification") +
  scale_color_manual(values = c("El Niño" = "red", "La Niña" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top")
