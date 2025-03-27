#### We apply our ENSO analysis on the different meteorological features (rel humidity, wind speed, temperature...)
#### to look at possible correlation between them and ENSO phenomena.
#### We aim at:
#### 1)Knowing which parameters are affected by La Nina/El Nino events (if there are any between those taken measured)
#### 2)Understanding parameters' variability among the months.
#### 3)Is there a difference in their values between Smoke-Haze and Non-smoke haze period?

#TEMPERATURE-->general trend idientified: stronger surface temperatures during El Nino
temp_CM16 <- mean_day_16 %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM17 <- mean_day_17 %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM18_sept <- mean_day_18_sept %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM18 <- mean_day_18 %>%
  filter(variable == "Temp" & mean != 0.00)
temp_CM19 <- mean_day_19 %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM20 <- mean_day_20 %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM21 <- mean_day_21 %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM22 <- mean_day_22 %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM23 <- mean_day_23 %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM23july <- mean_day_23july %>%
  filter(variable == "Wind.dir"& mean != 0.00)
temp_CM22may <- mean_day_22_may %>%
  filter(variable == "Temp"& mean != 0.00)
temp_CM24 <- mean_day_24 %>%
  filter(variable == "Temp"& mean != 0.00)
# add the year 
temp_CM16$year <-2016
temp_CM17$year <-2017
temp_CM18_sept$year <-2018
temp_CM18$year <- 2018
temp_CM19$year <- 2019
temp_CM20$year <- 2020
temp_CM21$year <- 2021
temp_CM22$year <-2022
temp_CM22may$year <-2024
temp_CM23$year <-2023
temp_CM23july$year <-2023
temp_CM24$year <-2024

temp_all_years <- bind_rows(
  mutate(temp_CM16, year = 2016, ENSO = "El Niño"),
  mutate(temp_CM17, year = 2017, ENSO = "La Niña"),
  mutate(temp_CM18_sept, year = 2018, ENSO = "La Niña"),
  mutate(temp_CM18, year = 2018, ENSO = "La Niña"),
  mutate(temp_CM19, year = 2019, ENSO = "El Niño"),
  mutate(temp_CM20, year = 2020, ENSO = "El Niño"),
  mutate(temp_CM21, year = 2021, ENSO = "La Niña"),
  mutate(temp_CM22, year = 2022, ENSO = "La Niña"),
  mutate(temp_CM22may, year = 2022, ENSO = "La Niña"),
  mutate(temp_CM23, year = 2023, ENSO = "El Niño"),
  mutate(temp_CM23july, year = 2023, ENSO = "El Niño"),
  mutate(temp_CM24, year = 2024, ENSO = "El Niño"),
)

ggplot(temp_all_years, aes(x = anno_mese_giorno, y = mean, color = ENSO, group = interaction(year, ENSO))) +
  geom_line(alpha = 0.8, na.rm = TRUE) +  # Draw lines, grouped by year and ENSO
  labs(title = "CHIANG MAI - Mean surface temperature and ENSO Classification",
       x = "",
       y = "°C Surface temperature (at 2m)",
       color = "ENSO Classification") +
  scale_color_manual(values = c("El Niño" = "red", "La Niña" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top")


#############WIND SPEED --->no general trend is seen/identified, but extremely higher values during 2016 compared to the other years. Why?
windsp_CM16 <- mean_day_16 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM17 <- mean_day_17 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM18_sept <- mean_day_18_sept %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM18 <- mean_day_18 %>%
  filter(variable == "Wind.speed" & mean != 0.00)
windsp_CM19 <- mean_day_19 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM20 <- mean_day_20 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM21 <- mean_day_21 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM22 <- mean_day_22 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM23 <- mean_day_23 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM23july <- mean_day_23july %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM22may <- mean_day_22_may %>%
  filter(variable == "Wind.speed"& mean != 0.00)
windsp_CM24 <- mean_day_24 %>%
  filter(variable == "Wind.speed"& mean != 0.00)
# add the year 
windsp_CM16$year <-2016
windsp_CM17$year <-2017
windsp_CM18_sept$year <-2018
windsp_CM18$year <- 2018
windsp_CM19$year <- 2019
windsp_CM20$year <- 2020
windsp_CM21$year <- 2021
windsp_CM22$year <-2022
windsp_CM22may$year <-2024
windsp_CM23$year <-2023
windsp_CM23july$year <-2023
windsp_CM24$year <-2024

windsp_all_years <- bind_rows(
  mutate(windsp_CM16, year = 2016, ENSO = "El Niño"),
  mutate(windsp_CM17, year = 2017, ENSO = "La Niña"),
  mutate(windsp_CM18_sept, year = 2018, ENSO = "La Niña"),
  mutate(windsp_CM18, year = 2018, ENSO = "La Niña"),
  mutate(windsp_CM19, year = 2019, ENSO = "El Niño"),
  mutate(windsp_CM20, year = 2020, ENSO = "El Niño"),
  mutate(windsp_CM21, year = 2021, ENSO = "La Niña"),
  mutate(windsp_CM22, year = 2022, ENSO = "La Niña"),
  mutate(windsp_CM22may, year = 2022, ENSO = "La Niña"),
  mutate(windsp_CM23, year = 2023, ENSO = "El Niño"),
  mutate(windsp_CM23july, year = 2023, ENSO = "El Niño"),
  mutate(windsp_CM24, year = 2024, ENSO = "El Niño"),
)

ggplot(windsp_all_years, aes(x = anno_mese_giorno, y = mean, color = ENSO, group = interaction(year, ENSO))) +
  geom_line(alpha = 0.8, na.rm = TRUE) +  # Draw lines, grouped by year and ENSO
  labs(title = "CHIANG MAI - Wind Speed and ENSO Classification",
       x = "",
       y = "Wind Speed at 10 m height (m/s)",
       color = "ENSO Classification") +
  scale_color_manual(values = c("El Niño" = "red", "La Niña" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top")

############## Relative humidity
relhum_CM16 <- mean_day_16 %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM17 <- mean_day_17 %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM18_sept <- mean_day_18_sept %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM18 <- mean_day_18 %>%
  filter(variable == "Rel.hum" & mean != 0.00)
relhum_CM19 <- mean_day_19 %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM20 <- mean_day_20 %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM21 <- mean_day_21 %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM22 <- mean_day_22 %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM23 <- mean_day_23 %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM23july <- mean_day_23july %>%
  filter(variable == "Temp"& mean != 0.00)
relhum_CM22may <- mean_day_22_may %>%
  filter(variable == "Rel.hum"& mean != 0.00)
relhum_CM24 <- mean_day_24 %>%
  filter(variable == "Rel.hum"& mean != 0.00)

relhum_CM16$year <-2016
relhum_CM17$year <-2017
relhum_CM18_sept$year <-2018
relhum_CM18$year <- 2018
relhum_CM19$year <- 2019
relhum_CM20$year <- 2020
relhum_CM21$year <- 2021
relhum_CM22$year <-2022
relhum_CM22may$year <-2024
relhum_CM23$year <-2023
relhum_CM23july$year <-2023
relhum_CM24$year <-2024

relhum_all_years <- bind_rows(
  mutate(relhum_CM16, year = 2016, ENSO = "El Niño"),
  mutate(relhum_CM17, year = 2017, ENSO = "La Niña"),
  mutate(relhum_CM18_sept, year = 2018, ENSO = "La Niña"),
  mutate(relhum_CM18, year = 2018, ENSO = "La Niña"),
  mutate(relhum_CM19, year = 2019, ENSO = "El Niño"),
  mutate(relhum_CM20, year = 2020, ENSO = "El Niño"),
  mutate(relhum_CM21, year = 2021, ENSO = "La Niña"),
  mutate(relhum_CM22, year = 2022, ENSO = "La Niña"),
  mutate(relhum_CM22may, year = 2022, ENSO = "La Niña"),
  mutate(relhum_CM23, year = 2023, ENSO = "El Niño"),
  mutate(relhum_CM23july, year = 2023, ENSO = "El Niño"),
  mutate(relhum_CM24, year = 2024, ENSO = "El Niño"),
)

ggplot(relhum_all_years, aes(x = anno_mese_giorno, y = mean, color = ENSO, group = interaction(year, ENSO))) +
  geom_line(alpha = 0.8, na.rm = TRUE) +  # Draw lines, grouped by year and ENSO
  labs(title = "CHIANG MAI - Wind Speed and ENSO Classification",
       x = "",
       y = "Wind Speed at 10 m height (m/s)",
       color = "ENSO Classification") +
  scale_color_manual(values = c("El Niño" = "red", "La Niña" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top")


#############RAIN
rain_CM15 <- mean_day_15 %>%
  filter(variable == "Rain"& mean)
rain_CM16 <- mean_day_16 %>%
  filter(variable == "Rain"& mean)
rain_CM17 <- mean_day_17 %>%
  filter(variable == "Rain"& mean)
rain_CM18_sept <- mean_day_18_sept %>%
  filter(variable == "Rain"& mean)
rain_CM18 <- mean_day_18 %>%
  filter(variable == "Rain" & mean)
rain_CM19 <- mean_day_19 %>%
  filter(variable == "Rain"& mean)
rain_CM20 <- mean_day_20 %>%
  filter(variable == "Rain"& mean)
rain_CM21 <- mean_day_21 %>%
  filter(variable == "Rain"& mean)
rain_CM22 <- mean_day_22 %>%
  filter(variable == "Rain"& mean)
rain_CM22may <- mean_day_22_may %>%
  filter(variable == "Rain"& mean)
#Note that for 2023 there were no rain data, so this graphic is not available!
rain_CM24 <- mean_day_24 %>%
  filter(variable == "Rain"& mean)

rain_CM15$year <-2015
rain_CM16$year <-2016
rain_CM17$year <-2017
rain_CM18_sept$year <-2018
rain_CM18$year <- 2018
rain_CM19$year <- 2019
rain_CM20$year <- 2020
rain_CM21$year <- 2021
rain_CM22$year <-2022
rain_CM22may$year <-2024
rain_CM24$year <-2024

# putting together the different months of the same years
rain_CM18t <- bind_rows(
  mutate(rain_CM18, year=2018),
  mutate(rain_CM18_sept, year=2018))
rain_CM22t <- bind_rows(
  mutate(rain_CM22, year= 2022),
  mutate(rain_CM22may, year=2022))

#Creating a multiplot of daily mean precipitation
rainplot15 <-ggplot(rain_CM15, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth()+ labs (title="2015 Daily Mean precipitation",
                       x="",
                        y= "Mean precipitation")
rainplot16 <-ggplot(rain_CM16, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="2016 Daily Mean precipitation",
                        x="",
                        y= "Mean precipitation")
rainplot17 <- ggplot(rain_CM17, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="Monthly precipitation",
                        x="",
                        y= "Mean precipitation")
rainplot18 <- ggplot(rain_CM18t, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="Monthly precipitation 2018",
                        x="",
                        y= "Mean precipitation")
rainplot19 <- ggplot(rain_CM19, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="Monthly precipitation 2019",
                        x="",
                        y= "Mean precipitation")
rainplot20 <- ggplot(rain_CM20, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="Monthly precipitation 2020",
                        x="",
                        y= "Mean precipitation")
rainplot21 <- ggplot(rain_CM21, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="Monthly precipitation 2021",
                        x="",
                        y= "Mean precipitation")
rainplot22 <- ggplot(rain_CM22t, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="Monthly precipitation 2022",
                        x="",
                        y= "Mean precipitation")
rainplot24 <- ggplot(rain_CM24, aes(x=anno_mese_giorno, y=mean)) +
  geom_smooth() + labs (title="Monthly precipitation 2024",
                        x="",
                        y= "Mean precipitation")
#Using the ggarrange function
ggarrange(rainplot15, rainplot16, rainplot17, rainplot18, rainplot19, 
                    rainplot20, rainplot21, rainplot22, rainplot24 
                    + rremove("x.text"), 
          labels = c("15", "16", "17", "18", "19", "20", "21", "22", "24"),
          ncol = 3, nrow = 3) +
scale_y_continuous(limits=c(0.0,1))

rain_all_years <- bind_rows(
  mutate(rain_CM16, year = 2016, ENSO = "El Niño"),
  mutate(rain_CM17, year = 2017, ENSO = "La Niña"),
  mutate(rain_CM18_sept, year = 2018, ENSO = "La Niña"),
  mutate(rain_CM18, year = 2018, ENSO = "La Niña"),
  mutate(rain_CM19, year = 2019, ENSO = "El Niño"),
  mutate(rain_CM20, year = 2020, ENSO = "El Niño"),
  mutate(rain_CM21, year = 2021, ENSO = "La Niña"),
  mutate(rain_CM22, year = 2022, ENSO = "La Niña"),
  mutate(rain_CM22may, year = 2022, ENSO = "La Niña"),
  mutate(rain_CM24, year = 2024, ENSO = "El Niño")
)

#Classic plot, to make a comparison between all grpahics
ggplot(rain_all_years, aes(x = anno_mese_giorno, y = mean, color = ENSO, group = interaction(year, ENSO))) +
  geom_line(alpha = 0.8, na.rm = TRUE) +  # Draw lines, grouped by year and ENSO
  labs(title = "CHIANG MAI - Daily rain mean and ENSO Classification",
       x = "",
       y = "Rain mean (mm/d)",
       color = "ENSO Classification") +
  scale_color_manual(values = c("El Niño" = "red", "La Niña" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top")

str(rain_all_years)

