
pm25CR_combined %>%
  mutate(
    year = format(anno_mese_giorno, "%Y"), 
    month = format(anno_mese_giorno, "%B"),
    day = as.integer(format(anno_mese_giorno, "%d")),
    PM2.5QualityIndex = case_when(
      mean <= 37 ~ "Good",
      mean <= 50 ~ "Fair",
      mean <= 90 ~ "Poor",
      mean > 90 ~ "Very_poor"
    ),
    month = factor(month, levels = month.name)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = month, y = day, fill = PM2.5QualityIndex)) +  # <-- mapping corretto
  geom_tile(color = "white") +
  facet_wrap(~ year, nrow = 1) +
  scale_fill_manual(values = c(
    "Good" = "#56B4E9", 
    "Fair" = "#009E73", 
    "Poor" = "#F0E442", 
    "Very_poor" = "#D55E00"
  )) +
  scale_y_continuous(breaks = 1:31) +
  labs(
    title = "PM2.5 Daily Quality Index per Year",
    x = "Month",
    y = "Day of the Month",
    fill = "PM2.5 Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(face = "bold")
  )
