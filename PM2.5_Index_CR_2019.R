#######################################################################
# We filter PM2.5 in 4 main categories, following the criteria of
# good quality 0<x<37 
# fair quality 38<x<51
# poor quality 59<x<91 
# dangerous quality x>91 , where x= [pm2.5]
# Applying it to our all years dataset and by subgrouping them into El Nino and La Nina years

pm25_CR19%>%
  group_by(Date = strftime(anno_mese_giorno, "%Y-%m")) %>%
  summarize(
    Good = length(variable[mean<=37]),
    Fair = length(variable[mean>37 & mean<=50]),
    Poor = length(variable[mean>50 & mean<= 90]),
    Very_poor = length(variable[mean>90])) %>%
 pivot_longer(cols=!Date,
              cols_vary = "fastest", 
              names_to = "PM2.5 Quality Index", 
              values_to = "N. of days"
) %>%
ggplot(aes(fill=`PM2.5 Quality Index`, y=`N. of days`, x=Date)) +
  geom_bar(position="stack", stat="identity") +
        scale_fill_manual(values = c(
          "Good" = "#56B4E9", 
          "Fair" = "#009E73",     
          "Poor" = "#F0E442",     
          "Very_poor" = "#D55E00"
        )) +
          theme_minimal() +
          theme( plot.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          labs(title = "PM2.5 Quality Index in Chiang Rai city, 2019",
               y = "N. of days",
               x = "",
               fill = "PM2.5 Index") +
  scale_x_discrete(labels=c("January","February","March","April", "May", "June",
                            "July", "August", "September", "October", "November",
                            "December")) 
  
# Acknolegments : https://aaqr.org/articles/aaqr-23-12-oa-0321
