library(tidyverse)

################################################## 
# weekly view (MI over one week)

week_in_nov <- read.csv2("/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/results/week_in_11_MI.csv", sep = ",")

week_in_nov %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Year = 2018) %>% 
  # mutate(Date = make_date(Year, Month)) %>% 
  # setting filters
  filter(Time_lag == 0) %>% 
  filter(Sensor1 < Sensor2) %>%
  #filter(Sensor1 < 10) %>% 
  #filter(Sensor2 < 10) %>% 
  #filter(MI > 2.0) %>% 
  select(-c(Stat_sig, Year, Month, Time_lag)) %>% 
  # heatmap with Sensor1 on x axis, Sensor2 on y axis, and MI as color
  ggplot(aes(x = Sensor1, y = Sensor2, fill = MI)) +
  geom_tile() +
  labs(x = "Sensor 1", y = "Sensor 2", fill = "MI") +
  # color scale between blue and red
  scale_fill_gradient(low = "white", high = "red") +
  # have a tick for all values
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

week_in_nov %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Year = 2018) %>% 
  # mutate(Date = make_date(Year, Month)) %>% 
  # setting filters
  filter(Time_lag == 0) %>% 
  #filter(Sensor1 < 10) %>% 
  #filter(Sensor2 < 10) %>% 
  #filter(MI > 1.0) %>% 
  arrange(desc(MI))


# daily view 

daily_week_in_nov <- read.csv2("/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/results/week/week_daily_hourly_MI_TL5.csv", sep = ",")


daily_week_in_nov %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(-Stat_Sig) %>% 
  #filter(Sensor1 < 10) %>% 
  #filter(Sensor2 < 10) %>% 
  filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  #unite(Sensors, Sensor1, Sensor2) %>% 
  unite(Date, Year, Month, Day, sep = "-") %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Time_lag == 1) %>% 
  ggplot(aes(x = Sensor1, y = Sensor2, fill = MI)) +
  geom_tile() +
  labs(x = "Sensor 1", y = "Sensor 2", fill = "MI") +
  # color scale between blue and red
  scale_fill_gradient(low = "white", high = "red") +
  # have a tick for all values
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

# not soooo good 
daily_week_in_nov %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(-Stat_Sig) %>% 
  #filter(Sensor1 < 10) %>% 
  #filter(Sensor2 < 10) %>% 
  filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(MI > 1.2) %>% 
  filter(Time_lag == 0) %>% 
  unite(Sensors, Sensor1, Sensor2) %>% 
  unite(Date, Year, Month, Day, sep = "-") %>% 
  mutate(Date = as.Date(Date)) %>% 
  ggplot(aes(x = Date, y = MI, color = factor(Sensors))) +
  geom_line() + geom_point()
  


# find the 3 most important sensor pairs for each day
daily_week_in_nov %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(-Stat_Sig) %>% 
  filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(Time_lag == 0) %>% 
  unite(Sensors, Sensor1, Sensor2) %>% 
  unite(Date, Year, Month, Day, sep = "-") %>% 
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date) %>% 
  arrange(desc(MI)) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  ggplot(aes(x = Date, y = MI, color = factor(Sensors))) +
  # geom_point with big size
  geom_point(size = 3) + geom_line() +
  #geom_point() + 
  # use our_colors to color the lines
  scale_color_manual(values = our_colors) +
  # use Mon - Sun as x-axis labels for each tick
  scale_x_date(date_breaks="1 day", date_labels="%a") 


# saving plots
daily_week_in_nov <- read.csv2("/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/results/week/week_daily_hourly_MI_TL10.csv", sep = ",")
plots <- list()
# get max TL from data
max_tl <- max(daily_week_in_nov$Time_lag)
# iterate over time lags
for (tl in 0:max_tl) {
  # find the 5 most important sensor pairs for each day and save them into an array
  daily_week_in_nov %>% 
    mutate_all(type.convert, as.is=TRUE) %>% 
    mutate_if(is.character, as.numeric) %>% 
    #select(-Stat_Sig) %>% 
    filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
    filter(Time_lag == tl) %>% 
    unite(Sensors, Sensor1, Sensor2) %>% 
    unite(Date, Year, Month, Day, sep = "-") %>% 
    mutate(Date = as.Date(Date)) %>% 
    group_by(Date) %>% 
    arrange(desc(MI)) %>% 
    slice(1:2) %>% 
    ungroup() %>% 
    select(Sensors) %>% 
    unique() -> most_important_sensor_pairs


  daily_week_in_nov %>% 
    mutate_all(type.convert, as.is=TRUE) %>% 
    mutate_if(is.character, as.numeric) %>% 
    #select(-Stat_Sig) %>% 
    filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
    filter(Time_lag == 1) %>% 
    unite(Sensors, Sensor1, Sensor2) %>% 
    unite(Date, Year, Month, Day, sep = "-") %>% 
    mutate(Date = as.Date(Date)) %>% 
    # filter by Sensors if they are in most_important_sensor_pairs
    filter(Sensors %in% most_important_sensor_pairs$Sensors) %>%
    ggplot(aes(x = Date, y = MI, color = factor(Sensors))) +
    # geom_point with size determined by Stat_Sig, where 0 is the biggest 
    #geom_point(aes(size = Stat_Sig)) + geom_line() +
    geom_point(size = 3) + geom_line() +
    #geom_point() + 
    # use our_colors to color the lines
    scale_color_manual(values = our_colors) +
    # use Mon - Sun as x-axis labels for each tick
    scale_x_date(date_breaks="1 day", date_labels="%a") +
    # legends
    guides(color = guide_legend(title = "Sensor Pairs")) +
    # title with time lag
    labs(title = paste("Mutual Information of Sensor Pairs for Week in November for TL: ", tl)) +
    xlab("Date") + ylab("Mutual Information") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") ->
    plot

  plots[[tl+1]] <- plot
}

# save plots to pdf
pdf("time_lags_with_stat_sig.pdf", width = 10, height = 10/(4/3))
for (i in 1:length(plots)) {
  print(plots[[i]])
}
dev.off()

################################################## 