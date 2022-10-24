library(tidyverse)
library(lubridate)

################################################## 
# read and plot AIS analysis 
# 
hourly_AIS <- read.csv2("pedestrians/results/hourly_AIS.csv", sep = ",")

hourly_AIS %>% 
  filter(Sensor_ID < 10) %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.character, as.numeric) %>% 
  ggplot(aes(x = Year, y = AIS, color = factor(Sensor_ID))) + 
  geom_point() + geom_line() + 
  scale_color_manual(values = our_colors) 

# daily AIS
daily_AIS <- read.csv2("pedestrians/results/daily_AIS.csv", sep = ",")

daily_AIS %>% 
  filter(Year %in% c(2018, 2019, 2020, 2021)) %>% 
  # filter(Sensor_ID < 10) %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.character, as.numeric) %>% 
  ggplot(aes(x = Year, y = AIS, color = factor(Sensor_ID))) + 
  geom_point() + geom_line() + 
  scale_color_manual(values = our_colors) 


  
# old code with different format - left for lookup of conversion
# sensor_analysis %>% 
#   select(year, str_sort(names(.), numeric=TRUE)) %>% 
#   pivot_longer(matches("\\d"), names_to = "Sensor_ID", values_to = "AIS") %>% 
#   mutate_all(type.convert) %>% 
#   mutate_if(is.character, as.numeric) %>% 
#   #filter(Sensor_ID > 10) %>% 
#   filter(Sensor_ID < 30) %>% 
#   ggplot(aes(x = year, y = AIS, color = factor(Sensor_ID))) + 
#   geom_point() + geom_line() + 
#   scale_color_manual(values = our_colors) 
  


################################################## 
# analysis of MI 

hourly_MI <- read.csv2("pedestrians/results/hourly_MI_TL5.csv", sep = ",")

# compare one sensor to all others
hourly_MI %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.character, as.numeric) %>% 
  filter(time_lag == 0) %>% 
  select(-stat_sig, -time_lag) %>% 
  filter(sensor1 == 5) %>% 
  filter(MI < 1) %>% 
  unite("Sensors", sensor1, sensor2) %>% 
  ggplot(aes(x = year, y = MI, color=factor(Sensors))) +
  geom_point() + geom_line() + 
  scale_color_manual(values = our_colors) 

# look for smallest and biggest MI
# 
hourly_MI %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.character, as.numeric) %>% 
  filter(time_lag == 0) %>% 
  select(-stat_sig, -time_lag) %>% 
  # filter(sensor1 == 17) %>% 
  filter(sensor1 %in% c(9,18)) %>% 
  filter(sensor2 %in% c(9,18)) %>% 
  #filter(MI > 1.6) %>% 
  unite("Sensors", sensor1, sensor2) %>% 
  ggplot(aes(x = year, y = MI, color=factor(Sensors))) +
  geom_point() + geom_line() + 
  scale_color_manual(values = our_colors) 

hourly_MI %>% 
  mutate_all(type.convert, as.is=T) %>% 
  mutate_if(is.character, as.numeric) %>% 
  filter(time_lag == 0) %>% 
  select(-stat_sig, -time_lag) %>% 
  filter(sensor1 == 12, sensor2 == 11) %>% 
  unite("Sensors", sensor1, sensor2) %>% 
  ggplot(aes(x = year, y = MI, color=factor(Sensors))) +
  geom_point() + geom_line() 
  
  

# daily MI

daily_MI <- read.csv2("pedestrians/results/daily_MI_TL5.csv", sep = ",")

daily_MI %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.character, as.numeric) %>% 
  filter(Time_lag == 0) %>% 
  select(-Stat_sig, -Time_lag) %>% 
  # filter(Sensor1 %in% c(44,)) %>% 
  filter(Year < 2020) %>% 
  filter(MI > 1.4) %>% 
  unite("Sensors", Sensor1, Sensor2) %>% 
  ggplot(aes(x = Year, y = MI, color=factor(Sensors))) +
  geom_point() + geom_line() + 
  scale_color_manual(values = our_colors) 


################################################## 
# TE between sensors
# hourly

hourly_TE <- read.csv2("pedestrians/results/hourly_TE_TL5_dyncorr55.csv", sep = ",")

hourly_TE %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.character, as.numeric) %>% 
  #filter(Time_lag == 0) %>% 
  select(-Stat_sig) %>% 
  group_by(Year, Time_lag) %>% 
  summarise(mean = mean(TE), sd = sd(TE)) %>% 
  ggplot(aes(x = Year, y = mean, color=factor(Time_lag))) +
  geom_point() + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width=.2)) + 
  scale_color_manual(values = our_colors) 
  
hourly_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  filter(Time_lag > 0) %>% 
  select(-Stat_sig) %>% 
  arrange(desc(TE)) %>% 
  head(10)



################################################## 
# analysis of weeks

sensors_monthly_hourly_MI <- read.csv2("pedestrians/results/sensors_monthly_hourly_MI_TL5_dyncorr55.csv", sep = ",")

sensors_monthly_hourly_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  unite("Sensors", Sensor1, Sensor2) %>% 
  filter(Time_lag == 0) %>% 
  filter(MI > 4.0) %>% 
  mutate(Date = make_date(Year, Month)) %>% 
  select(-c(Stat_sig, Time_lag, Year, Month)) %>% 
  ggplot(aes(x = Date, y = MI, color=factor(Sensors))) +
  geom_line() + geom_point()
  

jan_18 <- read.csv2("/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/hourly_months/daily_date_sensor_id_refactor_1-2018.csv", sep = ",")

jan_18 %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(X31, X51)
  
  
  
  
  
################################################## 
# analysis of one week in november
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
