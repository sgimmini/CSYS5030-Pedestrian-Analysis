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
  
  
  
  
  
