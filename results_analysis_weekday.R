

################################################## 
# MI
weekdays_MI <- read.csv2("pedestrians/results/weekdays/weekday_hourly_MI_TL5.csv", sep = ",")
weekdays_MI <- read.csv2("pedestrians/results/weekdays/weekday_hourly_MI_TL5_stat_sig.csv", sep = ",")

# get the most important sensor pairs for each day 

# cleaning
weekdays_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  #select(-Stat_Sig) %>% 
  filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(Time_lag == 0) %>% 
  unite(Sensors, Sensor1, Sensor2) %>% 
  mutate(Month = 1) %>% 
  unite(Date, Year, Month, Day, sep = "-") %>% 
  mutate(Date = as.Date(Date)) ->
  weekdays_MI

# get the ones with highest MI
weekdays_MI %>% 
  group_by(Day) %>% 
  top_n(1, MI) %>% 
  ungroup() %>%
  select(Sensors) %>%
  unique() -> top_sensor_pairs

# get the ones with lowest MI 
weekdays_MI %>% 
  group_by(Day) %>% 
  top_n(1, desc(MI)) %>% 
  ungroup() %>%
  select(Sensors) %>%
  unique() -> low_sensor_pairs

weekdays_MI %>% 
  filter(Sensors == "18_24") %>% 
  arrange(MI)

# plot 
weekdays_MI %>% 
  filter(Sensors %in% c(top_sensor_pairs$Sensors)) %>%
  #filter(Sensors %in% c(low_sensor_pairs$Sensors)) %>%
  #filter(Sensors %in% c("9_36", "1_2")) %>%
  ggplot(aes(x = Day, y = MI, color = factor(Sensors))) +
  # geom_point with big size
  geom_point(size = 3) + geom_line() +
  #geom_point() + 
  # use our_colors to color the lines
  scale_color_manual(values = our_colors) +
  # use Mon - Sun as x-axis labels for each tick
  #scale_x_date(date_breaks="1 day", date_labels="%a") +
  # title 
  labs(title = "MI for weekdays", x = "Weekday", y = "MI in nats", color = "Sensor pairs") 

# -> 9 and 36 have a very high MI throughout the week and a low on sat

# so plot them, indicating saturday
data %>%
  filter(Year == 2018, Month %in% c(11)) %>% 
  filter(Sensor_ID %in% c(9, 36)) %>%
  select(Date_Time, Date, Sensor_ID, Hourly_Counts) %>%
  group_by(Date_Time, Sensor_ID) %>%
  #summarise(Daily_Counts = sum(Hourly_Counts)) %>%
  ungroup() %>%
  arrange(Date_Time) %>%
  # get weekday from Date
  mutate(Weekday = weekdays_MI(Date_Time)) %>% 
  mutate(Hour = hour(Date_Time)) %>%
  group_by(Sensor_ID) %>% 
  #ggplot(aes(x = Date, y = Daily_Counts, color = factor(Sensor_ID))) +
  ggplot(aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) +
  # increase line thickness
  geom_line(size = 1) +
  # plot vertical lines for weekends using column Weekday
  geom_vline(data = . %>% filter(Weekday %in% c("Sunday", "Saturday"), Hour == 0), aes(xintercept = Date_Time), color = "black", linetype = "dashed") +
  # color values by our_colors 1, 3, 5, 6
  scale_color_manual(values = c(our_colors[1], our_colors[3], our_colors[5], our_colors[6])) +
  #scale_color_manual(values = our_colors) +
  labs(x = "Date Time", y = "Hourly Counts", color = "Sensor ID") +
  ggtitle("Hourly counts in November 2018") #-> plot

ggsave("november18_hourlycounts_9-36.pdf", plot = plot, width = 50, height = 10, limitsize = F)

# also heatmap over time of the day and weekday
data %>% 
  filter(Year == 2018, Month == 11) %>% 
  filter(Sensor_ID == 36) %>% 
  # get the sum of Hourly_Counts for each hour of each day of the week
  group_by(Day, Time) %>%
  summarise(Hourly_Counts = sum(Hourly_Counts)) %>%
  ungroup() %>% 
  # order weekdays by day of the week
  mutate(Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot(aes(x = Time, y = Day, fill = Hourly_Counts)) +
  geom_tile(color="black") +
  scale_fill_gradientn(colours = colfunc(100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Time", y = "Day", fill = "Hourly Counts") +
  ggtitle("Pedestrian Counts for Sensor 36 in 11.2018") -> plot 

# save
ggsave("november18_heatmap_36.pdf", plot = plot, width = 10, height = 10/(16/9), limitsize = F)
  


# get sensors from top3_sensor_pairs and put into array
top_sensor_pairs$Sensors %>% strsplit(split = "_") %>% unlist() -> top_sensors

data %>%
  filter(Year == 2018) %>% 
  filter(Sensor_ID %in% top_sensors) %>%
  select(Date, Sensor_ID, Hourly_Counts) %>%
  group_by(Date, Sensor_ID) %>%
  summarise(Daily_Counts = sum(Hourly_Counts)) %>%
  ungroup() %>%
  arrange(Date) %>%
  group_by(Sensor_ID) %>%
  mutate(total = rollmean(Daily_Counts, 7)) %>%
  ggplot(aes(x = Date, y = total, color = factor(Sensor_ID))) +
  geom_line()

# average MI over days for each sensor pair
weekdays_MI %>% 
  group_by(Sensors) %>% 
  summarise(Average_MI = mean(MI)) %>% 
  ungroup() %>% 
  arrange(desc(Average_MI)) %>% view()

plot_daily_sensor_counts(data = data, sensors = c(21, 46), title = "test", year = 2018, month = 11)

# DONE: sensors with lowest MI 
plot_hourly_sensor_counts(data = data, sensors = c(21, 43), title = "Sensors with the lowest averaged MI", year = 2018, month = 11, day = c(1:12), out_filename = "november_lowest_MI_pair.pdf")

# DONE: sensors with highest MI 
plot_hourly_sensor_counts(data = data, sensors = c(1, 2), title = "Sensors with the highest averaged MI", year = 2018, month = 11, day = c(1:12), out_filename = "november_highest_MI_pair.pdf")
plot_hourly_sensor_counts(data = data, sensors = c(4, 45), title = "Sensors with the highest averaged MI", year = 2018, month = 11, day = c(1:12))


plot_hourly_sensor_counts(data = data, sensors = c(1, 2, 9, 36, 18, 24), title = "Different sensors in November", year = 2018, month = 11, day = c(09:20))#, out_filename = "november_highest_MI_pair.pdf")

################################################## 
# for every sensor pair the biggest difference in MI and the weekdays it occured on

weekdays_MI %>% 
  group_by(Sensors) %>% 
  filter(Time_lag %in% c(0,1)) %>% 
  summarise(max_MI = max(MI), max_MI_date = Date[which.max(MI)], min_MI = min(MI), min_MI_date = Date[which.min(MI)]) %>% 
  mutate(difference = max_MI - min_MI) %>%
  ungroup() %>% 
  # arrange(desc(max_MI)) %>% 
  mutate(max_MI_date = as.Date(max_MI_date)) %>% 
  mutate(max_MI_date = format(max_MI_date, "%a")) %>% 
  mutate(max_MI_date = factor(max_MI_date, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  mutate(min_MI_date = as.Date(min_MI_date)) %>%
  mutate(min_MI_date = format(min_MI_date, "%a")) %>%
  mutate(min_MI_date = factor(min_MI_date, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  mutate(min_max_MI_date = paste(min_MI_date, max_MI_date, sep = " - ")) %>%
  arrange(difference) %>% 
  top_n(100, difference) %>% 
  count(min_max_MI_date) %>% 
  arrange(n)# %>%
  # save in csv
  write.csv2(file = "weekday_MI_2018_day_diff.csv", row.names = FALSE)


  
  # plot
  filter(difference > .75) %>% 
  ggplot(aes(x = reorder(Sensors, difference), y = difference, fill = min_max_MI_date)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = our_colors) +
  labs(x = "Sensor pairs", y = "Difference in MI in nats", fill = "Weekdays", title = "Highest difference in MI of sensor pairs and their weekday pair") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) #-> the_plot


# save the_plot in a very high resolution pdf
ggsave("pedestrians/results/weekdays/weekday_MI_difference.png", the_plot, width = 10, height = 10, units = "in", dpi = 300, limitsize = FALSE)

    


################################################## 
# plot heatmap for weekday MI

plot_MI_heatmap(weekdays_MI, "weekdays_MI_heatmap.pdf")

colfunc<-colorRampPalette(c("white","yellow","red","black"))

max_tl <- max(weekdays_MI$Time_lag)

max_MI <- as.numeric(max(as.numeric(weekdays_MI$MI)))
min_MI <- as.numeric(min(as.numeric(weekdays_MI$MI)))

weekdays_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(-Stat_Sig) %>% 
  #filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(Time_lag == 0) %>% 
  ggplot(aes(x = Sensor1, y = Sensor2, fill = MI)) +
  geom_tile() +
  labs(x = "Sensor 1", y = "Sensor 2", fill = "MI") +
  scale_fill_gradientn(colours = colfunc(100), limits=c(min_MI, max_MI)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  ggtitle(paste("MI for time lag",0, sep = " ")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) 


################################################## 
# plot MI according to distance

weekdays_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  #select(-Stat_Sig) %>% 
  filter(Time_lag == 0) %>% 
  filter(Sensor1 < Sensor2) %>% 
  filter(Day == 2) %>% 
  # combine sensor1 and sensor2 into one column sensors
  mutate(sensors = paste(Sensor1, Sensor2, sep = "_")) %>%
  select(-c(Sensor1, Sensor2, Time_lag, Year, Month, Day)) %>%
  # left join with sensor_distances
  left_join(sensor_distances, by = c("sensors" = "sensors")) %>%
  # remove rows with NA
  na.omit() %>%
  # plot with Stat_Sig as size
  ggplot(aes(x = distance, y = MI)) +
  #ggplot(aes(x = distance, y = MI, size = Stat_Sig)) +
  geom_point() +
  # add line for correlation
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Distance in m", y = "MI in nats") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  ggtitle("MI for time lag 0 with correlation") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) 

# for analyis
weekdays_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  #select(-Stat_Sig) %>% 
  filter(Time_lag == 0) %>% 
  filter(Sensor1 < Sensor2) %>% 
  filter(Day == 2) %>% 
  # combine sensor1 and sensor2 into one column sensors
  mutate(sensors = paste(Sensor1, Sensor2, sep = "_")) %>%
  select(-c(Sensor1, Sensor2, Time_lag, Year, Month, Day)) %>%
  # left join with sensor_distances
  left_join(sensor_distances, by = c("sensors" = "sensors")) %>%
  # remove rows with NA
  na.omit() %>%
  arrange(desc(distance))


 
################################################## 
################################################## 
################################################## 
################################################## 
# AIS

weekday_AIS <- read.csv2("pedestrians/results/weekdays/weekday_hourly_AIS_stat_sig.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

weekday_AIS %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Month = 1) ->
  # mutate Day 1-7 to Weekdays Mon-Sun
  # mutate(Day = factor(Day, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) ->
  weekday_AIS
  
weekday_AIS %>%
  # for each day find the the 3 sensors with the highest AIS
  group_by(Sensor) %>%
  arrange(desc(AIS)) %>%
  top_n(1, AIS) %>%
  ungroup() %>% #view()
  select(Sensor) %>% 
  unique() -> weekday_top3_AIS

# find sensors with the highest AIS difference between weekdays 
weekday_AIS %>% 
  group_by(Sensor) %>%
  summarise(AIS_diff = max(AIS) - min(AIS)) %>%
  arrange(desc(AIS_diff)) %>%
  top_n(3, AIS_diff) %>%
  ungroup() %>%
  select(Sensor) -> weekday_top_AIS_diff

# average AIS per day
weekday_AIS %>% 
  group_by(Day) %>% 
  summarise(avg_AIS = mean(AIS))

# find sensors with the smallest AIS difference between weekdays 
weekday_AIS %>% 
  group_by(Sensor) %>%
  summarise(AIS_diff = max(AIS) - min(AIS)) %>%
  arrange(AIS_diff) %>%
  # use slice 
  slice(1:3) %>%
  ungroup() %>%
  select(Sensor) -> weekday_top_AIS_diff_small

weekday_AIS %>% 
  #filter(Sensor %in% weekday_top_AIS$Sensor) %>% 
  #filter(Sensor %in% weekday_top_AIS_diff$Sensor) %>% 
  #filter(Sensor %in% weekday_top_AIS_diff_small$Sensor) %>% 
  filter(Sensor %in% c(weekday_top_AIS_diff_small$Sensor, weekday_top_AIS_diff$Sensor)) %>% 
  # plot with day on x axis, ais on y axis and color for each sensor, with Stat_Sig indicating size 
  ggplot(aes(x = Day, y = AIS, color = factor(Sensor))) +
  geom_line() + 
  geom_point() + 
  labs(x = "Weekday", y = "AIS in nats", color = "Sensor", size = "Stat. Sig.") +
  scale_color_manual(values = our_colors) +
  # have weekdays on x axis in order Mon-Sun
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  # add title
  ggtitle("AIS for weekdays") -> plot

# save
ggsave("weekday_AIS_2018_min_max.png", plot=plot, width = 10, height = 5, units = "in", dpi = 300, limitsize = FALSE)

plot_sensor_counts(data = data, sensors = c(1, 10), title="", year = 2018, month = c(11, 12))

# get table 
weekday_AIS %>% 
  #filter(Sensor %in% weekday_top_AIS$Sensor) %>% 
  #filter(Sensor %in% weekday_top_AIS_diff$Sensor) %>% 
  #filter(Sensor %in% weekday_top_AIS_diff_small$Sensor) %>% 
  filter(Sensor %in% c(weekday_top_AIS_diff_small$Sensor, weekday_top_AIS_diff$Sensor)) %>% 
  select(-c(Year, Month, Stat_Sig)) %>% 
  pivot_wider(names_from = Day, values_from = AIS) %>%
  # sort columns 
  rename(Monday = '1', Tuesday = '2', Wednesday = '3', Thursday = '4', Friday = '5', Saturday = '6', Sunday = '7') %>%
  select(Sensor, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>%
  # save in csv
  write.csv2(file = "weekday_AIS_2018_min_max.csv", row.names = FALSE)


# plot hourly counts for november 2018 for the sensors determined before
data %>%
  filter(Year == 2018, Month %in% c(11)) %>% 
  filter(Sensor_ID %in% c(1, 18, 19, 9)) %>%
  select(Date_Time, Date, Sensor_ID, Hourly_Counts) %>%
  group_by(Date_Time, Sensor_ID) %>%
  #summarise(Daily_Counts = sum(Hourly_Counts)) %>%
  ungroup() %>%
  arrange(Date_Time) %>%
  # get weekday from Date
  mutate(Weekday = weekdays(Date_Time)) %>% 
  mutate(Hour = hour(Date_Time)) %>%
  group_by(Sensor_ID) %>% 
  #ggplot(aes(x = Date, y = Daily_Counts, color = factor(Sensor_ID))) +
  ggplot(aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) +
  # increase line thickness
  geom_line(size = 1) +
  # plot vertical lines for weekends using column Weekday
  geom_vline(data = . %>% filter(Weekday %in% c("Monday", "Saturday"), Hour == 0), aes(xintercept = Date_Time), color = "black", linetype = "dashed") +
  # color values by our_colors 1, 3, 5, 6
  scale_color_manual(values = c(our_colors[1], our_colors[3], our_colors[5], our_colors[6])) +
  #scale_color_manual(values = our_colors) +
  labs(x = "Date Time", y = "Hourly Counts", color = "Sensor ID") +
  ggtitle("Hourly counts in November 2018") #-> plot

# save plot in a very wide format
ggsave("november18_hourlycounts_1-9-18-19.pdf", plot = plot, width = 50, height = 10, limitsize = F)


################################################## 
# TE

weekday_TE <- read.csv2("pedestrians/results/weekdays/weekday_hourly_TE_TL5.csv", sep = ",")


weekday_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  #filter(Sensor2 == 15) %>% 
  filter(Time_lag == 1) %>% 
  arrange(desc(TE)) %>% 
  head(10)



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # TODO: local transfer entropy for Feb between 8 and 15

data %>% 
  filter(Year == 2018, Month == 2) %>% 
  filter(Sensor_ID %in% c(8, 15)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  group_by(Date_Time, Sensor_ID) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = Sensor_ID, values_from = Hourly_Counts) %>% 
  select(-row) %>% 
  arrange(Date_Time) %>% 
  select(Date_Time, str_sort(names(.), numeric=TRUE)) %>% 
  ungroup() %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(-Date_Time) %>% 
  write_csv(file="pedestrians/datetime_sensor_id_eight_fifteen_2-2018.csv")


# plot with locals

feb_locals <- read.csv2("pedestrians/results/month/feb18_sensor-8-15_hourly_TE_TL5_locals_stat_sig.csv", sep = ",")

feb_locals %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  # mutate_if(is.character, as.numeric) %>% 
  filter(Sensor1 == 8, Time_lag == 1) %>%
  # mutate Local_TE to a numeric array
  select(Local_TE) -> local_te

  mutate(Local_TE = as.numeric(strsplit(Local_TE[1], ",")[[1]])) 

  
local_te_num <- as.numeric(strsplit(local_te$Local_TE[1], ",")[[1]])

# make data frame from local_te_num with column as row index
local_te_num %>% 
  as.data.frame() %>% 
  mutate(row = row_number()) -> local_te_df

data %>% 
  filter(Year == 2018, Month == 2) %>% 
  filter(Sensor_ID %in% c(8, 15)) %>% 
  select(Date_Time) %>%
  distinct() %>%
  mutate(row = row_number()) -> date_time_index

data %>% 
  filter(Year == 2018, Month == 2) %>% 
  filter(Sensor_ID %in% c(8, 15)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  arrange(Date_Time) %>%
  # combine date_time_index by Date_Time
    left_join(date_time_index, by = "Date_Time") %>%
  # combine with local_te_df
    left_join(local_te_df, by = "row") %>% 
  # plot with local_te_num as x markers
  rename(local_te = ".") %>% 
  # replace local_te between 1 and -1 with NA 
  mutate(local_te = ifelse(local_te < 1 & local_te > -0.5, NA, local_te)) %>%
  filter(Date_Time < "2018-02-15") %>% 
  ggplot() +
  geom_line(aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) +
  geom_point(aes(x = Date_Time, y = local_te*1000, color="TE"), color = "red", shape = "x", size = 3) +
  geom_text(aes(x = Date_Time, y = local_te*1000, label = ifelse(local_te>1.5, as.character(Date_Time), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name="TE")) +
    labs(title = "Hourly counts for sensors 8 and 15 in February 2018 with local TE", x = "Date", y = "Hourly counts", color = "Sensor") +
  scale_color_manual(values = our_colors) -> plot

ggsave("sensor8-15_feb18_local_te_wtext.png", plot=plot, width = 10, height = 5, units = "in", dpi = 300, limitsize = FALSE)

  


