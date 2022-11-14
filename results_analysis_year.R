library(seriation)
library(ggdendro)

################################################## 
year_MI <- read.csv2("pedestrians/results/year/year_hourly_MI_TL5.csv", sep = ",")

year_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  filter(Sensor1 < Sensor2) -> 
  year_MI
  
  
################################################## 
# MI
year_MI %>% 
  arrange(desc(MI))


################################################## 
# MI distance plot
plot_MI_distance_correlation(data = year_MI, time_lag = 0, title = "MI against distances for time lag 0 and correlation", out_filename = "year18_MI_distances_correlation.png" )


year_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  select(-Stat_Sig) %>% 
  filter(Time_lag == 0) %>% 
  filter(Sensor1 < Sensor2) %>% 
  # combine sensor1 and sensor2 into one column sensors
  mutate(sensors = paste(Sensor1, Sensor2, sep = "_")) %>%
  select(-c(Sensor1, Sensor2, Time_lag, Year, Month, Day)) %>%
  # left join with sensor_distances
  left_join(sensor_distances, by = c("sensors" = "sensors")) %>%
  # remove rows with NA
  na.omit() -> 
  year_MI_distances

# just to see what difference these outliers do
year_MI_distances %>% 
  filter(sensors %!in% c("18_24", "18_23", "24_33", "23_33", "24_40", "9_18", "10_18")) ->
  year_MI_distances

# plot with annotation for interesting points 
year_MI_distances %>% 
  # plot with correlation line
  ggplot() +
  #ggplot(aes(x = distance, y = MI, size = Stat_Sig)) +
  geom_point(aes(x = distance, y = MI)) +
  geom_smooth(aes(x = distance, y = MI), method = "lm", se = FALSE, color = "red") +
  # add annotation to points where MI > 1.5 
  geom_text(aes(x = distance, y = MI, label = ifelse(MI>1.5, as.character(sensors), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) +
  # add annotation to points where MI > 1 and distance > 2000
  geom_text(aes(x = distance, y = MI, label = ifelse(MI>1 & distance>2000, as.character(sensors), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) +
  geom_text(aes(x = distance, y = MI, label = ifelse(MI>1.2 & distance>1500, as.character(sensors), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) +
  # add correlation value to plot
  annotate("text", x = 0.5, y = 0.5, label = paste("r = ", round(cor(year_MI_distances$distance, year_MI_distances$MI), 2))) +
  labs(x = "Distance in m", y = "MI in nats") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  #ggtitle(title) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) #->
  plot

ggsave(plot, filename = "year18_MI_distances_correlation.png", width = 10, height = 10/(16/9), units = "in")

# normalized MI with distance
year_MI_distances %>% 
  # normalize MI by distance
  mutate(MI_norm = MI/distance) %>%
  # plot with correlation line
  ggplot() +
  #ggplot(aes(x = distance, y = MI, size = Stat_Sig)) +
  geom_point(aes(x = distance, y = MI_norm)) +
  geom_smooth(aes(x = distance, y = MI_norm), method = "lm", se = FALSE, color = "red") +
  geom_text(aes(x = distance, y = MI_norm, label = ifelse(MI_norm>0.02, as.character(sensors), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) 


################################################## 
colfunc<-colorRampPalette(c("white","yellow","red","black"))

max_tl <- max(year_MI$Time_lag)

max_MI <- as.numeric(max(year_MI$MI))
min_MI <- as.numeric(min(year_MI$MI))

tl <- 0

plots <- list()
for (tl in 0:max_tl) {
  year_MI %>% 
    mutate_all(type.convert, as.is=TRUE) %>% 
    mutate_if(is.character, as.numeric) %>% 
    select(-Stat_Sig) %>% 
    #filter(Sensor1 < 10) %>% 
    #filter(Sensor2 < 10) %>% 
    #filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
    #unite(Sensors, Sensor1, Sensor2) %>% 
    filter(Time_lag == tl) %>% 
    ggplot(aes(x = Sensor1, y = Sensor2, fill = MI)) +
    geom_tile() +
    labs(x = "Sensor 1", y = "Sensor 2", fill = "MI") +
    # color scale with colfunc and min/max MI
    scale_fill_gradientn(colours = colfunc(100), limits=c(min_MI, max_MI)) +
    # color scale between blue and red
    #scale_fill_gradient(low = "white", high = "red") +
    # have a tick for all values
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
    # add title
    ggtitle(paste("MI for time lag",tl,"over the year 2018", sep = " ")) +
    # add legend
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) #-> 
    plot
  
  plots[[tl+1]] <- plot
}

# save plots to pdf
pdf("year_MI_heatmap.pdf", width = 10, height = 10/(4/3))
for (i in 1:length(plots)) {
  print(plots[[i]])
}
dev.off()




year_MI %>%
  mutate_all(type.convert, as.is = TRUE) %>%
  mutate_if(is.character, as.numeric) %>%
  filter(Time_lag == 0) -> year_MI_filtered
  
year_MI_filtered

pimage(dist_result, main = "Random Order")




# plot with locals

year_MI_locals <- read.csv2("pedestrians/year_hourly_MI_TL5_locals.csv", sep = ",", header = TRUE)

year_MI_locals %>%
  mutate_all(type.convert, as.is = TRUE) %>%
  #mutate_if(is.character, as.numeric) %>%
  filter(Time_lag == 0) -> local_mi 

# get only one column if knowing which one it is
local_mi <- as.numeric(strsplit(local_mi$Local_MI[1], ",")[[1]])

local_mi %>%
  as.data.frame() %>%
  mutate(row = row_number()) -> local_mi_df

data %>%
  filter(Year == 2018) %>% 
  #filter(Sensor_ID %in% c(8, 15)) %>% 
  select(Date_Time) %>%
  distinct() %>%
  arrange(Date_Time) %>%
  mutate(row = row_number()) -> date_time_index

data %>% 
  filter(Year == 2018) %>% 
  filter(Sensor_ID %in% c(8, 15)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  arrange(Date_Time) %>%
  # combine date_time_index by Date_Time
    left_join(date_time_index, by = "Date_Time") %>%
    left_join(local_mi_df, by = "row") %>% 
  rename(local_mi = ".") %>% 
  # replace local_te between 1 and -1 with NA 
  mutate(local_mi = ifelse(local_mi < 2 & local_mi > -0.5, NA, local_mi)) %>%
  filter(Date_Time < "2018-02-15") %>% 
  ggplot() +
  geom_line(aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) +
  geom_point(aes(x = Date_Time, y = local_mi*1000, color="MI"), color = "red", shape = "x", size = 3) +
  geom_text(aes(x = Date_Time, y = local_mi*1000, label = ifelse(local_mi>3.5, as.character(Date_Time), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name="MI")) +
    labs(title = "Hourly counts for sensors 8 and 15 in February 2018 with local MI", x = "Date", y = "Hourly counts", color = "Sensor") +
  scale_color_manual(values = our_colors)

# in numbers
data %>% 
  filter(Year == 2018) %>% 
  #filter(Sensor_ID %in% c(8, 15)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  arrange(Date_Time) %>%
  # get hour from Date_Time
  mutate(Hour = hour(Date_Time)) %>%
  # combine date_time_index by Date_Time
  left_join(date_time_index, by = "Date_Time") %>%
  left_join(local_mi_df, by = "row") %>% 
  rename(local_mi = ".")


# python prepared df file 

all_locals_MI <- read.csv2("pedestrians/local_MI_2018_TL0.csv", sep = ",", header = TRUE)


all_locals_MI %>%
  mutate_all(type.convert, as.is = TRUE) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(row = row_number()) %>%
  # left join with date_time_index
  left_join(date_time_index, by = "row") %>%
  select(-row) %>% 
  # get hour from Date_Time
  mutate(Hour = hour(Date_Time)) %>%
  # melt to long format
  gather(key = "Sensor_ID", value = "Local_MI", -Date_Time, -Hour) %>%
  #filter(Sensor_ID == "X1_2") %>% 
  # remove X from Sensor_ID
  mutate(Sensor_ID = gsub("X", "", Sensor_ID)) %>%
  #filter(Sensor_ID %in% c("1_2") %>% 
  # split Sensor_ID into Sensor1 and Sensor2
  separate(Sensor_ID, c("Sensor1", "Sensor2"), sep = "_") %>% 
  filter(Sensor1 < Sensor2) %>%
  #filter(Sensor1 == "1") %>% 
  # combine Sensor1 and Sensor2 into one column
  mutate(Sensors = paste(Sensor1, Sensor2, sep = "_")) -> all_locals_MI_state

all_locals_MI_state %>% 
  filter(Sensors == "1_2") %>% 
  select(-c(Sensor1, Sensor2)) %>% 
  # get month from Date_Time
  mutate(Month = month(Date_Time)) %>%
  filter(Month == 11) %>% 
  ggplot() +
  geom_line(aes(x = Date_Time, y = Local_MI, color = factor(Hour))) +
  labs(title = "Local MI for sensors 1 and 2 in November 2018", x = "Date", y = "Local MI", color = "Hour") +
  scale_color_manual(values = our_colors)



%>% 
  ggplot() +
  geom_line(aes(x = Hour, y = Local_MI_mean, color = Sensors)) +
  geom_ribbon(aes(x = Hour, ymin = Local_MI_min, ymax = Local_MI_max, fill = Sensors), alpha = 0.2) +
  labs(title = "Mean and variance of local MI for all sensor pairs in 2018", x = "Hour", y = "Local MI", color = "Sensor pair", fill = "Sensor pair") +
  scale_color_manual(values = our_colors) +
  scale_fill_manual(values = our_colors) +
  theme(legend.position = "bottom")


#%>%
  group_by(Sensors, Hour) %>% 
  # summarize mean, var, max and min 
  summarise(Local_MI_mean = mean(Local_MI), Local_MI_var = var(Local_MI), Local_MI_max = max(Local_MI), Local_MI_min = min(Local_MI)) ->
  summarised_locals

#%>% view()


  
# analyze summarised locals
summarised_locals %>% 
  ungroup() %>% 
  arrange(desc(Local_MI_max)) %>% 
  slice(1:100) %>% 
  count(Hour) #%>% 
  write.csv2("pedestrians/local_MI_max_hour.csv", row.names = F)
 
summarised_locals %>% 
  ungroup() %>% 
  arrange((Local_MI_min)) %>% 
  slice(1:100) %>% 
  count(Hour) #%>% 
  write.csv2("pedestrians/local_MI_min_hour.csv", row.names = F)

# better approach:
summarised_locals %>% 
  group_by(Sensors) %>% 
  arrange(desc(Local_MI_max)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(desc(Local_MI_max)) %>% 
  slice(1:100) %>% 
  count(Hour) %>% 
  write.csv2("pedestrians/local_MI_max_hour.csv", row.names = F)
  
summarised_locals %>% 
  group_by(Sensors) %>% 
  arrange((Local_MI_min)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange((Local_MI_min)) %>% 
  slice(1:100) %>% 
  count(Hour) %>% 
  write.csv2("pedestrians/local_MI_min_hour.csv", row.names = F)
   
# plot sensor sensor xx and yy for 2018 with local
all_locals_MI %>%
  mutate_all(type.convert, as.is = TRUE) %>%
  mutate_if(is.character, as.numeric) %>%
  select(X1_2) %>% 
  mutate(row = row_number()) %>%
  # left join with date_time_index
  left_join(date_time_index, by = "row") -> locals_xx_yy
  
   
data %>% 
  filter(Year == 2018) %>% 
  filter(Sensor_ID %in% c(1, 2)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  arrange(Date_Time) %>%
  # combine date_time_index by Date_Time
  left_join(date_time_index, by = "Date_Time") %>% 
  left_join(locals_xx_yy, by = "row") %>% 
  rename(local_mi = X1_2, Date_Time = Date_Time.x) %>% 
  select(-Date_Time.y) %>% 
  mutate(local_mi = ifelse(local_mi < 3 & local_mi > -1, NA, local_mi)) %>%
  
  ggplot() +
  geom_line(aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) +
  geom_point(aes(x = Date_Time, y = local_mi*1000, color="MI"), color = "red", shape = "x", size = 3) +
  geom_text(aes(x = Date_Time, y = local_mi*1000, label = ifelse(local_mi>3.5, as.character(Date_Time), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) +
  geom_text(aes(x = Date_Time, y = local_mi*1000, label = ifelse(local_mi<(-3.5), as.character(Date_Time), "")), hjust = -.1, vjust = -.1, position = position_dodge(width = 2), size = 2) +
  # vertical line on 18-11-2018 at 4 pm
  #geom_vline(xintercept = as.POSIXct("2018-11-09 16:00:00"), linetype = "dashed", color = "black") +
  #geom_vline(xintercept = as.POSIXct("2018-11-08 06:00:00"), linetype = "dashed", color = "black") +
  #geom_vline(data = . %>% filter(Date_Time == as_datetime("2018-11-09 16:00:00")), aes(xintercept = Date_Time), color = "black", linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name="MI")) +
    labs(title = "Hourly counts for sensors 1 and 2 in 2018 with local MI", x = "Date", y = "Hourly counts", color = "Sensor") +
  scale_color_manual(values = our_colors) #-> plot

# save plot in very wide format
ggsave("pedestrians/2018_1_2_vline_MI.pdf", plot, width = 200, height = 5, dpi = 300, limitsize = FALSE)

  
# find the hour where there is the highest MI 

  
  
  
  
  
  