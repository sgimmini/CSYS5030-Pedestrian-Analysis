

################################################## 
# TE

months_TE <- read.csv2("pedestrians/results/months/months_hourly_TE_TL5.csv", sep = ",")


months_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  # filter(Sensor2 == 28) %>% 
  # filter(Time_lag == 1) %>% 
  arrange(desc(TE)) %>% 
  head(10) # %>% 
  # summarize the number of occurences  
  count(Month) 
 
# for 8 -> 15 look the other way around 
months_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  # filter(Sensor2 == 8) %>% 
  # filter(Sensor1 == 15) %>% 
  filter(Time_lag == 1) %>% 
  arrange(desc(TE)) %>% 
  head(10) 
 


plot <- plot_hourly_sensor_counts(data = data, sensors = c(8, 15), "8 to 15 TL 1", year = 2018, month = 2)   

plot_hourly_sensor_counts(data = data, sensors = c(35, 15), "35 to 15 TL 1", year = 2018, month = 2)  

plot_hourly_sensor_counts(data = data, sensors = c(8, 2), "8 to 2 TL 1", year = 2018, month = 1)  

plot_hourly_sensor_counts(data = data, sensors = c(33, 15), "8 to 2 TL 1", year = 2018, month = 2)  

plot_hourly_sensor_counts(data = data, sensors = c(35, 2), "8 to 2 TL 1", year = 2018, month = 1)  

plot <- plot_hourly_sensor_counts(data = data, sensors = c(24, 15), "8 to 2 TL 1", year = 2018, month = 2)  

pdf("8-to-15-tl1.pdf", width = 30, height = 10/(4/3))
print(plot)
dev.off()

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


# heatmap

months_TE_renamed <- months_TE %>% rename(MI = TE) %>% rename(Stat_Sig = Stat_sig)
plot_MI_heatmap(months_TE_renamed, "test.pdf")

colfunc<-colorRampPalette(c("white","yellow","red","black"))

max_tl <- max(months_TE$Time_lag)

max_TE <- as.numeric(max(as.numeric(months_TE$TE)))
min_TE <- as.numeric(min(as.numeric(months_TE$TE)))

months_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  #filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(Time_lag == 1) %>% 
  ggplot(aes(x = Sensor1, y = Sensor2, fill = TE)) +
  geom_tile() +
  labs(x = "Sensor 1", y = "Sensor 2", fill = "MI") +
  scale_fill_gradientn(colours = colfunc(100), limits=c(min_TE, max_TE)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  ggtitle(paste("MI for time lag",tl, sep = " ")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI"))


