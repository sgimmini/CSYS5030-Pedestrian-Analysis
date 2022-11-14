

################################################## 
# TE

months_TE <- read.csv2("pedestrians/results/months/months_hourly_TE_TL5.csv", sep = ",")


months_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  # filter(Sensor2 == 28) %>% 
  # filter(Time_lag == 1) %>% 
  arrange(desc(TE)) %>% 
  head(100) %>% 
  # summarize the number of occurences  
  count(Month) #%>% 
  write.csv2(file="pedestrians/TE_month_counter.csv", row.names = F)
 
# for 8 -> 15 look the other way around 
months_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  # filter(Sensor2 == 8) %>% 
  # filter(Sensor1 == 15) %>% 
  filter(Time_lag == 1) %>% 
  arrange(desc(TE)) %>% 
  select(Sensor1, Sensor2, TE) %>% 
  head(10) #%>% 
  write.csv2(file="pedestrians/TE_top_10.csv", row.names = F)
 


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



################################################## 
# MI 

months_MI <- read.csv2("pedestrians/results/months/months_hourly_MI_TL5.csv", sep = ",")


months_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(Time_lag == 0) %>% 
  mutate(Day = 1) %>% 
  unite(Date, Year, Month, Day, sep = "-") %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, Sensor1, Sensor2, MI) ->
  months_MI


 
################################################## 
# highest difference in months

# for each sensor pair find the month with the highest difference in MI
months_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Day = 1) %>% 
  unite(Date, Year, Month, Day, sep = "-") %>% 
  filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(Time_lag == 0) %>% 
  unite(Sensors, Sensor1, Sensor2, sep = "_") %>%
  group_by(Sensors) %>%
  summarise(max_MI = max(MI), max_MI_date = Date[which.max(MI)], min_MI = min(MI), min_MI_date = Date[which.min(MI)]) %>%
  mutate(difference = max_MI - min_MI) %>%
  # transform dates to dates
  mutate(max_MI_date = as.Date(max_MI_date), min_MI_date = as.Date(min_MI_date)) %>%
  # get the month from Date for max_MI_date and min_MI_date
  mutate(max_MI_month = format(max_MI_date, "%m"), min_MI_month = format(min_MI_date, "%m")) %>%
  mutate(min_max_MI_date = paste(min_MI_month, max_MI_month, sep = " - ")) %>%
  ungroup() %>%
  select(-c(max_MI_date, min_MI_date)) %>% 
  arrange(desc(difference)) %>% 
  top_n(100, difference)  %>% 
  select(Sensors, min_MI_month, min_MI, max_MI_month, max_MI, difference) %>% 
  slice(1:10)# %>% 
  write.csv2("pedestrians/min_max_MI_top10.csv", row.names = F)

#%>% 
  separate(Sensors, into = c("Sensor1", "Sensor2")) %>% 
  count(Sensor2) %>% 
  arrange(desc(n))


#%>%
  #top_n(100, difference) %>% 
  count(min_MI_month, max_MI_month) %>% 
  filter(n > 9) %>% 
  arrange(desc(n)) %>%
  write.csv2("pedestrians/months_MI_max_diff.csv", row.names = FALSE)

# we found 1 and 4 have the highest difference from 7 to 4 so we want to compare their curves
data %>% 
  filter(Year == 2018, Month %in% c(4, 7), Sensor_ID %in% c(1, 4)) %>%
  ggplot() +
  geom_line(aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) +
  labs(title = "Hourly counts for sensors 1 and 4 in April and July 2018", x = "Date", y = "Hourly counts", color = "Sensor") +
  scale_color_manual(values = our_colors) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  # plot four lines in one plot
  facet_wrap(~Month, ncol = 1, scales = "free") +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(legend.position = "bottom") -> plot

# save in wide format
ggsave("sensor1-4_apr-jul18_MI.png", plot=plot, width = 10, height = 10, units = "in", dpi = 300, limitsize = FALSE)


data %>% 
  filter(Year == 2018, Month %in% c(4, 7), Sensor_ID %in% c(4)) %>%
  #mutate(counter = paste(Mdate, Time, sep = "-")) %>% 
  select(Date_Time) %>% 
  arrange(Date_Time) %>% 
  mutate(row = row_number()) -> april_july_rows


data %>% 
  filter(Year == 2018, Month %in% c(4, 7), Sensor_ID %in% c(1, 4)) %>%
  right_join(april_july_rows, by = "Date_Time") %>% 
  ggplot() +
  geom_line(aes(x = row, y = Hourly_Counts, color = factor(Sensor_ID)))

################################################## 
# heatmap stuff
max_MI <- as.numeric(max(as.numeric(months_MI$MI)))
min_MI <- as.numeric(min(as.numeric(months_MI$MI)))

# heatmap for 2018 
months_MI %>% 
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
  ggtitle(paste("MI for time lag", 0, sep = " ")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) 
 

# use above code in a for loop over all months and save to pdf
allmonths <- unique(months_MI$Month)

plots <- list()
for (m in allmonths) {
  months_MI %>% 
    mutate_all(type.convert, as.is=TRUE) %>% 
    mutate_if(is.character, as.numeric) %>% 
    #filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
    filter(Time_lag == 0) %>% 
    filter(Month == m) %>% 
    ggplot(aes(x = Sensor1, y = Sensor2, fill = MI)) +
    geom_tile() +
    labs(x = "Sensor 1", y = "Sensor 2", fill = "MI") +
    scale_fill_gradientn(colours = colfunc(100), limits=c(min_MI, max_MI)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
    ggtitle(paste("MI for time lag 0 for month", m, sep = " ")) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) -> plot

  plots[[m]] <- plot
}

pdf("MI_monthly_heatmap.pdf", width = 11, height = 8.5)
for (i in 1:length(plots)) {
  print(plots[[i]])
}
dev.off()

# FROM HERE
# try clustering for the 213 time
months_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>% 
  mutate_if(is.character, as.numeric) %>% 
  #filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
  filter(Time_lag == 0) %>% 
  filter(Month == 1) %>% 
  select(-c(Year, Month, Day, Stat_Sig, Time_lag)) %>% 
  # create matrix with Sensor1 on x axis and Sensor2 on y axis, and MI as value
  pivot_wider(names_from = Sensor1, values_from = MI) %>%
  arrange(Sensor2) %>% 
  replace(is.na(.), 0) -> MI_matrix_wSensor2 

MI_matrix_wSensor2 %>% 
  select(-Sensor2) %>%
  as.matrix() -> MI_matrix

row.names(MI_matrix) <- MI_matrix_wSensor2$Sensor2

# cluster with pheatmap and colfunc as color palette
pheatmap(MI_matrix, color = colfunc(100), cluster_rows = TRUE, cluster_cols = TRUE)


# use above code in for loop over all months
allmonths <- unique(months_MI$Month)

plots <- list()
for (m in allmonths) {
  months_MI %>% 
    mutate_all(type.convert, as.is=TRUE) %>% 
    mutate_if(is.character, as.numeric) %>% 
    #filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
    filter(Time_lag == 0) %>% 
    filter(Month == m) %>% 
    select(-c(Year, Month, Day, Stat_Sig, Time_lag)) %>% 
    # create matrix with Sensor1 on x axis and Sensor2 on y axis, and MI as value
    pivot_wider(names_from = Sensor1, values_from = MI) %>%
    arrange(Sensor2) %>% 
    replace(is.na(.), 0) -> MI_matrix_wSensor2 

  MI_matrix_wSensor2 %>% 
    select(-Sensor2) %>%
    as.matrix() -> MI_matrix

  row.names(MI_matrix) <- MI_matrix_wSensor2$Sensor2

  # cluster with pheatmap and colfunc as color palette
  plot <- pheatmap(MI_matrix, color = colfunc(100), cluster_rows = TRUE, cluster_cols = TRUE) 

  # save plot to png with ggsave
  ggsave(paste("MI_monthly_cluster_", m, ".png", sep = ""), plot = plot, width = 11, height = 8.5)

}

