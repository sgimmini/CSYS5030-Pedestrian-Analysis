


################################################## 
# AIS

months_1921 <- read.csv2("pedestrians/results/years/years1921_hourly_AIS_stat_sig.csv", sep = ",")


  
  
  
################################################## 
# MI

MI_1921 <- read.csv2("pedestrians/results/years/years1921_monthly_hourly_MI_TL5.csv", sep = ",")

MI_1921 %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  filter(Sensor1 < Sensor2) %>% 
  filter(Time_lag == 0) %>% 
  unite(Sensors, Sensor1, Sensor2, sep = "_") -> 
  MI_1921

MI_1921 %>% 
  group_by(Month) %>% 
  arrange(desc(MI)) %>% 
  slice(3) %>% 
  ungroup() %>% 
  select(Sensors) %>% 
  distinct() -> top_sensor_pairs
  
MI_1921 %>% 
  group_by(Month) %>% 
  arrange(MI) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(Sensors) %>% 
  distinct() -> bottom_sensor_pairs


MI_1921 %>% 
  #filter(Sensors %in% bottom_sensor_pairs$Sensors) %>%
  filter(Sensors %in% top_sensor_pairs$Sensors) %>%
  #filter(Sensors %in% bottom_sensor_pairs$Sensors | Sensors %in% top_sensor_pairs$Sensors) %>%
  ggplot() +
    geom_tile(aes(x = Month, y = Sensors, fill = MI)) +
    labs(x = "Month", y = "Sensor pair", fill = "MI") +
    scale_fill_gradientn(colours = colfunc(100)) +#, limits=c(min_MI, max_MI)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
    ggtitle("MI in 1921") +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) #->
    MI_1921_plot
    
MI_1921 %>% 
  #filter(Sensors %in% bottom_sensor_pairs$Sensors) %>%
  filter(Sensors %in% top_sensor_pairs$Sensors) %>%
  #filter(Sensors %in% bottom_sensor_pairs$Sensors | Sensors %in% top_sensor_pairs$Sensors) %>%
  ggplot() +
  geom_line(aes(x = Month, y = MI, color = Sensors)) +
    labs(x = "Month", y = "MI", color = "Sensor pair") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
    ggtitle("MI in 1921") +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5, title = "Sensor pair")) #->
    MI_1921_plot_lines

  