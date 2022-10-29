library(tidyverse)


################################################## 
# MI

november_MI <- read.csv2("/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/results/month/month_hourly_MI_TL10.csv", sep = ",")
november_MI <- read.csv2("pedestrians/results/month/month_hourly_MI_TL24.csv", sep = ",")


november_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  select(-c(Stat_Sig, Year, Month, Day)) %>% 
  filter(Sensor1 < Sensor2) 


colfunc<-colorRampPalette(c("white","yellow","red","black"))

max_tl <- max(november_MI$Time_lag)

max_MI <- as.numeric(max(as.numeric(november_MI$MI)))
min_MI <- as.numeric(min(as.numeric(november_MI$MI)))

tl <- 2

plots <- list()
for (tl in 0:max_tl) {
  november_MI %>% 
    mutate_all(type.convert, as.is=TRUE) %>% 
    mutate_if(is.character, as.numeric) %>% 
    select(-Stat_Sig) %>% 
    #filter(Sensor1 < 10) %>% 
    #filter(Sensor2 < 10) %>% 
    filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
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
    ggtitle(paste("MI in November for time lag",tl, sep = " ")) +
    # add legend
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) -> 
    plot
  
  plots[[tl+1]] <- plot
}

# save plots to individual png files
for (i in 1:length(plots)) {
  ggsave(paste("MI_plot_", i-1, ".png", sep = ""), plots[[i]], width = 10, height = 10/(16/9))
}



# save plots to pdf
pdf("month_MI_heatmap_dyn24.pdf", width = 10, height = 10/(4/3))
for (i in 1:length(plots)) {
  print(plots[[i]])
}
dev.off()


plot_MI_heatmap(november_MI, "m")


################################################## 
# AIS
november_AIS <- read.csv2("pedestrians/results/month/month_hourly_AIS_stat_sig.csv", sep = ",")

november_AIS %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  # plot with Sensor on x-axis, AIS on y-axis and point size depending on Stat_Sig
    ggplot(aes(x = Sensor, y = AIS, size = Stat_Sig)) +
    geom_point() +
    labs(x = "Sensor", y = "AIS", size = "Stat_Sig") +
    # have a tick for all values
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
    # add title
    ggtitle("AIS for all sensors in November 2018") +
    # add legend
    guides(size = guide_legend(title = "Stat_Sig")) 

  
  
################################################## 
# TE

november_TE <- read.csv2("pedestrians/results/month/month_hourly_TE_________.csv", sep = ",")

november_TE %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 

  
  

