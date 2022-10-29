library(seriation)
library(ggdendro)

year_MI <- read.csv2("pedestrians/results/year/year_hourly_MI_TL5.csv", sep = ",")

year_MI %>% 
  mutate_all(type.convert, as.is=TRUE) %>%
  mutate_if(is.character, as.numeric) %>% 
  filter(Sensor1 < Sensor2) 
  
  
  

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
