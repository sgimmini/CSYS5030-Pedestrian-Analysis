library(tidyverse)

# a reverse %in%
'%!in%' <- function(x,y)!('%in%'(x,y))


# function for rolling average, which makes plot a bit better to look at
rollmean <- function(x, k) {
  stopifnot("`x` is missing" = !missing(x))
  stopifnot("`k` is missing" = !missing(k))
  stopifnot("`x` must be numeric" = is.numeric(x))
  stopifnot("`k` must be numeric" = is.numeric(k))
  out <- rep(NA_real_, length(x))
  neg_offset <- floor(k / 2)
  pos_offset <- ceiling(k / 2) - 1
  if (k <= length(x)) {
    for (i in (1 + neg_offset):(length(x) - pos_offset)) {
      out[[i]] <- mean(x[(i - neg_offset):(i + pos_offset)])
    }
  }
  out
}

# plot MI heatmap of sensor pairs
plot_MI_heatmap <- function(data, out_filename) {
    
    colfunc<-colorRampPalette(c("white","yellow","red","black"))
    
    max_tl <- max(data$Time_lag)
    
    max_MI <- as.numeric(max(as.numeric(data$MI)))
    min_MI <- as.numeric(min(as.numeric(data$MI)))
    
    plots <- list()
    # add progressbar to for loop
    pb <- txtProgressBar(min = 0, max = max_tl, style = 3)
    for (tl in 0:max_tl) {
      data %>% 
        mutate_all(type.convert, as.is=TRUE) %>% 
        mutate_if(is.character, as.numeric) %>% 
        select(-Stat_Sig) %>% 
        filter(Sensor1 < Sensor2) %>% # remove duplicate sensor pairs (e.g. 1-2 and 2-1)
        filter(Time_lag == tl) %>% 
        ggplot(aes(x = Sensor1, y = Sensor2, fill = MI)) +
        geom_tile() +
        labs(x = "Sensor 1", y = "Sensor 2", fill = "MI") +
        scale_fill_gradientn(colours = colfunc(100), limits=c(min_MI, max_MI)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
        ggtitle(paste("MI for time lag",tl, sep = " ")) +
        guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI")) -> 
        plot
      
      plots[[tl+1]] <- plot
      setTxtProgressBar(pb, tl)
    }
    
    # save plots to pdf
    pdf(out_filename, width = 10, height = 10/(4/3))
    for (i in 1:length(plots)) {
      print(plots[[i]])
    }
    dev.off()
  
}


# fuction to plot the sum of Hourly_Counts for given sensors, year, month and day. Year, month and day are optional 
plot_daily_sensor_counts <- function(data, sensors, title, year = NULL, month = NULL, day = NULL, out_filename = NULL, use_rollmean=F) {
  
  stopifnot("`data` is missing" = !missing(data))
  stopifnot("`sensors` is missing" = !missing(sensors))
  # stopifnot("`sensors` must be numeric" = is.numeric(sensors))
  
  if (!is.null(year)) {
    stopifnot("`year` must be numeric" = is.numeric(year))
    data %>% filter(Year %in% year) -> data
  }
  if (!is.null(month)) {
    stopifnot("`month` must be numeric" = is.numeric(month))
    data %>% filter(Month %in% month) -> data
  }
  if (!is.null(day)) {
    stopifnot("`day` must be numeric" = is.numeric(day))
    data %>% filter(Mdate %in% day) -> data
  }
  
  data %>% 
    filter(Sensor_ID %in% sensors) %>% 
    select(Date, Sensor_ID, Hourly_Counts) %>% 
    group_by(Date, Sensor_ID) %>% 
    summarise(Daily_Counts= sum(Hourly_Counts)) %>% 
    ungroup() %>% 
    arrange(Date) -> 
    data
  
  if (use_rollmean) {
    group_by(Sensor_ID) %>% 
    mutate(total = rollmean(Daily_Counts, 7)) %>% 
    ggplot(aes(x = Date, y = total, color = factor(Sensor_ID))) +
    geom_line() + 
    labs(x = "Date", y = "Daily Counts", color = "Sensor ID") +
    ggtitle(title) ->
    plot
  } else {
    ggplot(data, aes(x = Date, y = Daily_Counts, color = factor(Sensor_ID))) +
    geom_line() + 
    labs(x = "Date", y = "Daily Counts", color = "Sensor ID") +
    ggtitle(title) ->
    plot
  }
  
  # if (!is.null(year)) {
  #   plot <- plot + facet_wrap(~Year)
  # }
  # if (!is.null(month)) {
  #   plot <- plot + facet_wrap(~Month)
  # }
  # if (!is.null(day)) {
  #   plot <- plot + facet_wrap(~Day)
  # }

  # if out_filename is not NULL, save plot to pdf
  if (!is.null(out_filename)) {
    pdf(out_filename, width = 10, height = 10/(4/3))
    print(plot)
    dev.off()
  } else {
    print(plot)
  }
}



plot_hourly_sensor_counts <- function(data, sensors, title, year = NULL, month = NULL, day = NULL, out_filename = NULL) {
  
  stopifnot("`data` is missing" = !missing(data))
  stopifnot("`sensors` is missing" = !missing(sensors))
  
  if (!is.null(year)) {
    stopifnot("`year` must be numeric" = is.numeric(year))
    data %>% filter(Year %in% year) -> data
  }
  if (!is.null(month)) {
    stopifnot("`month` must be numeric" = is.numeric(month))
    data %>% filter(Month %in% month) -> data
  }
  if (!is.null(day)) {
    stopifnot("`day` must be numeric" = is.numeric(day))
    data %>% filter(Mdate %in% day) -> data
  }
  
  data %>%
    filter(Sensor_ID %in% sensors) %>%
    select(Date_Time, Sensor_ID, Hourly_Counts) ->
    data
  
  ggplot(data, aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) +
  geom_line() + 
  labs(x = "Date Time", y = "Hourly Counts", color = "Sensor ID") +
  scale_color_manual(values = our_colors) +
  ggtitle(title) ->
  plot

  # if out_filename is not NULL, save plot to pdf
  if (!is.null(out_filename)) {
    ggsave(out_filename, plot = plot, width = 10, height = 10/(16/9), limitsize = F)
    #pdf(out_filename, width = 10, height = 10/(16/9))
    #print(plot)
    #dev.off()
  } else {
    print(plot)
  }
}



######################################
# funciton to plot scatter from MI values with correlation line

plot_MI_distance_correlation <- function(data, time_lag, title, out_filename = NULL) {
  
  stopifnot("`data` is missing" = !missing(data))
  stopifnot("`time_lag` is missing" = !missing(time_lag))
  
  data %>% 
    mutate_all(type.convert, as.is=TRUE) %>% 
    mutate_if(is.character, as.numeric) %>% 
    select(-Stat_Sig) %>% 
    filter(Time_lag == time_lag) %>% 
    filter(Sensor1 < Sensor2) %>% 
    # combine sensor1 and sensor2 into one column sensors
    mutate(sensors = paste(Sensor1, Sensor2, sep = "_")) %>%
    select(-c(Sensor1, Sensor2, Time_lag, Year, Month, Day)) %>%
    # left join with sensor_distances
    left_join(sensor_distances, by = c("sensors" = "sensors")) %>%
    # remove rows with NA
    na.omit() -> 
    data_distances
  
  data_distances %>% 
    # plot with correlation line
    ggplot(aes(x = distance, y = MI)) +
    #ggplot(aes(x = distance, y = MI, size = Stat_Sig)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    # add correlation value to plot
    annotate("text", x = 0.5, y = 0.5, label = paste("r = ", round(cor(data_distances$distance, data_distances$MI), 2))) +
    labs(x = "Distance in m", y = "MI in nats") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
    ggtitle(title) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "MI"))  ->
    plot
  
  # if out_filename is not NULL, save plot to pdf
  if (!is.null(out_filename)) {
    ggsave(out_filename, plot = plot, width = 10, height = 10/(16/9), limitsize = F)
  } else {
    print(plot)
  }
}

