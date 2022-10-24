library(tidyverse)
library(lubridate)
library(zoo)

# sensors to remove: 
# 41, 55, >59

data <- read.csv2("pedestrians/Pedestrian_Counting_System_-_Monthly__counts_per_hour_.csv", sep = ",")
data %>% 
  filter(Sensor_ID < 59) %>%  # remove all late sensors, because no datapoints < 2022
  filter(Sensor_ID != 55) %>% # offline after being online, just started before covid
  filter(Sensor_ID != 41) %>% # was offline in 2019 too long to have good data
  mutate(Month = as.integer(factor(Month, levels=month.name))) %>% 
  mutate(Date_Time = make_datetime(Year, Month, Mdate, Time)) %>% 
  mutate(Date = make_date(Year, Month, Mdate)) %>% 
  filter(Date < as.Date("2022-01-01")) %>% # set boundary to end of 2021
  filter(Date > as.Date("2009-12-31")) %>% # set boundary to beginning of 2010
  subset(!(Month == 02 & Mdate == 29)) -> data # remove 29.02.

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
  if (k <= length(x)){
    for (i in (1 + neg_offset):(length(x) - pos_offset)) {
      out[[i]] <- mean(x[(i - neg_offset):(i + pos_offset)])
    }
  }
  out
}

##################################################
# plot each for day
# very unclean overview on daily counts of all sensors
# DONE

data %>% 
  group_by(Sensor_ID, Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts)) %>% 
  # filter(Date < "2010-01-01") %>% 
  ggplot(aes(x = Date, color = factor(Sensor_ID))) +
  geom_line(aes(y = Day_Counts)) +
  scale_color_manual(values = our_colors) + 
  scale_x_date(date_breaks = '1 year', date_labels = "%Y")

##################################################
# sum of all with number of probes
# there is a correlation visible between totalcounts and number of probes
# rollmean for visualization to have a less jacky line
# DONE

data %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  mutate(total = rollmean(Day_Counts, 7) ) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = total, color="totalcounts")) + 
  geom_line(aes(y = total_probes*10000, color="totalprobes")) + 
  scale_y_continuous(sec.axis = sec_axis(~./10000, name="totalprobes"))

# save this in dataframe for analysis
data %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  arrange(Date) %>% 
  select(-Date) %>% 
  write_csv("pedestrians/daycounts_totalprobes.csv")



##################################################
# sum of all divided by number of probes
# it is visible that the total number of pedestrians does not increase over the 
# years, but roughly stays the same but not for covid

data %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  mutate(total = Day_Counts/total_probes) %>% 
  ggplot(aes(x = Date, y = total)) +
  geom_line() 

# sum of all divided by number of probes with rollmean
data %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  mutate(total = rollmean(Day_Counts/total_probes, 7)) %>% 
  ggplot(aes(x = Date, y = total)) +
  geom_line() 

##################################################
# sum of all 
# number of pedestrian counts increase over the years

data %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts)) %>% 
  mutate(total = rollmean(Day_Counts, 7)) %>% 
  ggplot(aes(x = Date, y = total)) +
  geom_line() 


##################################################
# save data for probes
data %>% 
  group_by(Date) %>% 
  summarise(total_probes = length(unique(Sensor_ID)))  ->
  probes_per_day
write_csv(probes_per_day, "pedestrians/probes_per_day.csv")

##################################################
# one week in july
data %>% 
  filter(Year == 2016) %>% 
  filter(Month < 4) %>% 
  # filter(Mdate < 4) %>% 
  # filter(Sensor_ID %in% c(1, 6, 7, 13, 24, 38)) %>% 
  # group_by(Sensor_ID, Date_Time) %>% 
  ggplot(aes(x = Date_Time, color = factor(Sensor_ID))) +
  geom_line(aes(y = Hourly_Counts)) +
  scale_color_manual(values = our_colors)
 
##################################################
# stacked plot of sensors
# does not really work with days or hours

data %>% 
  # filter(Sensor_ID %in% c(7, 11, 12, 32, 35, 57)) %>% 
  # filter(Year == 2019) %>% 
  # filter(Month == 7) %>% 
  filter(Sensor_ID %in% c(7, 57)) %>% 
  group_by(Sensor_ID, Date) %>% 
  summarise(total = sum(Hourly_Counts)) %>% 
  ggplot(aes(x = Date, y = total, fill = factor(Sensor_ID))) +
  geom_area(position = "stack") 
  
##################################################
# all sensors
# DONE
data %>% 
  select(Sensor_Name, Sensor_ID) %>% 
  unique() %>% 
  arrange(Sensor_ID) %>% 
  write_csv("pedestrians/SensorID_SensorName.csv")
  view()

##################################################
# refactor data: datetime (hours) as rows and sensor ids as columns
# DONE

data %>% 
  # filter(Year %in% c(2019, 2020, 2021)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  group_by(Date_Time, Sensor_ID) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = Sensor_ID, values_from = Hourly_Counts) %>% 
  select(-row) %>% 
  arrange(Date_Time) %>% 
  select(Date_Time, str_sort(names(.), numeric=TRUE)) ->
  datetime_sensor_id_refactor

datetime_sensor_id_refactor %>% 
  ungroup() %>% 
  mutate(Year = format(Date_Time, format="%Y")) %>% 
  filter(Year %in% c(2021)) %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(-Date_Time, -Year) %>% 
  write_csv(file="pedestrians/datetime_sensor_id_refactor_2021.csv")


write_csv(datetime_sensor_id_refactor, "pedestrians/datetime_sensor_id_refactor_19-20-21.csv")

##################################################
# refactor data: date (days) as rows and sensor ids as columns

data %>% 
  select(Date, Sensor_ID, Hourly_Counts) %>% 
  group_by(Sensor_ID, Date) %>% 
  summarise(Daily_Counts = sum(Hourly_Counts)) %>% 
  #mutate(row = row_number()) %>% 
  pivot_wider(names_from = Sensor_ID, values_from = Daily_Counts) %>% 
  #select(-row) %>% 
  arrange(Date) %>% 
  select(Date, str_sort(names(.), numeric=TRUE)) ->
  date_sensor_id_refactor

for(i in 2010:2021) {
 date_sensor_id_refactor %>% 
  ungroup() %>% 
  mutate(Year = format(Date, format="%Y")) %>% 
  filter(Year %in% c(i)) %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(-Date, -Year) %>% 
  write_csv(file=paste("pedestrians/daily_date_sensor_id_refactor_",i,".csv", sep=""))
}


##################################################
# save different months, not only years for a finer look
# DONE

months <- c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12)

for(yea in 2010:2021) {
  for(mon in months) {
    data %>% 
      filter(Year %in% yea) %>% 
      filter(Month %in% mon) %>% 
      select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
      group_by(Date_Time, Sensor_ID) %>% 
      mutate(row = row_number()) %>% 
      pivot_wider(names_from = Sensor_ID, values_from = Hourly_Counts) %>% 
      ungroup() %>% 
      select(-row) %>% 
      arrange(Date_Time) %>% 
      select(str_sort(names(.), numeric=TRUE)) %>% 
      select_if(~ !any(is.na(.))) %>% 
      select(-Date_Time) %>% 
      write_csv(file=paste("pedestrians/hourly_months/daily_date_sensor_id_refactor_",mon,"-",yea,".csv", sep=""))
  }
}
data %>% 
  filter(Year == 2018) %>% 
  filter(Month == 01) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  filter(Sensor_ID %in% c(31, 50)) %>% 
  group_by(Date_Time, Sensor_ID) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = Sensor_ID, values_from = Hourly_Counts) %>% 
  ungroup() %>% 
  select(-row) %>% 
  arrange(Date_Time) %>% 
  select(str_sort(names(.), numeric=TRUE)) %>% 
  select_if(~ !any(is.na(.)))


##################################################
# total probes with sum for months
data %>% 
  filter(Month %in% c(11)) %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  mutate(total = rollmean(Day_Counts, 7)/total_probes ) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = total, color="totalcounts")) + 
  geom_line(aes(y = total_probes*100, color="totalprobes")) + 
  scale_y_continuous(sec.axis = sec_axis(~./100, name="totalprobes"))


##################################################
# total per day for each month 
data %>% 
  filter(Year %in% c(2019, 2020, 2021)) %>% 
  group_by(Year, Month, Mdate) %>% 
  summarise(total = sum(Hourly_Counts)) %>% 
  mutate(date = make_date(Month, Mdate)) %>% 
  ggplot(aes(x = date, y = total, color = factor(Year))) +
  geom_line() 

##################################################
# total per day for each month ADJUSTED to probes
# PLOT
# DONE

data %>% 
  select(-c(ID, Day, Sensor_Name)) %>% 
  filter(Year %in% c(2019, 2020, 2021)) %>% 
  filter(Year %!in% c(2010)) %>% 
  #filter(Year %in% c(2019, 2020, 2021)) %>% 
  #subset(!(Month == 02 & Mdate == 29)) %>% 
  group_by(Date_Time) %>% 
  summarise(total = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  mutate(adjusted_total = rollmean(total/total_probes, 24)) %>% 
  mutate(Date_Time2 = format(Date_Time, format="%m-%d-%H")) %>% 
  mutate(Year = format(Date_Time, format="%Y")) %>% 
  mutate(idx = match(Date_Time2, unique(Date_Time2))) %>%  
  select(-Date_Time, -total_probes) %>% 
  ggplot(aes(x = idx, y = adjusted_total, color = factor(Year))) +
  geom_line() 
  # ggplot(aes(x = idx, y = adjusted_total, fill = factor(Year))) +
  # geom_area(position = "stack") 

# DATASET SAVE
data %>% 
  select(-c(ID, Day, Sensor_Name)) %>% 
  filter(Year %in% c(2019, 2020, 2021)) %>% 
  #filter(Year %!in% c(2009, 2010, 2022)) %>% 
  group_by(Date_Time) %>% 
  summarise(total = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  mutate(adjusted_total = total/total_probes) %>% 
  mutate(Date_Time2 = format(Date_Time, format="%m-%d-%H")) %>% 
  mutate(Year = format(Date_Time, format="%Y")) %>% 
  mutate(idx = match(Date_Time2, unique(Date_Time2))) %>%  
  arrange(Date_Time) %>% 
  select(-Date_Time, -total_probes, -total) %>% 
  pivot_wider(names_from = Year, values_from = adjusted_total) %>% 
  #mutate("2013" = na.approx(`2013`)) %>% 
  #mutate("2014" = na.approx(`2014`)) %>% 
  #mutate("2015" = na.approx(`2015`)) %>% 
  #mutate("2016" = na.approx(`2016`)) %>% 
  #mutate("2017" = na.approx(`2017`)) %>% 
  select(-Date_Time2, -idx) %>% 
  #filter(idx %in% c(6555, 6579, 6627, 6651, 6675))
  #"["(rowSums(is.na(.)) > 0,)
  write_csv("pedestrians/hourly_adjusted_per_year_19-to-21.csv")
  
   
  
  
  
  
data %>% 
  filter(Year == 2017, Month == 10, Mdate == 01) %>% 
  group_by(Date_Time) %>% 
  summarise(total = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  view()

  
  
  

##################################################
# total per year
# DONE

data %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts), total_probes = length(unique(Sensor_ID))) %>% 
  mutate(adjusted_total = Day_Counts/total_probes ) %>% 
  ungroup() %>% 
  mutate(Year = year(Date)) %>% 
  filter(Year %!in% c(2009, 2022)) %>% 
  mutate(MonDay = format(Date, format="%m-%d")) %>% 
  select(-c(Date, total_probes, Day_Counts)) %>% 
  pivot_wider(names_from = Year, values_from = adjusted_total) %>% 
  separate(MonDay, into=c("Month", "Day"), sep = "-") %>% 
  arrange(Month, Day) %>%  #-> month_day_years_total
  select(-c(Month, Day)) -> month_day_years_total

write_csv(month_day_years_total, "pedestrians/month_day_years_total.csv")

# remove feb 29
month_day_years_total %>% 
  subset(!(Month == "02" & Day == "29")) %>% 
  select(-c(Month, Day)) ->
  month_day_years_total_wo_feb

write_csv(month_day_years_total_wo_feb, "pedestrians/month_day_years_total_wo_feb.csv")


################################################## 
# plot different sensors on different plots to see the range of different sensors
# DONE

data %>% 
  select(Sensor_Name, Sensor_ID) %>% 
  unique() %>% 
  arrange(Sensor_ID) %>% 
  view()


data %>% 
  filter(Sensor_ID == 1) %>% 
  group_by(Date) %>% 
  summarise(Day_Counts = sum(Hourly_Counts)) %>% 
  arrange(Date) %>% 
  ggplot(aes(x = Date, y = Day_Counts)) + 
  geom_line() + 
  ggtitle(as.character(1)) +
  scale_x_date(limits = as.Date(c('2010-01-01','2021-12-31')))


plot_list = list()
for (id in 1:87) {
  p <- tryCatch({
    data %>% 
      filter(Sensor_ID == id) %>% 
      group_by(Date) %>% 
      summarise(Day_Counts = sum(Hourly_Counts)) %>% 
      arrange(Date) %>% 
      ggplot(aes(x = Date, y = Day_Counts)) +
      geom_line() + 
      ggtitle(as.character(id)) + 
      scale_x_date(limits = as.Date(c('2010-01-01','2021-12-31')))
  })
  tryCatch({
    plot_list[[id]] <- p
  })
}

pdf("plots.pdf")
for (i in 1:length(plot_list)) {
  print(plot_list[[i]])
}
dev.off()
 

################################################## 
# analysis of 2011 showed a big difference in sensors 2 and 7
# 7 has a really unpredictable behavior after July
# DONE?

data %>% 
  # filter(Year == 2011) %>% 
  filter(Sensor_ID %in% c(2, 11)) %>% 
  select(Date, Sensor_ID, Hourly_Counts) %>% 
  group_by(Date, Sensor_ID) %>% 
  summarise(Daily_Counts= sum(Hourly_Counts)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  group_by(Sensor_ID) %>% 
  mutate(total = rollmean(Daily_Counts, 7)) %>% 
  ggplot(aes(x = Date, y = total, color = factor(Sensor_ID))) +
  geom_line()


################################################## 
# data for the 31.12.
# TODO

data %>% 
  filter()


################################################## 
# MI between pairs showed a high MI between 5 and 6, but not for 2015

data %>% 
  filter(Sensor_ID %in% c(5, 6)) %>% 
  filter(Year %in% c(2014, 2015, 2016)) %>% 
  #filter(Year %in% c(2014)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  group_by(Sensor_ID) %>% 
  mutate(Total = rollmean(Hourly_Counts, 24)) %>% 
  ggplot(aes(x = Date_Time, y = Total, color = factor(Sensor_ID))) +
  geom_line()


data %>% 
  filter(Sensor_ID %in% c(9 ,18)) %>% 
  filter(Year %in% c(2013, 2014, 2015, 2016, 2017)) %>% 
  #filter(Year %in% c(2014)) %>% 
  select(Date, Sensor_ID, Hourly_Counts) %>% 
  group_by(Date, Sensor_ID) %>% 
  summarise(Daily_Counts = sum(Hourly_Counts)) %>% 
  group_by(Sensor_ID) %>% 
  mutate(Total = rollmean(Daily_Counts, 7)) %>% 
  ggplot(aes(x = Date, y = Total, color = factor(Sensor_ID))) +
  geom_line()

# 11 and 12 have a high TE
# they have the same values for some time - probably a measure or saving failure
data %>% 
  filter(Sensor_ID %in% c(31, 50)) %>% 
  filter(Year %in% c(2017, 2018)) %>% 
  #filter(Year %in% c(2014)) %>% 
  select(Date, Sensor_ID, Hourly_Counts) %>% 
  group_by(Date, Sensor_ID) %>% 
  summarise(Daily_Counts = sum(Hourly_Counts)) %>% 
  group_by(Sensor_ID) %>% 
  #mutate(Total = rollmean(Daily_Counts, 7)) %>% 
  mutate(Total = Daily_Counts) %>% 
  ggplot(aes(x = Date, y = Total, color = factor(Sensor_ID))) +
  geom_line()


################################################## 
# find a month with many sensors to compare "a typical" week
# -> 15.11.2018 has the most sensors - taking this week
# DONE

data %>% 
  select(Date_Time, Date, Month, Hourly_Counts, Sensor_ID) %>% 
  group_by(Date, Month) %>% 
  mutate(Total_probes = length(unique(Sensor_ID))) %>% 
  arrange(desc(Total_probes))

# plot data for this week
data %>% 
  filter(Year == 2018, Month == 11, Mdate %in% c(12, 13, 14, 15, 16, 17, 18)) %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts, color = factor(Sensor_ID))) + 
  geom_line()

# save data
data %>% 
  filter(Year == 2018, Month == 11, Mdate %in% c(12, 13, 14, 15, 16, 17, 18)) %>% 
  select(Date_Time, Sensor_ID, Hourly_Counts) %>% 
  group_by(Date_Time, Sensor_ID) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = Sensor_ID, values_from = Hourly_Counts) %>% 
  select(-row) %>% 
  select(Date_Time, str_sort(names(.), numeric=TRUE)) %>% 
  arrange(Date_Time) %>% 
  ungroup() %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(-Date_Time) %>% 
  write_csv(file="pedestrians/datetime_sensor_id_week-in_11-2018.csv")
  
# save data for each day
for (day in c(12, 13, 14, 15, 16, 17, 18)) {
  data %>%
    filter(Year == 2018, Month == 11, Mdate == day) %>%
    select(Date_Time, Sensor_ID, Hourly_Counts) %>%
    group_by(Date_Time, Sensor_ID) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = Sensor_ID, values_from = Hourly_Counts) %>%
    select(-row) %>%
    select(Date_Time, str_sort(names(.), numeric = TRUE)) %>%
    arrange(Date_Time) %>%
    ungroup() %>%
    select_if( ~ !any(is.na(.))) %>%
    select(-Date_Time) %>%
    write_csv(file = paste("pedestrians/datetime_sensor_id_",day,"-11-2018.csv", sep=""))
}

