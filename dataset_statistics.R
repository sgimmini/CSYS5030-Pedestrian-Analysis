

# get the total number of rows in the dataset
data %>% 
  nrow()

# get the total number of unique sensors in Sensor_ID
data %>% 
  distinct(Sensor_ID) %>% 
  nrow()

# get the total number of Days 
data %>% 
  distinct(Date) %>% 
  nrow()

# get the total number of Hours
data %>% 
  distinct(Date_Time) %>% 
  nrow()

# which hours are missing in Date_Time from "01.01.2010" to "31.12.2021"
# array of all hours in Date_Time format from "01.01.2010" to "31.12.2021"
hours <- seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2021-12-31 23:00:00"), by = "hour")

data %>% 
  distinct(Date_Time) %>% 
  filter(!Date_Time %in% hours) %>% 
  arrange(Date_Time)

data %>% 
  filter(Date == "2021-12-31") %>% 
  distinct(Time) %>% 
  nrow()


# used for presentation:

data %>% 
  filter(Year == 2018) %>% 
  distinct(Date_Time) %>% 
  nrow()





