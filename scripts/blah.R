# blah blah
# blah
# This is my change

library(tidyverse)
library(janitor)
library(lubridate)

durham <- read_csv("data/OpenAQDurham.csv") %>% clean_names()
sanFran <- read_csv("data/OpenAQSanFran.csv") %>% clean_names()

unique(sanFran$parameter)
sanFran %>% 
  select(location, city, country, utc, local, parameter, value, unit, latitude, longitude) %>%
  filter(parameter == "pm25") %>%
  mutate(month = month(utc),
         year = year(utc),
         day = day(utc)) %>%
  group_by( latitude, longitude) %>%
  summarize(avg_pm25 = mean(value)) -> summarySan
  
  
ggplot(data = summarySan, mapping = aes(x = longitude, y = latitude, color = avg_pm25))+
  geom_point()+
  facet_wrap(~month)+
  geom_sf()

unique(durham$location)
unique(sanFran$location)

time_durham_pm10 <- durham_pm10$utc
month_durham_pm10 <- month(time_durham_pm10)
