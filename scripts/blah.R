
#loading packages
library(tidyverse)
library(janitor)
library(lubridate)

#reading in csvs
durham <- read_csv("data/OpenAQDurham.csv") %>% clean_names()
sanFran <- read_csv("data/OpenAQSanFran.csv") %>% clean_names()
sanClimate <- read_csv('data/2303976.csv')
#summarizing sanfran averages-----
unique(sanFran$parameter)
sanFran %>% 
  select(location, city, country, utc, local, parameter, value, unit, latitude, longitude) %>%
  filter(parameter == "pm25") %>%
  mutate(month = month(utc),
         year = year(utc),
         day = day(utc)) %>%
  group_by( latitude, longitude) %>%
  summarize(avg_pm25 = mean(value)) -> summarySan

#looking at pm25 only------
durham25 <- durham %>%
  filter(parameter == 'pm25')
sanfran25 <- sanFran %>%
  filter(parameter == "pm25")

ggplot(data = sanfran25, mapping = aes(x = utc, y = value))+
  geom_point()+
  geom_smooth(span = .1)+
  facet_wrap(~location)

#showing durham and sanfran pm25 time correlation-----
ggplot(data = durham25, mapping = aes(x = utc, y = value))+
  geom_point()+
  geom_smooth(span = .1)+
  facet_wrap(~location)
  
ggplot(data = summarySan, mapping = aes(x = longitude, y = latitude, color = avg_pm25))+
  geom_point()+
  geom_smooth(span = .2)+
  facet_wrap(~month)+
  geom_sf()

##looking at Livermore-Rincon for all parameters
sanFranLiver <- sanFran %>%
  filter(location == "Livermore-Rincon")

ggplot(data = sanFranLiver, mapping = aes(x = utc, y = value))+
  geom_point()+
  geom_smooth(span = .1)+
  facet_wrap(~parameter)

