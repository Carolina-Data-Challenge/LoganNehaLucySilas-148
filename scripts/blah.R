# blah blah
# blah
# This is my change

library(tidyverse)
library(janitor)

durham <- read_csv("data/OpenAQDurham.csv") %>% clean_names()
sanFran <- read_csv("data/OpenAQSanFran.csv") %>% clean_names()

durham_pm10 <- durham %>% 
  select(location, city, country, utc, local, parameter, value, unit) %>%
  filter(parameter == "pm10")

time_durham_pm10 <- durham_pm10$utc
month_durham_pm10 <- month(time_durham_pm10)
