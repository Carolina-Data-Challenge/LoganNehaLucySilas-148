# blah blah
# blah
# This is my change

library(tidyverse)
library(janitor)

durham <- read_csv("data/OpenAQDurham.csv") %>% clean_names()
sanFran <- read_csv("data/OpenAQSanFran.csv") %>% clean_names()
