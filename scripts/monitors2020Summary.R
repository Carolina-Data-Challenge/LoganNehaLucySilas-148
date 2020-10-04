library(tidyverse)
library(janitor)
library(lubridate)

#monitors 2020 summary----
monitors <- read_csv("data/annual_conc_by_monitor_2020.csv") %>% clean_names()
unique(monitors$parameter_name) #~ 300 variables measured!

#get 2.5 measurements only()\
monitors %>%
  filter(parameter_name == "PM2.5 - Local Conditions") -> monitors25

ggplot(data = monitors)+
  geom_histogram(mapping = aes(x = parameter_name), stat = 'count')

monitors$parameter_name <- factor(monitors$parameter_name)
monitors$parameter_name <- fct_infreq(monitors$parameter_name)
monitors$parameter_name #most common: PM2.5 - Local Conditions Ozone Sulfur dioxide PM10 Total 0-10um STP

#filtering for top 4 counted pollutants
monitors %>%
  filter(parameter_name %in% c("PM2.5 - Local Conditions", "Ozone", "Sulfur dioxide","PM10 Total 0-10um STP")) ->monitor_common

#nc daily (showing change in pm due to covid?)----
dailyNC2020 <- read_csv("data/ad_viz_plotval_data.csv") %>% clean_names()
str(dailyNC2020)
summary(dailyNC2020)
dailyNC2020 %>%
  mutate(date = mdy(date)) -> dailyNC2020
colnames(dailyNC2020)
ggplot(data = dailyNC2020, mapping = aes(x = date, y = daily_mean_pm2_5_concentration))+
  geom_point()

#loading in all the csvs----

m2001 <- read_csv("data/annualMonitors/annual_aqi_by_county_2001.csv") %>% clean_names()
m2002 <- read_csv("data/annualMonitors/annual_aqi_by_county_2002.csv") %>% clean_names()
m2003 <- read_csv("data/annualMonitors/annual_aqi_by_county_2003.csv") %>% clean_names()
m2004 <- read_csv("data/annualMonitors/annual_aqi_by_county_2004.csv") %>% clean_names()
m2005 <- read_csv("data/annualMonitors/annual_aqi_by_county_2005.csv") %>% clean_names()
m2006 <- read_csv("data/annualMonitors/annual_aqi_by_county_2006.csv") %>% clean_names()
m2007 <- read_csv("data/annualMonitors/annual_aqi_by_county_2007.csv") %>% clean_names()
m2008 <- read_csv("data/annualMonitors/annual_aqi_by_county_2008.csv") %>% clean_names()
m2009 <- read_csv("data/annualMonitors/annual_aqi_by_county_2009.csv") %>% clean_names()
m2010 <- read_csv("data/annualMonitors/annual_aqi_by_county_2010.csv") %>% clean_names()
m2011 <- read_csv("data/annualMonitors/annual_aqi_by_county_2011.csv") %>% clean_names()
m2012 <- read_csv("data/annualMonitors/annual_aqi_by_county_2012.csv") %>% clean_names()
m2013 <- read_csv("data/annualMonitors/annual_aqi_by_county_2013.csv") %>% clean_names()
m2014 <- read_csv("data/annualMonitors/annual_aqi_by_county_2014.csv") %>% clean_names()
m2015 <- read_csv("data/annualMonitors/annual_aqi_by_county_2015.csv") %>% clean_names()
m2016 <- read_csv("data/annualMonitors/annual_aqi_by_county_2016.csv") %>% clean_names()
m2017 <- read_csv("data/annualMonitors/annual_aqi_by_county_2017.csv") %>% clean_names()
m2018 <- read_csv("data/annualMonitors/annual_aqi_by_county_2018.csv") %>% clean_names()
m2019 <- read_csv("data/annualMonitors/annual_aqi_by_county_2019.csv") %>% clean_names()

m2001_2019 <- rbind(m2001, #extending all the tables into one
                    m2002,
                    m2003,
                    m2004,
                    m2005,
                    m2006,
                    m2007,
                    m2008,
                    m2009,
                    m2010,
                    m2011,
                    m2012,
                    m2013,
                    m2014,
                    m2015,
                    m2016,
                    m2017,
                    m2018,
                    m2019)
#gets a metric for the aqi of each county based on data from 2001-2019
m2001_2019 %>%
  group_by(county, state)%>%
  summarize(average_median_aqi = mean(median_aqi), daysWithAQI = sum(days_with_aqi)) -> countyAQI

#reading in covid data----
covid <- read_csv("data/us-counties.csv") %>% clean_names()
covid%>%
  group_by(county,state)%>%
  summarize(maxCases = max(cases),
            maxDeaths = max(deaths))->covid
#joining covid data with counties----
covid%>%
  full_join(countyAQI) %>%
  mutate(ratio = maxDeaths/maxCases)->AQICOVID

ggplot(data = AQICOVID, mapping = aes(x = average_median_aqi, y = ratio))+
  geom_point()+
  scale_y_continuous(limits = c(0,.25))

#redoing to include only nc-----
covid%>%
  full_join(countyAQI) %>%
  mutate(ratio = maxDeaths/maxCases) %>%
  filter(state == "North Carolina")->NCCOVIDRATIO

ggplot(data = NCCOVIDRATIO, mapping = aes(x = average_median_aqi, y = ratio))+
  geom_point()+
  scale_y_continuous(limits = c(0,.25))
