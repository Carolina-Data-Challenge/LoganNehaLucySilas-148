#loading packages
library(tidyverse)
library(janitor)
library(lubridate)
install.packages('ggforce')
library(ggforce)
library(sf)

#reading in csvs----
durham <- read_csv("data/OpenAQDurham.csv") %>% clean_names()
sanFran <- read_csv("data/OpenAQSanFran.csv") %>% clean_names()

#exploring pollutants by stations in SF----
ggplot(data= sanFran, mapping = aes(x = utc, y = value, color = location))+
  geom_smooth(se = F)+
  facet_wrap(~parameter, scale = 'free_y')+
  labs(title = 'Investigating Pollutants',
       x = 'Date',
       y = 'Concentration (unit varies)')

#filtering only pm2.5----
sanfran25 <- sanFran %>%
  filter(parameter == "pm25")

#overall graphing pm25----
ggplot(data = sanfran25, mapping = aes(x = utc, y = value))+
  geom_point()+
  geom_smooth(span = .4)+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  labs(x = 'Date',
       y= 'particulate matter (µg/m³)',
       title = 'PM2.5 Monitors in San Francisco during August Complex Fire')+
  facet_wrap(~location) #makes panels

#technique 1 finding peak dates----
ggplot(data = sanfran25, mapping = aes(x = utc, y = value, color = location))+
  geom_smooth(span = .4, se = F)

#finding peak dates----
searching <- smallTime %>%
  filter(location == 'Redwood City') #switched the location to find peak

ggplot(data = searching, mapping = aes(x = utc, y = value))+
  geom_point()+
  geom_smooth(span = .4)+
  scale_x_datetime(date_breaks = '1 days')+
  theme(axis.text.x = element_text(angle=90, hjust=1))

#manual dataframe of resulting analysis----
peakdates <- c('2020-08-23 23:00:00','2020-08-23 12:00:00','2020-08-23 01:00:00','2020-08-21 12:00:00','2020-08-22 12:00:00','2020-08-21 23:00:00','2020-08-21 23:00:00','2020-08-21 23:00:00','2020-08-22 12:00:00')
orderedMonitors <- c('San Rafael','San Pablo - Rumrill','Concord','Oakland West','Laney College','San Francisco','Oakland','Pleasanton - Owens C','Livermore - Rincon')

fun <- data.frame(peakdates = peakdates,
                  location = orderedMonitors)

#getting latitudes of locations----
sanfran25 %>%
  select(location, latitude) %>%
  group_by(location)%>%
  summarize(latitudeMode = median(latitude)) -> latLocation
sanfran25 %>%
  select(location, longitude) %>%
  group_by(location)%>%
  summarize(longitudeMode = median(longitude)) -> longLocation


#testing north theory-----
fun%>%
  full_join(latLocation) -> orderOfMonitors

ggplot(data = orderOfMonitors)+
  geom_point(mapping = aes(x = latitudeMode, y = peakdates))

#testing west theory----
fun %>%
  full_join(longLocation) -> orderOfMonitors2

ggplot(data = orderOfMonitors2)+
  geom_point(mapping = aes(x = longitudeMode, y = peakdates))
