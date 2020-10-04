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
searching <- sanfran25 %>%
  filter(utc > "2020-08-10 03:00" & utc < "2020-09-10 03:00") %>%
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
  geom_point(mapping = aes(y = latitudeMode, x = peakdates, color = location), size = 4)+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(y='South - North',
       x='Date of Peak PM2.5',
       title = 'Was there smoke coming from the North?')

#testing west theory----
fun %>%
  full_join(longLocation) -> orderOfMonitors2

ggplot(data = orderOfMonitors2)+
  geom_point(size = 4, mapping = aes(x = longitudeMode, y = peakdates,color = location))+
  labs(x='West-East',
       y='Date of Peak PM2.5',
       title = 'Was there smoke coming from the West?')

#map gif libraries----
library("rnaturalearth")
library("rnaturalearthdata")
library("RColorBrewer")

#adding fixed color Scale----
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 6))

#fire coordinate for ref----
fire <- data.frame(latitude = c(39.659), longitude = c(-122.809))

#reading in county map----
caCounties <- st_read('data/CACounty/geo_export_855ac6f6-d8fb-4ea4-956d-ab7edbd84204.shp')


#mannually itterated to make gif----
sanfran25%>%
  filter(utc > "2020-08-16 03:00" & utc < "2020-08-29 03:00")%>%
  mutate(day = day(utc),
         logValue = log(abs(value))) %>%
  filter(day == "28")->searchingMap #heres where I changed the day

ggplot()+
  geom_sf(data = caCounties)+
  geom_point(data = searchingMap, mapping = aes(x = longitude, y = latitude, color = logValue), size = 4)+
  labs(title = '08/28/20') #also changed here
#+facet_wrap(~day)
#geom_point(data = fire, mapping = aes(x = longitude, y = latitude), color = 'red', size = 5)

