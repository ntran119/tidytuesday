# TidyTuesday 2023-04-18
# Neolithic Founder Crops

# import libraries
library(dplyr)
library(readr)

# load data
tuesdata <- tidytuesdayR::tt_load('2023-04-18')
founder_crops <- tuesdata$founder_crops

# explore
dim(founder_crops) # 4490 rows, 24 columns
glimpse(founder_crops)

# find NAs per column
colSums(is.na(founder_crops))

# count the amount of unique values in each column
sapply(founder_crops , function(x) n_distinct(x))

# import map data
world_coordinates <- map_data("world")

# find min/max long/lat for crops
founder_crops$latitude %>% max() # 39.85
founder_crops$latitude %>% min() # 29.84

founder_crops$longitude %>% max() # 48.5
founder_crops$longitude %>% min() # 30.07

# filter values from world coordinates
middle_east <- world_coordinates %>% 
  filter(lat <= 39.85 & lat >= 29.84,
         long <= 48.5 & long >= 30.07)

# render the map with points for crops
ggplot() + 
  
  geom_map(
  data = middle_east, 
  map = world_coordinates,
  aes(long, lat, map_id = region),
  color = "black", fill= "lightyellow", size = 0.2) +

  geom_point(
    data = founder_crops %>% filter(!is.na(founder_crop)),
    aes(longitude, latitude, color = founder_crop),
    alpha = 1) +
  labs(title = 'Cultivation of Founder Crops',
       subtitle = 'In Southwest Asia',
       x = NULL,
       y = NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())
