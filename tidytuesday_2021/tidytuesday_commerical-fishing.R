# tidytuesday | July 8, 2021
# Commercial Fishing
# @ntran119

rm(list = ls()) # clear environment
library(tidyverse)

fishing <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')

#### Explore Data ####

glimpse(fishing)
glimpse(stocked)

range(fishing$year) # 1867-2015
range(stocked$YEAR) # 1950-2018

n_distinct(fishing$year) # 149
n_distinct(stocked$YEAR) # 67

colSums(is.na(fishing))
colSums(is.na(stocked))

