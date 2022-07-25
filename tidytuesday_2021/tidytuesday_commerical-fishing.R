# tidytuesday | July 8, 2021
# Commercial Fishing
# @ntran119

rm(list = ls()) # clear environment
library(tidyverse)

fishing <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')
