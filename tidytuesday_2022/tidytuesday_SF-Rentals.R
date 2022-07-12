# tidytuesday | July 5, 2022 
# San Francisco Rentals
# @ntran119

rm(list = ls()) # clear environment

library(tidyverse)

rent <-read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

glimpse(rent) #explore the data

# compare mean price across counties, accounting for sqft
