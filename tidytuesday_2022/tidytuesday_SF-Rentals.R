# tidytuesday | July 5, 2022 
# San Francisco Rentals
# @ntran119

rm(list = ls()) # clear environment

library(tidyverse)

rent <-read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

glimpse(rent) #explore the data

range(rent$year) # 2008-2018
n_distinct(rent$year) # 11 years
length(unique(rent$county)) # 11 unique counties
colSums(is.na(rent)) # counts NAs in each column

## QUESTIONS TO EXPLORE ##
# 1. How do rent prices change over years
# 2. Difference in rent prices from different counties
# 3. Predict rent price based on sqft
## QUESTIONS TO EXPLORE ##


# 1. How do rent prices change over years

q1df <- rent %>%
  select(year, county, price) %>%
  drop_na()

q1df %>%
  group_by(year) %>%
  summarize(mean_price = mean(price)) %>%
  ggplot(aes(year, mean_price)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
